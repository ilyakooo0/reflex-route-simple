{-# LANGUAGE CPP #-}

module Reflex.Dom.Route.Simple
  ( runRouteT,
    runRouteT',
    runRouteTEv,
    runRouteTEv',
    RouteT,
    MonadRoute (..),
    (://) (..),
    (:#),
    Routable,
    Generic,
    Proxy (..),
    ToHttpApiData (..),
    FromHttpApiData (..),
  )
where

import Control.Monad.Reader
import Data.Foldable
import qualified Data.List as L
import Data.Proxy
import Data.Semigroup (Last (Last))
import qualified Data.Semigroup as S
import qualified Data.Text as T
import GHC.Generics
import qualified GHCJS.DOM.Types as DOM
import Language.Javascript.JSaddle (MonadJSM, jsNull)
import Network.URI
import Reflex.Dom
import Reflex.Dom.Route.Simple.Internal
import Reflex.Network
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))

type Routable r =
  ( ParseRouteG (Rep (r InternalRouteValue)),
    PrintRouteG (Rep (r InternalRouteValue)),
    Defined (Rep (r RouteValue)) (NoGeneric r)
  )

runRouteT ::
  ( Routable r,
    MonadJSM m,
    TriggerEvent t m,
    PerformEvent t m,
    MonadHold t m,
    MonadJSM (Performable m),
    NotReady t m,
    Adjustable t m,
    PostBuild t m,
    MonadFix m,
    (forall x. Generic (r x))
  ) =>
  (Maybe (r RouteValue) -> RouteT t r m a) ->
  m (Event t a)
runRouteT = runRouteTEv never
{-# INLINE runRouteT #-}

runRouteT' ::
  ( Routable r,
    MonadJSM m,
    TriggerEvent t m,
    PerformEvent t m,
    MonadHold t m,
    MonadJSM (Performable m),
    NotReady t m,
    Adjustable t m,
    PostBuild t m,
    MonadFix m,
    (forall x. Generic (r x))
  ) =>
  (Maybe (r RouteValue) -> RouteT t r m a) ->
  m ()
runRouteT' = void . runRouteT
{-# INLINE runRouteT' #-}

runRouteTEv' ::
  ( Routable r,
    MonadJSM m,
    TriggerEvent t m,
    PerformEvent t m,
    MonadHold t m,
    MonadJSM (Performable m),
    NotReady t m,
    Adjustable t m,
    PostBuild t m,
    MonadFix m,
    (forall x. Generic (r x))
  ) =>
  Event t (r RouteValue) ->
  (Maybe (r RouteValue) -> RouteT t r m a) ->
  m ()
runRouteTEv' ev = void . runRouteTEv ev
{-# INLINE runRouteTEv' #-}

runRouteTEv ::
  ( Routable r,
    MonadJSM m,
    TriggerEvent t m,
    PerformEvent t m,
    MonadHold t m,
    MonadJSM (Performable m),
    NotReady t m,
    Adjustable t m,
    PostBuild t m,
    MonadFix m,
    (forall x. Generic (r x))
  ) =>
  Event t (r RouteValue) ->
  (Maybe (r RouteValue) -> RouteT t r m a) ->
  m (Event t a)
runRouteTEv ev f = mdo
  currentHistoryItemDyn <- manageHistory updateHistoryCommandEv
  let currentPage = do
        currentHistoryItem <- currentHistoryItemDyn
        let fragments = case uriFragment . _historyItem_uri $ currentHistoryItem of
              ('#' : rest) -> fmap (T.pack . unEscapeString) . splitOnSlashes $ rest
              _ -> []
        pure $ f $ parseRoutes fragments

  (a, rerouteEv) <- runEventWriterT . unRouteT $ networkView currentPage
  let updateHistoryCommandEv =
        leftmost [Last <$> ev, rerouteEv] & push \(S.Last route) -> do
          let newFragment =
                fold . L.intersperse "/" $
                  escapeURIString isUnescapedInURIComponent <$> printRouteValue route
          lastURI <- fmap _historyItem_uri $ sample $ current currentHistoryItemDyn
          pure $
            Just $
              HistoryCommand_PushState $
                HistoryStateUpdate
                  { _historyStateUpdate_state = DOM.SerializedScriptValue jsNull,
                    _historyStateUpdate_title = "",
                    _historyStateUpdate_uri =
                      Just lastURI {uriFragment = '#' : newFragment}
                  }
  pure a
{-# INLINE runRouteTEv #-}

splitOnSlashes :: String -> [String]
splitOnSlashes s = case breakStringOnSlash s of
  Just (l, s') -> cons (l, splitOnSlashes s')
  Nothing -> [s]
  where
    cons ~(h, t) = h : t

    breakStringOnSlash :: String -> Maybe (String, String)
    breakStringOnSlash "" = Nothing
    breakStringOnSlash ('/' : xs) = Just ([], xs)
    breakStringOnSlash (c : xs) = (\(ys, zs) -> (c : ys, zs)) <$> breakStringOnSlash xs
