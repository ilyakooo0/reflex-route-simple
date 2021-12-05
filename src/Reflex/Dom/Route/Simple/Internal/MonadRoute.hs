{-# LANGUAGE CPP #-}

module Reflex.Dom.Route.Simple.Internal.MonadRoute
  ( MonadRoute (..),
    RouteT (..),
  )
where

import Control.Monad.Reader
import Control.Monad.Ref
import Data.Coerce
import Data.Functor
import Data.Kind
import qualified Data.Semigroup as S
import Language.Javascript.JSaddle (MonadJSM)
import Reflex.Dom
import Reflex.Dom.Route.Simple.Internal.Common
import Reflex.Host.Class

class MonadRoute r t m | m -> r t where
  reroute :: r RouteValue -> m ()
  default reroute :: (Monad m', MonadTrans h, MonadRoute r t m', m ~ h m') => r RouteValue -> m ()
  reroute f = lift $ reroute f
  {-# INLINE reroute #-}

  rerouteEvent :: Event t (r RouteValue) -> m ()
  default rerouteEvent :: (Monad m', MonadTrans h, MonadRoute r t m', m ~ h m') => Event t (r RouteValue) -> m ()
  rerouteEvent ev = lift $ rerouteEvent ev
  {-# INLINE rerouteEvent #-}

newtype RouteT t (r :: Type -> Type) m a = RouteT {unRouteT :: EventWriterT t (S.Last (r RouteValue)) m a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadFix,
      MonadIO,
      NotReady t,
      MonadHold t,
      MonadSample t,
      PostBuild t,
      TriggerEvent t,
      MonadReflexCreateTrigger t,
      HasDocument,
      DomRenderHook t
    )

instance (MonadJSM m, PostBuild t m) => MonadRoute r t (RouteT t r m) where
  reroute r = RouteT $ do
    pb <- getPostBuild
    tellEvent $ pb $> S.Last r
  rerouteEvent ev = RouteT $ tellEvent $ S.Last <$> ev

instance MonadTrans (RouteT t r) where
  lift = RouteT . lift

instance (MonadFix m, MonadHold t m, DomBuilder t m) => DomBuilder t (RouteT t r m) where
  type DomBuilderSpace (RouteT t r m) = DomBuilderSpace m
  element t cfg child = RouteT $ element t cfg $ unRouteT child
  inputElement = lift . inputElement
  textAreaElement = lift . textAreaElement
  selectElement cfg child = RouteT $ selectElement cfg $ unRouteT child

instance HasJSContext m => HasJSContext (RouteT t r m) where
  type JSContextPhantom (RouteT t r m) = JSContextPhantom m
  askJSContext = lift askJSContext

instance (Monad m, MonadRoute r t m) => MonadRoute r t (ReaderT r' m)

instance Requester t m => Requester t (RouteT t r m) where
  type Request (RouteT t r m) = Request m
  type Response (RouteT t r m) = Response m
  requesting = RouteT . requesting
  requesting_ = RouteT . requesting_

instance (Monad m, MonadRoute r t m) => MonadRoute r t (RequesterT t req rsp m)

#ifndef ghcjs_HOST_OS
instance MonadJSM m => MonadJSM (RouteT t r m)
#endif

instance PerformEvent t m => PerformEvent t (RouteT t r m) where
  type Performable (RouteT t r m) = Performable m
  performEvent = lift . performEvent
  performEvent_ = lift . performEvent_

instance HasJS x m => HasJS x (RouteT t r m) where
  type JSX (RouteT t r m) = JSX m
  liftJS = lift . liftJS

instance (MonadHold t m, Adjustable t m) => Adjustable t (RouteT t r m) where
  runWithReplace a0 a' = RouteT $ runWithReplace (coerce a0) $ coerceEvent a'
  traverseIntMapWithKeyWithAdjust f a0 a' = RouteT $ traverseIntMapWithKeyWithAdjust (coerce f) (coerce a0) $ coerce a'
  traverseDMapWithKeyWithAdjust f a0 a' = RouteT $ traverseDMapWithKeyWithAdjust (\k v -> coerce $ f k v) (coerce a0) $ coerce a'
  traverseDMapWithKeyWithAdjustWithMove f a0 a' = RouteT $ traverseDMapWithKeyWithAdjustWithMove (\k v -> coerce $ f k v) (coerce a0) $ coerce a'

instance (Monad m, MonadQuery t vs m) => MonadQuery t vs (RouteT t r m) where
  tellQueryIncremental = lift . tellQueryIncremental
  askQueryResult = lift askQueryResult
  queryIncremental = lift . queryIncremental

instance MonadRef m => MonadRef (RouteT t r m) where
  type Ref (RouteT t r m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef r = lift . writeRef r
