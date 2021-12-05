module Reflex.Dom.Route.Simple.Internal.ParseRoute
  ( parseRoutes,
    ParseRouteG,
  )
where

import Control.Applicative
import Data.Kind
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import GHC.TypeLits
import Reflex.Dom.Route.Simple.Internal.Common
import Unsafe.Coerce
import Web.HttpApiData

class ParseRoute x where
  parseRoute :: [Text] -> Maybe (ConcreteRoute x)

instance (ParseRoute y, KnownSymbol s) => ParseRoute ((s :: Symbol) :/ y) where
  parseRoute (s' : rest)
    | T.toLower s' == T.toLower (T.pack (symbolVal @s Proxy)) =
      (Proxy :/) <$> parseRoute @y rest
  parseRoute _ = Nothing
  {-# INLINE parseRoute #-}

instance (ParseRoute y, FromHttpApiData x) => ParseRoute ((x :: Type) :/ y) where
  parseRoute (s : rest) | Right x' <- parseUrlPiece s = (x' :/) <$> parseRoute @y rest
  parseRoute _ = Nothing
  {-# INLINE parseRoute #-}

instance FromHttpApiData x => ParseRoute (x :: Type) where
  parseRoute [s] | Right x' <- parseUrlPiece s = Just x'
  parseRoute _ = Nothing
  {-# INLINE parseRoute #-}

instance (KnownSymbol s) => ParseRoute (s :: Symbol) where
  parseRoute [s'] | s' == T.pack (symbolVal @s Proxy) = Just Proxy
  parseRoute _ = Nothing
  {-# INLINE parseRoute #-}

class ParseRouteG f where
  parseRouteG :: [Text] -> Maybe (f p)

instance ParseRoute c => ParseRouteG (K1 i (InternalConcreteRoute c)) where
  parseRouteG r = K1 . InternalConcreteRoute <$> parseRoute @c r
  {-# INLINE parseRouteG #-}

instance ParseRouteG x => ParseRouteG (M1 i t x) where
  parseRouteG r = M1 <$> parseRouteG @x r
  {-# INLINE parseRouteG #-}

instance ParseRouteG U1 where
  parseRouteG [] = Just U1
  parseRouteG _ = Nothing
  {-# INLINE parseRouteG #-}

instance (ParseRouteG a, ParseRouteG b) => ParseRouteG (a :+: b) where
  parseRouteG r = (L1 <$> parseRouteG @a r) <|> (R1 <$> parseRouteG @b r)
  {-# INLINE parseRouteG #-}

instance (TypeError ProductTypeError) => ParseRouteG (a :*: b) where
  parseRouteG = undefined
  {-# INLINE parseRouteG #-}

parseRoutes ::
  forall r.
  (Generic (r InternalRouteValue), ParseRouteG (Rep (r InternalRouteValue))) =>
  [Text] ->
  Maybe (r RouteValue)
parseRoutes = fmap ((unsafeCoerce :: r InternalRouteValue -> r RouteValue) . to) . parseRouteG
{-# INLINE parseRoutes #-}
