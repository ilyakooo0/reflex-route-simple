module Reflex.Dom.Route.Simple.Internal.PrintRoute
  ( printRouteValue,
    PrintRouteG,
  )
where

import Data.Kind
import Data.Proxy
import qualified Data.Text as T
import GHC.Generics
import GHC.TypeLits
import Reflex.Dom.Route.Simple.Internal.Common
import Unsafe.Coerce
import Web.HttpApiData

class PrintRoute x where
  printRoute :: InternalConcreteRoute x -> [String]

instance
  (PrintRoute y, KnownSymbol s) =>
  PrintRoute ((s :: Symbol) :/ y)
  where
  printRoute (InternalConcreteRoute (_ :/ y)) = symbolVal @s Proxy : printRoute @y (InternalConcreteRoute y)
  {-# INLINE printRoute #-}

instance
  (PrintRoute y, ToHttpApiData x) =>
  PrintRoute ((x :: Type) :/ y)
  where
  printRoute (InternalConcreteRoute (x :/ y)) = T.unpack (toUrlPiece x) : printRoute @y (InternalConcreteRoute y)
  {-# INLINE printRoute #-}

instance ToHttpApiData x => PrintRoute (x :: Type) where
  printRoute (InternalConcreteRoute x) = [T.unpack $ toUrlPiece x]
  {-# INLINE printRoute #-}

instance KnownSymbol s => PrintRoute (s :: Symbol) where
  printRoute _ = [symbolVal @s Proxy]
  {-# INLINE printRoute #-}

class PrintRouteG x where
  printRouteG :: x p -> [String]

instance PrintRoute c => PrintRouteG (K1 i (InternalConcreteRoute c)) where
  printRouteG (K1 x) = printRoute @c x
  {-# INLINE printRouteG #-}

instance PrintRouteG U1 where
  printRouteG _ = []
  {-# INLINE printRouteG #-}

instance PrintRouteG x => PrintRouteG (M1 i t x) where
  printRouteG (M1 x) = printRouteG x
  {-# INLINE printRouteG #-}

instance (PrintRouteG a, PrintRouteG b) => PrintRouteG (a :+: b) where
  printRouteG (L1 x) = printRouteG x
  printRouteG (R1 x) = printRouteG x
  {-# INLINE printRouteG #-}

instance (TypeError ProductTypeError) => PrintRouteG (a :*: b) where
  printRouteG = undefined
  {-# INLINE printRouteG #-}

printRouteValue ::
  forall r.
  ( Generic (r InternalRouteValue),
    PrintRouteG (Rep (r InternalRouteValue))
  ) =>
  r RouteValue ->
  [String]
printRouteValue =
  printRouteG @(Rep (r InternalRouteValue)) . from . (unsafeCoerce :: r RouteValue -> r InternalRouteValue)
{-# INLINE printRouteValue #-}
