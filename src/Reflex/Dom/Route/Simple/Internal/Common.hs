module Reflex.Dom.Route.Simple.Internal.Common
  ( (://) (..),
    ProductTypeError,
    NoGenericError,
    (:#),
    RouteValue,
    InternalRouteValue,
    InternalConcreteRoute (..),
    ConcreteRoute,
    Defined,
    NoGeneric,
  )
where

import Data.Kind
import Data.Proxy
import GHC.Generics (Generic (..))
import GHC.TypeLits

type ProductTypeError = 'Text "Product types (records) are not allowed in routes"

type NoGenericError t = 'Text "Did you forget to derive `Generic' for " :<>: ShowType t

data a :// b = a :/ b
  deriving stock (Eq, Ord, Show)

infixr 4 :/

type family (r :: k1) :# (x :: k2)

infixl 0 :#

data RouteValue

type instance RouteValue :# x = ConcreteRoute x

data InternalRouteValue

type instance InternalRouteValue :# x = InternalConcreteRoute x

newtype InternalConcreteRoute x = InternalConcreteRoute (ConcreteRoute x)

type family ConcreteRoute (x :: k)

type instance ConcreteRoute ((s :: Symbol) :/ y) = Proxy s :// ConcreteRoute y

type instance ConcreteRoute ((x :: Type) :/ y) = x :// ConcreteRoute y

type instance ConcreteRoute (x :: Type) = x

type instance ConcreteRoute (s :: Symbol) = Proxy s

-- Type error stuff

type family NoGeneric (a :: k)  :: Constraint where
  NoGeneric a =
    PrettyError '[ 'Text "No instance for " ':<>: QuoteType (ShowType Generic :<>: 'Text " " :<>: ShowType a)]

type family PrettyError (ctxt :: [ErrorMessage]) :: k where
  PrettyError '[] = TypeError ('Text "")
  PrettyError (c ': cs) = TypeError ('Text "| " ':<>: c ':$$: PrettyLines cs)

type family PrettyLines (ctxt :: [ErrorMessage]) :: ErrorMessage where
  PrettyLines '[] = 'Text ""
  PrettyLines (c ': cs) = 'Text "|   " ':<>: c ':$$: PrettyLines cs

type family Defined (break :: Type -> Type) (err :: Constraint) :: Constraint where
  Defined Void1 _  = Any
  Defined _ _ = ()

data Void1 a

type family Any :: k

type family QuoteType (err :: ErrorMessage) :: ErrorMessage where
  QuoteType err = 'Text "‘" ':<>: err ':<>: 'Text "’"
