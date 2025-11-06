module Math.Agate.ODE.RealValued (RealValuedODE (..), RealFunction (..), promoteUnary) where

import Data.Map (Map)
import Data.Map qualified as Map
import Math.Agate.ODE

type Assignment k v = v -> k

newtype RealFunction k v = RealFunction (Assignment k v -> k)

instance (Num k) => Num (RealFunction k v) where
    (+) = promoteBinary (+)
    (-) = promoteBinary (-)
    (*) = promoteBinary (*)
    signum = promoteUnary signum
    abs = promoteUnary abs
    fromInteger n = RealFunction (const $ fromInteger n)

instance (Fractional k) => Fractional (RealFunction k v) where
    fromRational k = RealFunction (const $ fromRational k)
    (/) = promoteBinary (/)

promoteBinary :: (k -> k -> k) -> RealFunction k v -> RealFunction k v -> RealFunction k v
promoteBinary f (RealFunction f1) (RealFunction f2) = RealFunction (\a -> f1 a `f` f2 a)

promoteUnary :: (k -> k) -> RealFunction k v -> RealFunction k v
promoteUnary op (RealFunction f1) = RealFunction (op . f1)

newtype RealValuedODE k v = RealValuedODE (Map v (RealFunction k v))

instance (Ord v, Show v, Eq k, Num k) => ODESystem (RealValuedODE k v) where
    type Exp (RealValuedODE k v) = RealFunction k v
    type Var (RealValuedODE k v) = v
    type Field (RealValuedODE k v) = k
    v += e = RealValuedODE (Map.singleton v e)
    var v = RealFunction (\assignment -> assignment v)
    eval a (RealFunction f) = f a
    derivative (RealValuedODE eqns) v = eqns Map.! v

instance (Ord v, Show v, Eq k, Num k) => Semigroup (RealValuedODE k v) where
    RealValuedODE m1 <> RealValuedODE m2 = RealValuedODE (Map.unionWith (+) m1 m2)

instance (Ord v, Show v, Eq k, Num k) => Monoid (RealValuedODE k v) where
    mempty = RealValuedODE Map.empty
