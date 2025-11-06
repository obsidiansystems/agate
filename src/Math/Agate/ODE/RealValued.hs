module Math.Agate.ODE.RealValued (RealValuedODE (..), RealFunction (..), promoteUnary) where

import Data.Map (Map)
import Data.Map qualified as Map
import Math.Agate.ODE

type Assignment v k = v -> k

newtype RealFunction v k = RealFunction (Assignment v k -> k)

instance (Num k) => Num (RealFunction v k) where
    (+) = promoteBinary (+)
    (-) = promoteBinary (-)
    (*) = promoteBinary (*)
    signum = promoteUnary signum
    abs = promoteUnary abs
    fromInteger n = RealFunction (const $ fromInteger n)

instance (Fractional k) => Fractional (RealFunction v k) where
    fromRational v = RealFunction (const $ fromRational v)
    (/) = promoteBinary (/)

promoteBinary :: (k -> k -> k) -> RealFunction v k -> RealFunction v k -> RealFunction v k
promoteBinary f (RealFunction f1) (RealFunction f2) = RealFunction (\a -> f1 a `f` f2 a)

promoteUnary :: (k -> k) -> RealFunction v k -> RealFunction v k
promoteUnary op (RealFunction f1) = RealFunction (op . f1)

newtype RealValuedODE v k = RealValuedODE (Map v (RealFunction v k))

instance (Ord v, Show v, Eq k, Num k) => ODESystem (RealValuedODE v k) where
    type Exp (RealValuedODE v k) = RealFunction v k
    type Var (RealValuedODE v k) = v
    type Field (RealValuedODE v k) = k
    v += e = RealValuedODE (Map.singleton v e)
    var v = RealFunction (\assignment -> assignment v)
    eval a (RealFunction f) = f a
    derivative (RealValuedODE eqns) v = eqns Map.! v

instance (Ord v, Show v, Eq k, Num k) => Semigroup (RealValuedODE v k) where
    RealValuedODE m1 <> RealValuedODE m2 = RealValuedODE (Map.unionWith (+) m1 m2)

instance (Ord v, Show v, Eq k, Num k) => Monoid (RealValuedODE v k) where
    mempty = RealValuedODE Map.empty
