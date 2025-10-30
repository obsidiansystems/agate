module Math.Agate.ODE.Polynomial (PolynomialODE (..)) where

import Data.Map (Map)
import Data.Map qualified as Map
import Math.Agate.ODE
import Math.CommutativeAlgebra.Polynomial (GlexPoly)
import Math.CommutativeAlgebra.Polynomial qualified as Poly

newtype PolynomialODE k v = PolynomialODE (Map v (GlexPoly k v))
    deriving (Eq, Ord, Show)

instance (Ord v, Show v, Eq k, Num k) => ODESystem (PolynomialODE k v) where
    type Exp (PolynomialODE k v) = GlexPoly k v
    type Var (PolynomialODE k v) = v
    v += e = PolynomialODE (Map.singleton v e)
    var = Poly.var

instance (Ord v, Show v, Eq k, Num k) => Semigroup (PolynomialODE k v) where
    PolynomialODE m1 <> PolynomialODE m2 = PolynomialODE (Map.unionWith (+) m1 m2)

instance (Ord v, Show v, Eq k, Num k) => Monoid (PolynomialODE k v) where
    mempty = PolynomialODE Map.empty
