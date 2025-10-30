{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant flip" #-}
module Math.Agate.ODE.Polynomial (PolynomialODE (..)) where

import Data.Map (Map)
import Data.Map qualified as Map
import Math.Agate.ODE
import Math.CommutativeAlgebra.Polynomial (GlexPoly)
import Math.CommutativeAlgebra.Polynomial qualified as Poly
import Data.List.Extra (enumerate)

newtype PolynomialODE k v = PolynomialODE (Map v (GlexPoly k v))
    deriving (Eq, Ord, Show)

instance (Enum v, Ord v, Show v, Eq k, Num k, Bounded v) => ODESystem (PolynomialODE k v) where
    type Exp (PolynomialODE k v) = GlexPoly k v
    type Var (PolynomialODE k v) = v
    type Field (PolynomialODE k v) = k
    v += e = PolynomialODE (Map.singleton v e)
    var = Poly.var
    eval assignment expr = Poly.eval expr $ flip fmap enumerate (\v -> (Poly.var v, assignment v))
    derivative (PolynomialODE sys) variable = sys Map.! variable

instance (Ord v, Show v, Eq k, Num k) => Semigroup (PolynomialODE k v) where
    PolynomialODE m1 <> PolynomialODE m2 = PolynomialODE (Map.unionWith (+) m1 m2)

instance (Ord v, Show v, Eq k, Num k) => Monoid (PolynomialODE k v) where
    mempty = PolynomialODE Map.empty
