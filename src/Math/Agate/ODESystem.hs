{-# LANGUAGE AllowAmbiguousTypes #-}

module Math.Agate.ODESystem where

import Data.Map (Map)
import Data.Map qualified as Map
import Math.CommutativeAlgebra.Polynomial qualified as Poly
import Math.CommutativeAlgebra.Polynomial
  (GlexPoly)

-- | First order ODEs
class (Monoid system, Num (Exp system)) => ODESystem system where
  -- The type of mathematical expressions associated to the system of ODEs
  type Exp system
  -- Type type of variables (occurring in those expressions)
  type Var system
  -- | Express a basic differential equation of the form var' = expr. The monoid instance for system should add these rates of change together for each given variable.
  (+=) :: Var system -> Exp system -> system
  -- | Contruct the expression which is just a single variable
  var :: Var system -> Exp system

infixl 0 +=

newtype PolynomialODE k v = PolynomialODE (Map v (GlexPoly k v))
  deriving (Eq, Ord, Show)

instance (Ord v, Show v, Eq k, Num k) => ODESystem (PolynomialODE k v) where
  type Exp (PolynomialODE k v) = GlexPoly k v
  type Var (PolynomialODE k v) = v
  v += e = PolynomialODE (Map.singleton v e)
  var v = Poly.var v

instance (Ord v, Show v, Eq k, Num k) => Semigroup (PolynomialODE k v) where
  PolynomialODE m1 <> PolynomialODE m2 = PolynomialODE (Map.unionWith (+) m1 m2)

instance (Ord v, Show v, Eq k, Num k) => Monoid (PolynomialODE k v) where
  mempty = PolynomialODE Map.empty
