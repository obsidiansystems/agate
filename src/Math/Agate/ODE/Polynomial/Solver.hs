{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

module Math.Agate.ODE.Polynomial.Solver (odeSolve, ODEParams (..)) where

import Data.Map as Map
import Math.Agate.ODE.Polynomial
import Math.CommutativeAlgebra.Polynomial as Poly

data ODEParams k = ODEParams
  { stepSize :: k
  }

odeSolve :: forall v k. (Num k, Ord v, Show v, Eq k) => PolynomialODE k v -> ODEParams k -> Map v k -> [Map v k]
odeSolve s@(PolynomialODE p) params x0 =
  newValues : odeSolve s params newValues
  where
    varList :: [(GlexPoly k v, k)]
    varList = [(Poly.var vv, value) | (vv, value) <- Map.toList x0]
    newValues :: Map v k
    newValues =
      (flip Map.mapWithKey) p $ \v e ->
        case Map.lookup v x0 of
          Just e' -> e' + (stepSize params) * (Poly.eval e varList)
          Nothing -> error "key not found"
