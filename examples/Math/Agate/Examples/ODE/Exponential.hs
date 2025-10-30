module Math.Agate.Examples.ODE.Exponential where

import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Math.Agate.ODE.Polynomial (PolynomialODE (..))
import Math.Agate.ODE.Polynomial.Solver
import Math.CommutativeAlgebra.Polynomial qualified as Poly

-- | Simple ODE example where x' = x
exponentialODE :: PolynomialODE Double String
exponentialODE =
    PolynomialODE $
        Map.fromList
            [ ("x", x)
            ]
  where
    x = Poly.var "x"

runSolverExponential :: [Double]
runSolverExponential =
    mapMaybe (Map.lookup "x")
        . odeSolve exponentialODE (ODEParams 0.0001)
        $ Map.fromList [("x", 1)]
