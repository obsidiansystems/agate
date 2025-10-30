module Math.Agate.Examples.ODE.Exponential where

import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Math.Agate.ODE.Polynomial (PolynomialODE (..))
import Math.Agate.ODE.Polynomial.Solver
import Math.CommutativeAlgebra.Polynomial qualified as Poly

data ExponentialVar = X deriving (Show, Eq, Ord, Enum, Bounded)
--
-- simple ODE example where x' = x
exponentialODE :: PolynomialODE Double ExponentialVar
exponentialODE =
    PolynomialODE $
        Map.fromList
            [ (X, x)
            ]
  where
    x = Poly.var X

runSolverExponential :: [Double]
runSolverExponential =
    mapMaybe (Map.lookup X)
        . odeSolve exponentialODE (ODEParams 0.0001)
        $ Map.fromList [(X, 1)]
