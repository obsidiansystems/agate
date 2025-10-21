module Math.Agate.Examples.ODE.Exponential where
import Math.Agate.ODE.Polynomial (PolynomialODE (..))
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Math.Agate.ODE.Polynomial.Solver
import qualified Math.CommutativeAlgebra.Polynomial as Poly

-- simple ODE example where x' = x
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
    mapMaybe (Map.lookup "x") (Prelude.take 10000 (odeSolve exponentialODE (ODEParams 0.0001) (Map.fromList [("x", 1)])))
