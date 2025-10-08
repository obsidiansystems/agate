module Tests.ODE.Solver where

import Math.Agate.ODE.Polynomial (PolynomialODE (..))
import qualified Data.Map.Lazy as Map
import Math.CommutativeAlgebra.Polynomial as Poly
import Data.Maybe (mapMaybe)
import Math.Agate.ODE.Polynomial.Solver
import Tests.PetriNet (exampleSIRODE)
import Test.Tasty
import Test.Tasty.HUnit

odeSolverTests :: TestTree
odeSolverTests = testGroup "ODE Solver"
    [ testCase "Exponential ODE" $ do
        assertBool "Expected solution" (not (null runSolverExponential))
        assertBool "Expected positivity" (all (> 0) runSolverExponential)
    , testCase "ODE Solver SIR Model" $ do
        assertBool "Expected solution" (not (null runSolverSIR))
        assertBool "Expected constant population" (all (\(s, i, r) -> s + i + r <= 1 + eps && s + i + r >= 1 - eps) runSolverSIR)
    ]
    where eps = 1e-6

-- simple ODE example where x' = x
sampleODE :: PolynomialODE Double String
sampleODE =
  PolynomialODE $
    Map.fromList
      [ ("x", x)
      ]
  where
    x = Poly.var "x"

runSolverExponential :: [Double]
runSolverExponential =
    mapMaybe (Map.lookup "x") (Prelude.take 10000 (odeSolve sampleODE (ODEParams 0.0001) (Map.fromList [("x", 1)])))

runSolverSIR :: [(Double, Double, Double)]
runSolverSIR =
    mapMaybe lookupSir (Prelude.take 100 (
        odeSolve exampleSIRODE (ODEParams 0.1) (Map.fromList [("S", 0.95), ("I", 0.05), ("R", 0)]))) where
            lookupSir m = do
                s <- Map.lookup "S" m
                i <- Map.lookup "I" m
                r <- Map.lookup "R" m
                return (s, i, r)
