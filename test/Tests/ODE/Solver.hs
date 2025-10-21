module Tests.ODE.Solver where

import qualified Data.Map.Lazy as Map
import Math.Agate.Examples.ODE.SIR (runSolverSIR)
import Test.Tasty
import Test.Tasty.HUnit
import Math.Agate.Examples.ODE.Exponential

odeSolverTests :: TestTree
odeSolverTests = testGroup "ODE Solver"
    [ testCase "Exponential ODE" $ do
        assertBool "Expected solution" (not (null runSolverExponential))
        assertBool "Expected positivity" (all (> 0) runSolverExponential)
    , testCase "ODE Solver SIR Model" $ do
        assertBool "Expected solution" (not (null solverResult))
        assertBool "Expected constant population" (all (\(s, i, r) -> s + i + r <= 1 + eps && s + i + r >= 1 - eps) solverResult)
    ]
    where
        eps = 1e-6
        solverResult = take 100 $ zip3 ss is rs
        ss = runSolverSIR Map.! "S"
        is = runSolverSIR Map.! "I"
        rs = runSolverSIR Map.! "R"
