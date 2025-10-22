module Tests.ODE.Solver where

import Math.Agate.Examples.ODE.Exponential
import Math.Agate.Examples.ODE.SIR
import Test.Tasty
import Test.Tasty.HUnit

odeSolverTests :: TestTree
odeSolverTests =
    testGroup
        "ODE Solver"
        [ testCase "Exponential ODE" $ do
            assertBool "Expected solution" (not (null runSolverExponential))
            assertBool "Expected positivity" (all (> 0) runSolverExponential)
        , testCase "ODE Solver SIR Model" $ do
            assertBool "Expected solution" (not (null solverResult))
            assertBool "Expected constant population" $ all (\m -> let t = sum m in t <= 1 + eps && t >= 1 - eps) solverResult
        ]
  where
    eps = 1e-6
    solverResult = take 100 runSolverSIR
