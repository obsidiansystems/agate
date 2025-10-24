module Tests.ODE.Solver where

import Math.Agate.Examples.ODE.Exponential
import Math.Agate.Examples.ODE.SIR
import Test.Tasty
import Test.Tasty.HUnit
import Math.Agate.Examples.ODE.Malthusian

odeSolverTests :: TestTree
odeSolverTests =
    testGroup
        "ODE Solver"
        [ testCase "Exponential ODE" $ do
            assertBool "Expected solution" (not (null runSolverExponential))
            assertBool "Expected positivity" (all (> 0) runSolverExponential)
        , let result = take 100 runSolverSIR
          in testCase "ODE Solver SIR Model" $ do
            assertBool "Expected solution" (not (null result))
            assertBool "Expected constant population" $ all (\m -> let t = sum m in t <= 1 + eps && t >= 1 - eps) result
        ,  let result = take 100 runSolverMalthusian
              in testCase "Malthusian Model" $ do
                  assertBool "Expected solution" (not (null result))
                  assertBool "Expected increasing population" 
                    . all (uncurry (<))
                    . zip result
                    $ drop 1 result
        ]
  where
    eps = 1e-6
