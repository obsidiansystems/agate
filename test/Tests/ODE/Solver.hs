module Tests.ODE.Solver where

import Data.Colour.Names
import Diagrams.AreaChart
import Math.Agate.Examples.ODE.Exponential
import Math.Agate.Examples.ODE.Malthusian
import Math.Agate.Examples.ODE.SIR
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit
import TestUtils

odeSolverTests :: TestTree
odeSolverTests =
    testGroup
        "ODE Solver"
        [ testGroup
            "Exponential"
            [ testCase "Checks" $ do
                assertBool "Expected solution" (not (null runSolverExponential))
                assertBool "Expected positivity" (all (> 0) runSolverExponential)
            , goldenVsString "Chart" "test/outputs/ode/exponential/chart.svg"
                . pure
                . diagToSVGBS
                $ areaChart
                    False
                    3
                    [ Variable
                        { name = "x"
                        , colour = green
                        , values = takeEvery 100 runSolverExponential
                        }
                    ]
            ]
        , let result = take 100 runSolverSIR
           in testCase "SIR" $ do
                assertBool "Expected solution" (not (null result))
                assertBool "Expected constant population" $ all (\m -> let t = sum m in t <= 1 + eps && t >= 1 - eps) result
        , let result = take 100 runSolverMalthusian
           in testCase "Malthusian" $ do
                assertBool "Expected solution" (not (null result))
                assertBool "Expected increasing population"
                    . all (uncurry (<))
                    . zip result
                    $ drop 1 result
        ]
  where
    eps = 1e-6
