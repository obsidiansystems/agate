module Tests.ODE.Solver where

import Data.Colour.Names
import Data.Map.Lazy qualified as M
import Diagrams.AreaChart
import Math.Agate.Diagrams.PetriNet (PetriPlace (..))
import Math.Agate.Examples.ODE.Exponential
import Math.Agate.Examples.ODE.Malthusian
import Math.Agate.Examples.ODE.Pendulum
import Math.Agate.Examples.ODE.Rossler (runSolverRossler)
import Math.Agate.Examples.ODE.SIR
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit
import TestUtils
import Utils

odeSolverTests :: TestTree
odeSolverTests =
    testGroup
        "ODE Solver"
        [ testGroup
            "Exponential"
            [ testCase "Checks" $ do
                assertBool "Expected solution" (not (null runSolverExponential))
                assertBool "Expected positivity" (all (> 0) $ take 10000 runSolverExponential)
            , goldenVsString "Chart" "test/outputs/ode/exponential/chart.svg"
                . pure
                . diagToSVGBS
                $ areaChart
                    Nothing
                    3
                    [ Variable
                        { name = "x"
                        , colour = green
                        , values = takeEvery 100 $ take 10000 runSolverExponential
                        }
                    ]
            ]
        , testGroup
            "Rossler"
            [ goldenVsString "Chart" "test/outputs/ode/rossler/chart.svg"
                . pure
                . diagToSVGBS
                $ areaChart
                    Nothing
                    3
                    [ Variable
                        { name = "X"
                        , colour = blue
                        , values = takeEvery 1000 $ take 1000000 runSolverRossler
                        }
                    ]
            , testGroup
                "Double Pendulum"
                [ goldenVsString "Chart" "test/outputs/ode/pendulum/chart.svg"
                    . pure
                    . diagToSVGBS
                    $ lineChartInner
                        3
                        ( ( \v ->
                                Variable
                                    { name = placeSymbol v
                                    , colour = placeColour v
                                    , values = take 100 . takeEvery 100 $ (M.! v) <$> runSolverDoublePendulum
                                    }
                          )
                            <$> [Theta1, Theta2]
                        )
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
