module Tests.PetriNet.Chart where

import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSL
import Data.List
import Data.Map.Lazy qualified as Map
import Diagrams.AreaChart (Variable (..), areaChart)
import Diagrams.Prelude
import Math.Agate.Examples.ODE.SIR
import Math.Agate.Examples.PetriNet.SIR
import Test.Tasty
import Test.Tasty.Golden
import TestUtils

petriChartTest :: TestTree
petriChartTest =
    testGroup
        "Area Chart Tests"
        [ goldenVsString
            "SIR SVG"
            "test/outputs/sir.svg"
            $ pure
            $ diagToSVGBS
            $ areaChart 3
            $ zipWith
                (\(colour, name) values -> Variable{name, colour, values})
                [ (uncurryRGB sRGB $ hsl 120 0.7 0.32, "recovered")
                , (uncurryRGB sRGB $ hsl 0 0.7 0.55, "infected")
                , (uncurryRGB sRGB $ hsl 240 0.7 0.4, "susceptible")
                ]
            $ transpose
            $ map (\(s, i, r) -> [r, i, s])
            $ take 100
            $ zip3 (runSolverSIR Map.! S) (runSolverSIR Map.! I) (runSolverSIR Map.! R)
        ]
