{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Tests.PetriNet where

import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSL
import Data.GraphViz
import Data.Map.Lazy qualified as Map
import Diagrams.AreaChart
import Diagrams.Backend.SVG
import Diagrams.Prelude hiding (outer)
import Math.Agate.Diagrams.PetriNet
import Math.Agate.Examples.ODE.SIR
import Math.Agate.Examples.PetriNet.Madrid
import Math.Agate.Examples.PetriNet.SIR
import Math.Agate.PetriNet
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit
import TestUtils

petriTests :: TestTree
petriTests =
    testGroup
        "Petri Nets Implementation"
        [ testGroup
            "SIR Model"
            [ testCase "Transitions Correct" $
                assertBool "2 Transitions present" $
                    length (transitions exampleSIR) == 2
            , goldenVsString "diagram" "test/outputs/petri-sir.svg" do
                p <- layoutPetri exampleSIR $ LayoutOpts (1 / 3) Neato
                pure . diagToSVGBS $ drawPetri defaultDrawOpts sirColour p
            , goldenVsString "animation with chart" "test/outputs/petri-sir-animated-overlayed.svg" do
                p <- layoutPetri exampleSIR $ LayoutOpts (1 / 3) Neato
                pure
                    . diagToSVGBS
                    $ vcat
                        [ scale 160 animatedAreaChart
                        , drawPetriDynamic
                            defaultDrawOpts
                            sirColour
                            (take 1000 . (runSolverSIR Map.!))
                            p
                        ]
            ]
        , testGroup
            "Madrid"
            [ goldenVsString "diagram" "test/outputs/petri-madrid.svg" do
                p <- layoutPetri madridNet $ LayoutOpts 1 Neato
                pure . diagToSVGBS $ drawPetri defaultDrawOpts{placeSize = 15} (const white) p
            ]
        , testCase "SIR Model" $
            assertBool "Expected transitions" $
                length (transitions exampleSIR) == 2
        ]
  where
    exampleSIR :: (Place net ~ SIRPlace, Transition net ~ Double, PetriNet net) => net
    exampleSIR = generalSIR

animatedAreaChart :: QDiagram B V2 Double Any
animatedAreaChart = movingRect <> chart
  where
    chart =
        areaChart
            3
            ( zipWith
                (\(colour, name) values -> Variable{name, colour, values})
                [ (uncurryRGB sRGB $ hsl 240 0.7 0.4, "susceptible")
                , (uncurryRGB sRGB $ hsl 0 0.7 0.55, "infected")
                , (uncurryRGB sRGB $ hsl 120 0.7 0.32, "recovered")
                ]
                $ (\ls -> [S, I, R] <&> take 1000 . (ls Map.!))
                    runSolverSIR
            )
    movingRect = animate t $ rect 0.005 1
      where
        t = TransformAnimation 15 Nothing $ TranslateAnimation [V2 0 0, V2 3 0]
