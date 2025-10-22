{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Tests.PetriNet where

import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSL
import Data.GraphViz
import Data.Map.Lazy qualified as Map
import Diagrams.AreaChart
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
        "Petri nets"
        [ testGroup
            "SIR"
            let
                exampleSIR :: (Place net ~ SIRPlace, Transition net ~ Double, PetriNet net) => net
                exampleSIR = generalSIR
                chart animated =
                    areaChart
                        animated
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
             in
                [ testGroup
                    "Implementation"
                    [ testCase "Transitions correct" $
                        assertBool "2 transitions present" $
                            length (transitions exampleSIR) == 2
                    ]
                , testGroup
                    "Diagrams"
                    [ goldenVsString "Petri" "test/outputs/petri/sir/petri.svg" do
                        p <- layoutPetri exampleSIR $ LayoutOpts (1 / 3) Neato
                        pure . diagToSVGBS $ drawPetri defaultDrawOpts{vertexColour = sirColour} p
                    , goldenVsString "Chart" "test/outputs/petri/sir/chart.svg" . pure . diagToSVGBS $ chart False
                    ]
                , goldenVsString "Combined" "test/outputs/petri/sir/combined.svg" do
                    p <- layoutPetri exampleSIR $ LayoutOpts (1 / 3) Neato
                    pure
                        . diagToSVGBS
                        $ vcat
                            [ scale 160 $ chart True
                            , drawPetri
                                defaultDrawOpts
                                    { vertexColour = sirColour
                                    , animation = Just (take 1000 . (runSolverSIR Map.!))
                                    }
                                p
                            ]
                ]
        , testGroup
            "Madrid"
            [ testGroup
                "Diagrams"
                [ goldenVsString "Petri" "test/outputs/petri/madrid/petri.svg" do
                    p <- layoutPetri madridNet $ LayoutOpts 1 Neato
                    pure . diagToSVGBS $ drawPetri defaultDrawOpts{placeSize = 15} p
                ]
            ]
        ]
