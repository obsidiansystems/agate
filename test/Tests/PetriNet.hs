{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Tests.PetriNet where

import Data.GraphViz
import Data.List.Extra
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
                solverResult = Map.fromList $ enumerate <&> \v -> (v, map (Map.! v) runSolverSIR)
                layoutOpts = LayoutOpts{command = Neato, aspectRatio = 1 / 3}
                drawOpts = defaultDrawOpts
                chart animated =
                    areaChart animated 3 $
                        enumerate <&> \p ->
                            Variable
                                { name = placeName p
                                , colour = placeColour p
                                , values = take 1000 $ solverResult Map.! p
                                }
             in
                [ testGroup
                    "Implementation"
                    [ testCase "Transitions correct" $
                        assertBool "2 transitions present" $
                            length (transitions exampleSIR) == 2
                    ]
                , testGroup
                    "Diagrams"
                    [ goldenVsString "Petri" "test/outputs/petri/sir/petri.svg" $
                        diagToSVGBS <$> layoutAndDrawPetri layoutOpts drawOpts exampleSIR
                    , goldenVsString "Chart" "test/outputs/petri/sir/chart.svg" . pure . diagToSVGBS $ chart False
                    ]
                , goldenVsString "Combined" "test/outputs/petri/sir/combined.svg" do
                    petri <- layoutAndDrawPetri layoutOpts drawOpts{animation = Just (take 1000 . (solverResult Map.!))} exampleSIR
                    pure
                        . diagToSVGBS
                        $ vcat
                            [ scaleUToX 1 $ chart True
                            , scaleUToX 1 petri
                            ]
                ]
        , testGroup
            "Madrid"
            [ testGroup
                "Diagrams"
                [ goldenVsString "Petri" "test/outputs/petri/madrid/petri.svg" $
                    diagToSVGBS
                        <$> layoutAndDrawPetri
                            LayoutOpts{command = Neato, aspectRatio = 1}
                            defaultDrawOpts{placeSize = 15}
                            madridNet
                ]
            ]
        ]
