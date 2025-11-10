{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Tests.PetriNet where

import Data.GraphViz
import Data.List.Extra
import Data.Map.Lazy qualified as Map
import Diagrams.AreaChart
import Diagrams.Prelude hiding (outer)
import Math.Agate.Diagrams.PetriNet
import Math.Agate.Examples.ODE.Malthusian
import Math.Agate.Examples.ODE.SCIR
import Math.Agate.Examples.ODE.SEAIR
import Math.Agate.Examples.ODE.SEIR
import Math.Agate.Examples.ODE.SIR
import Math.Agate.Examples.ODE.SIRD
import Math.Agate.Examples.ODE.SIRS
import Math.Agate.Examples.ODE.SIS
import Math.Agate.Examples.ODE.SIWR
import Math.Agate.Examples.PetriNet.Madrid
import Math.Agate.Examples.PetriNet.Malthusian
import Math.Agate.Examples.PetriNet.SCIR
import Math.Agate.Examples.PetriNet.SEAIR
import Math.Agate.Examples.PetriNet.SEIR
import Math.Agate.Examples.PetriNet.SIR
import Math.Agate.Examples.PetriNet.SIRD
import Math.Agate.Examples.PetriNet.SIRS
import Math.Agate.Examples.PetriNet.SIS
import Math.Agate.Examples.PetriNet.SIWR
import Math.Agate.PetriNet
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit
import TestUtils
import Utils

petriTests :: TestTree
petriTests =
    testGroup
        "Petri nets"
        [ testGroup
            "SIR"
            [ testGroup
                "Implementation"
                [ testCase "Transitions correct" $
                    assertBool "2 transitions present" $
                        length (transitions sir) == 2
                ]
            , allDiagramTests "sir" sir runSolverSIR
            ]
        , testGroup
            "SIRD"
            [allDiagramTests "sird" sird runSolverSIRD]
        , testGroup
            "SIRS"
            [allDiagramTests "sirs" sirs runSolverSIRS]
        , testGroup
            "SIS"
            [allDiagramTests "sis" sis runSolverSIS]
        , testGroup
            "Malthusian"
            [allDiagramTests "malthusian" malthusian runSolverMalthusian]
        , testGroup
            "SEIR"
            [allDiagramTests "seir" seir runSolverSEIR]
        , testGroup
            "SEAIR"
            [allDiagramTests "seair" seair runSolverSEAIR]
        , testGroup
            "SCIR"
            [allDiagramTests "scir" scir runSolverSCIR]
        , testGroup
            "SIWR"
            [allDiagramTests "siwr" siwr runSolverSIWR]
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

allDiagramTests ::
    (PetriPlace p, Bounded p, Enum p, Show p, Show t, Real t, Ord p) =>
    FilePath ->
    PetriNetImpl p t ->
    [Map.Map p Double] ->
    TestTree
allDiagramTests name net solution =
    testGroup
        "Diagrams"
        [ petriTest
        , chartTest
        , combinedTest
        ]
  where
    animationLength = 15
    solverResult =
        fmap
            (takeEvery 10)
            . Map.fromList
            $ enumerate <&> \v -> (v, map (Map.! v) solution)
    layoutOpts = LayoutOpts{command = Neato, aspectRatio = 1 / 3}
    drawOpts = defaultDrawOpts
    chart animated =
        areaChart animated 3 $
            enumerate <&> \p ->
                Variable
                    { name = placeName p
                    , colour = placeColour p
                    , values = take 100 $ solverResult Map.! p
                    }
    petriTest =
        goldenVsString "Petri" ("test/outputs/petri/" <> name <> "/petri.svg") $
            diagToSVGBS <$> layoutAndDrawPetri layoutOpts drawOpts net
    chartTest = goldenVsString "Chart" ("test/outputs/petri/" <> name <> "/chart.svg") . pure . diagToSVGBS $ chart Nothing
    combinedTest = goldenVsString "Combined" ("test/outputs/petri/" <> name <> "/combined.svg") do
        petri <- layoutAndDrawPetri layoutOpts drawOpts{animation = Just (take 100 . (solverResult Map.!), animationLength)} net
        pure
            . diagToSVGBS
            $ vcat
                [ scaleUToX 1 $ chart $ Just animationLength
                , scaleUToX 1 petri
                ]
