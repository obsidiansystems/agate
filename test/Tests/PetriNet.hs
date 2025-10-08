{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
module Tests.PetriNet where

import Math.Agate.PetriNet
import Test.Tasty
import Test.Tasty.HUnit
import Math.Agate.ODE.Polynomial (PolynomialODE)
import Test.Tasty.Golden
import Math.Agate.Diagrams.PetriNet
import Data.GraphViz
import Graphics.Svg
import Diagrams.Prelude
import Diagrams.Backend.SVG
import Data.List.NonEmpty qualified as NE

petriTests :: TestTree
petriTests =
    testGroup
        "Petri Nets Implementation"
        [ testGroup
            "SIR Model"
            [ testCase "Transitions Correct" $
                assert $
                    length (transitions exampleSIR) == 2
            , goldenVsString "diagram" "test/outputs/petri-sir.svg" do
                p <- layoutPetri exampleSIR Neato
                pure
                    . renderBS
                    . renderDia SVG
                      ( SVGOptions
                        (mkSizeSpec (V2 (Just 1000) Nothing))
                        Nothing
                        mempty
                        []
                        True
                      )
                    $ drawPetri p
            ]
        , testGroup
            "Madrid"
            [ goldenVsString "diagram" "test/outputs/petri-madrid.svg" do
                p <- layoutPetri madridNet Neato
                pure
                    . renderBS
                    . renderDia SVG
                      ( SVGOptions
                        (mkSizeSpec (V2 (Just 1000) Nothing))
                        Nothing
                        mempty
                        []
                        True
                      )
                    $ drawPetri p
            ]
         , testCase "SIR Model" $
            assertBool "Expected transitions" $
                length (transitions exampleSIR) == 2
        ]

exampleSIR :: (Place net ~ String, Fractional (Transition net), PetriNet net) => net
exampleSIR =  generalSIR id recovery transmission where
      recovery = 0.03
      transmission = 0.4


generalSIR :: (Fractional (Transition net), PetriNet net) => (String -> Place net) -> Transition net -> Transition net -> net
generalSIR place recovery transmission =
  mconcat
    [ transition [place "I", place "S"] transmission [place "I", place "I"]
    , transition [place "I"] recovery [place "R"]
    ]

exampleSIRODE :: PolynomialODE Double String
exampleSIRODE = asODE exampleSIR

madridNet :: (Place net ~ String, Transition net ~ Double, PetriNet net) => net
madridNet =
  mconcat
    [ transition [s] 1 [t] <> transition [t] 1 [s]
      | (s, t) <- mconcat [
         ("C",) <$> outer,
         zip outer (NE.tail (NE.fromList (cycle outer)))
      ]
    ]
  where
    outer =["N", "E", "SE", "S", "W", "NW"]
