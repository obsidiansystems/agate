{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests.PetriNet where

import Math.Agate.PetriNet
import Test.Tasty
import Test.Tasty.HUnit
import Math.Agate.ODE.Polynomial (PolynomialODE)
import Test.Tasty.Golden
import Math.Agate.Diagrams.PetriNet
import Data.GraphViz
import Graphics.Svg (prettyText)
import Diagrams.Prelude hiding (outer)
import Diagrams.Backend.SVG
import Data.List.NonEmpty qualified as NE
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSL
import qualified Data.Map.Lazy as Map
import Math.Agate.ODE.Polynomial.Solver
import Data.Map (Map)
import Diagrams.AreaChart

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
                p <- layoutPetri exampleSIR Neato
                pure
                    . encodeUtf8
                    . prettyText
                    . renderDia SVG
                      ( SVGOptions
                        (mkSizeSpec (V2 (Just 1000) Nothing))
                        Nothing
                        mempty
                        []
                        True
                      )
                    $ drawPetri sirColour p
            , goldenVsString "animation" "test/outputs/petri-sir-animated.svg" do
                p <- layoutPetri exampleSIR Neato
                pure
                    . encodeUtf8
                    . prettyText
                    . renderDia SVG
                      ( SVGOptions
                        (mkSizeSpec (V2 (Just 1000) Nothing))
                        Nothing
                        mempty
                        []
                        True
                      )
                    $ drawPetriDynamic
                        sirColour
                        (take 1000 . (runSolverSIR Map.!))
                        p
            , goldenVsString "animation with chart" "test/outputs/petri-sir-animated-overlayed.svg" do
                p <- layoutPetri exampleSIR Neato
                pure
                    . encodeUtf8
                    . prettyText
                    . renderDia SVG
                      ( SVGOptions
                        (mkSizeSpec (V2 (Just 1000) Nothing))
                        Nothing
                        mempty
                        []
                        True
                      )
                    $ vcat [
                        scale 80 animatedAreaChart,
                        drawPetriDynamic
                          sirColour
                          (take 1000 . (runSolverSIR Map.!))
                          p
                  ]
            ]
        , testGroup
            "Madrid"
            [ goldenVsString "diagram" "test/outputs/petri-madrid.svg" do
                p <- layoutPetri madridNet Neato
                pure
                    . encodeUtf8
                    . prettyText
                    . renderDia SVG
                      ( SVGOptions
                        (mkSizeSpec (V2 (Just 3000) Nothing))
                        Nothing
                        mempty
                        []
                        True
                      )
                    $ drawPetri (const white) p
            ]
         , testCase "SIR Model" $
            assertBool "Expected transitions" $
                length (transitions exampleSIR) == 2
        ]
    where
      exampleSIR :: (Place net ~ String, Transition net ~ Double, PetriNet net) => net
      exampleSIR =  generalSIR


animatedAreaChart :: QDiagram B V2 Double Any
animatedAreaChart = movingRect <> chart
  where
    chart = areaChart 3 (zipWith
                  (\(colour, name) values -> Variable{name, colour, values})
                  [ (uncurryRGB sRGB $ hsl 240 0.7 0.4, "susceptible")
                  , (uncurryRGB sRGB $ hsl 0 0.7 0.55, "infected")
                  , (uncurryRGB sRGB $ hsl 120 0.7 0.32, "recovered")
                  ]
                  $ (\ls ->  ["S", "I", "R"] <&> take 1000 . (ls Map.!))
                  runSolverSIR)
    movingRect = animate t $ rect 0.005 1
      where
        t = TransformAnimation 15 Nothing $ TranslateAnimation [V2 0 0, V2 3 0]

exampleSIRODE :: PolynomialODE Double String
exampleSIRODE = asODE generalSIR

generalSIR :: (Place net ~ String, Fractional (Transition net), PetriNet net) => net
generalSIR =
  mconcat
    [ transition ["I", "S"] transmission ["I", "I"]
    , transition ["I"] recovery ["R"]
    ]
    where
      transmission = 0.4
      recovery = 0.03

madridNet :: (Place net ~ String, Transition net ~ Double, PetriNet net) => net
madridNet =
  mconcat
    [ transition [s] 1 [t] <> transition [t] 1 [s]
      | (s, t) <- (("C",) <$> outer) ++ zip outer (NE.tail (NE.fromList (cycle outer)))
    ]
  where
    outer =["N", "E", "SE", "S", "W", "NW"]

runSolverSIR :: Map String [Double]
runSolverSIR =
        (\ls -> Map.fromList $ ["S", "I", "R"] <&> \v -> (v, map (Map.! v) ls))
        $ odeSolve exampleSIRODE (ODEParams 0.1)
        $ Map.fromList [("S", 0.95), ("I", 0.05), ("R", 0)]

sirColour :: String -> Colour Double
sirColour = \case
  "R" -> uncurryRGB sRGB $ hsl 120 0.7 0.32
  "I" -> uncurryRGB sRGB $ hsl 0 0.7 0.55
  "S" -> uncurryRGB sRGB $ hsl 240 0.7 0.4
  _ -> black
