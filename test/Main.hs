module Main (main) where

import Data.List
import Diagrams.Backend.SVG
import Diagrams.Prelude
import Graphics.Svg
import Math.Agate.Examples.ODE
import Math.Agate.PetriNetDiagram
import Test.Tasty
import Test.Tasty.Golden

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "Tests"
        [ goldenVsString
            "SIR SVG"
            "test/outputs/sir.svg"
            $ pure
            $ renderBS
            $ renderDia SVG (SVGOptions (mkSizeSpec (V2 (Just 1000) Nothing)) Nothing mempty [] True)
            $ sirDiagramDecorated 3
            $ zipWith
                (\(colour, name) values -> Variable{name, colour, values})
                [ (green, "recovered")
                , (blue, "susceptible")
                , (red, "infected")
                ]
            $ transpose
            $ map (\(s, i, r) -> [r, s, i])
            $ runSolverSIR
        ]
