module Main (main) where

import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSL
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
                [ (uncurryRGB sRGB $ hsl 120 0.7 0.32, "recovered")
                , (uncurryRGB sRGB $ hsl 0 0.7 0.55, "infected")
                , (uncurryRGB sRGB $ hsl 240 0.7 0.4, "susceptible")
                ]
            $ transpose
            $ map (\(s, i, r) -> [r, i, s])
            $ runSolverSIR
        ]
