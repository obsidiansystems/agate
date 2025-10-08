module Tests.PetriNet.Chart where

import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSL
import Data.List
import Diagrams.Backend.SVG
import Diagrams.Prelude
import Graphics.Svg
import Test.Tasty
import Test.Tasty.Golden
import Diagrams.AreaChart (Variable(..), areaChart)
import qualified Data.Map.Lazy as Map
import Data.Maybe (catMaybes)
import Math.Agate.ODE.Polynomial.Solver
import Tests.PetriNet (exampleSIRODE)

petriChartTest :: TestTree
petriChartTest = testGroup "Area Chart Tests" [
    goldenVsString
                "SIR SVG"
                "test/outputs/sir.svg"
                $ pure
                $ renderBS
                $ renderDia SVG (SVGOptions (mkSizeSpec (V2 (Just 1000) Nothing)) Nothing mempty [] True)
                $ areaChart 3
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

runSolverSIR :: [(Double, Double, Double)]
runSolverSIR =
    catMaybes (lookupSir <$> Prelude.take 100 (
        odeSolve exampleSIRODE (ODEParams 0.1) (Map.fromList [("S", 0.95), ("I", 0.05), ("R", 0)]))
        ) where
            lookupSir m = do
                s <- Map.lookup "S" m
                i <- Map.lookup "I" m
                r <- Map.lookup "R" m
                return (s, i, r)
