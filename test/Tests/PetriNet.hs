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
import Graphics.Svg (prettyText)
import Diagrams.Prelude hiding (outer)
import Diagrams.Backend.SVG
import Data.List.NonEmpty qualified as NE
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSL
import qualified Data.Map.Lazy as Map
import Data.Maybe
import Math.Agate.ODE.Polynomial.Solver
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T


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
                    $ drawPetri p
            , goldenVsString "animation" "test/outputs/petri-sir-animated.svg" do
                p <- layoutPetri exampleSIR Neato
                pure
                    . encodeUtf8
                    . animatePetri
                    . prettyText
                    . renderDia SVG
                      ( SVGOptions
                        (mkSizeSpec (V2 (Just 1000) Nothing))
                        Nothing
                        mempty
                        []
                        True
                      )
                    $ drawPetriDynamic sirColour p
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
    where
      exampleSIR :: (Place net ~ String, Transition net ~ Double, PetriNet net) => net
      exampleSIR =  generalSIR

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

animatePetri :: Text -> Text
animatePetri svg =
  svg'
  <> mconcat (zipWith cssFor ["S", "I", "R"] sirData)
  <> T.pack "</svg>"
  where
    svg' :: Text
    svg' = T.unlines . init . T.lines $ svg
    sirData :: [[Double]]
    sirData = (\(x, y, z) -> [x, y, z]) $ unzip3 $ take 1000 runSolverSIR

cssFor :: String -> [Double] -> Text
cssFor name vs = T.pack $ unlines
  (["<style>",
    "@keyframes " ++ name ++ "_data {"] ++ [
    show p ++ "% {transform: scale(" ++ show v ++ ");}" | (v, p) <- zip vs ((\x -> x/(l-1) * 100) <$> [0..(l-1)]) ] ++ [
    "}",
    "#" ++ name ++ " {",
    "animation: " ++ name ++ "_data 15s linear infinite;",
    "transform-origin: 50% 50%;",
    "transform-box: fill-box;",
    "}",
    "</style>"
  ])
  where
    l :: Double
    l = fromIntegral . length $ vs

runSolverSIR :: [(Double, Double, Double)]
runSolverSIR =
    mapMaybe lookupSir $ odeSolve exampleSIRODE (ODEParams 0.1)
        $ Map.fromList [("S", 0.95), ("I", 0.05), ("R", 0)]
    where
        lookupSir m = do
            s <- Map.lookup "S" m
            i <- Map.lookup "I" m
            r <- Map.lookup "R" m
            return (s, i, r)

sirColour :: String -> Colour Double
sirColour = \case
  "R" -> uncurryRGB sRGB $ hsl 120 0.7 0.32
  "I" -> uncurryRGB sRGB $ hsl 0 0.7 0.55
  "S" -> uncurryRGB sRGB $ hsl 240 0.7 0.4
  _ -> black
