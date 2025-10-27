module Diagrams.AreaChart (areaChart, Variable (..)) where

import Control.Applicative
import Data.List
import Data.Maybe
import Data.Monoid.Extra
import Diagrams.Backend.SVG
import Diagrams.Prelude

data Variable = Variable
    { name :: String
    , colour :: Colour Double
    , values :: [Double]
    }

areaChartInner :: Double -> [Variable] -> Diagram B
areaChartInner overallWidth sirData =
    scaleY (1 / maximum (snd $ last ys)) . mconcat $
        zipWith
            ( \(bottoms, tops) Variable{colour} ->
                fromVertices
                    (zipWith (curry p2) [0, w ..] bottoms <> reverse (zipWith (curry p2) [0, w ..] tops))
                    & closeLine
                    & strokeLoop
                    & translate (maybe 0 (V2 0) $ listToMaybe bottoms)
                    & fc colour
                    & lcA transparent
            )
            ys
            sirData
  where
    ys = map unzip $ mapColumns (adjacentPairs . scanl (+) 0) $ map (\Variable{values} -> values) sirData
    w = overallWidth / maximum (map (genericLength . \Variable{values} -> values) sirData)
    adjacentPairs :: [a] -> [(a, a)]
    adjacentPairs = \case
        [] -> []
        x : xs -> zip (x : xs) xs
    mapColumns :: ([a] -> [b]) -> [[a]] -> [[b]]
    mapColumns f = transpose . zipWithN f
    zipWithN :: (Traversable t) => (t a -> b) -> t [a] -> [b]
    zipWithN f xs = getZipList $ f <$> traverse ZipList xs

areaChart :: Bool -> Double -> [Variable] -> Diagram B
areaChart animated w sirData =
    mwhen
        animated
        ( animate
            (TransformAnimation 15 Nothing $ TranslateAnimation [V2 0 0, V2 w 0])
            (rect 0.005 1)
        )
        <> hsep
            0.1
            [ named "chartInner" $ areaChartInner w sirData & centerY
            , key & alignL & centerY
            ]
  where
    key :: Diagram B
    key =
        vcat
            . map
                ( \Variable{name, colour} ->
                    (rect 0.15 0.15 & fc colour)
                        ||| ((text name & scale 0.1 & font "helvetica") <> (rect 0.8 0.15 & fc whitesmoke))
                )
            . reverse
            $ sirData
