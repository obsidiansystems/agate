module Diagrams.AreaChart (areaChart, Variable (..)) where

import Control.Applicative
import Data.List
import Data.Maybe
import Diagrams.Backend.SVG
import Diagrams.Prelude

data Variable = Variable
    { name :: String
    , colour :: Colour Double
    , values :: [Double]
    }

areaChartInner :: Double -> [Variable] -> Diagram B
areaChartInner overallWidth sirData =
    mconcat $
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
            -- accumulate y bounds from sums of preceding data points
            (map unzip $ mapColumns (adjacentPairs . scanl (+) 0) $ map (\Variable{values} -> values) sirData)
            sirData
  where
    w = overallWidth / maximum (map (genericLength . \Variable{values} -> values) sirData)
    adjacentPairs :: [a] -> [(a, a)]
    adjacentPairs = \case
        [] -> []
        x : xs -> zip (x : xs) xs
    mapColumns :: ([a] -> [b]) -> [[a]] -> [[b]]
    mapColumns f = transpose . zipWithN f
    zipWithN :: (Traversable t) => (t a -> b) -> t [a] -> [b]
    zipWithN f xs = getZipList $ f <$> traverse ZipList xs

areaChart :: Double -> [Variable] -> Diagram B
areaChart w sirData =
    hsep
        0.1
        [ areaChartInner w sirData & centerY
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
