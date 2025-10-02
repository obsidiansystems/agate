module Math.Agate.PetriNetDiagram where

import Control.Applicative
import Data.List
import Diagrams.Backend.SVG
import Diagrams.Prelude

data Variable = Variable
    { name :: String
    , colour :: Colour Double
    , values :: [Double]
    }

sirDiagram :: Double -> [Variable] -> Diagram B
sirDiagram overallWidth sirData =
    mconcat
        $ map
            ( \((bottoms, tops), Variable{colour}) ->
                fromVertices
                    (zipWith (curry p2) [0, w ..] bottoms <> reverse (zipWith (curry p2) [0, w ..] tops))
                    & closeLine
                    & strokeLoop
                    & translate (V2 0 (head bottoms))
                    & fc colour
                    & lcA transparent
            )
        $ zip
            -- accumulate y bounds from sums of preceding data points
            (map unzip $ mapColumns (adjacentPairs . scanl (+) 0 . id) $ map (\Variable{values} -> values) sirData)
        $ sirData
  where
    w = overallWidth / maximum (map (genericLength . \Variable{values} -> values) sirData)
    adjacentPairs :: [a] -> [(a, a)]
    adjacentPairs xs = zip xs $ tail xs
    mapColumns :: ([a] -> [b]) -> [[a]] -> [[b]]
    mapColumns f = transpose . zipWithN \as -> f as
    zipWithN :: (Traversable t) => (t a -> b) -> t [a] -> [b]
    zipWithN f xs = getZipList $ f <$> traverse ZipList xs

sirDiagramDecorated :: Double -> [Variable] -> Diagram B
sirDiagramDecorated w sirData =
    hsep
        0.1
        [ sirDiagram w sirData & centerY
        , key & alignL & centerY
        ]
  where
    key :: Diagram B
    key =
        vcat
            . map
                ( \Variable{name, colour} ->
                    (rect 0.1 0.1 & fc colour)
                        ||| ((text name & scale 0.1) <> rect 1 0.1)
                )
            . reverse
            $ sirData
