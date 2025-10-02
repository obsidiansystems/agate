module Math.Agate.PetriNetDiagram where

import Data.List
import Diagrams.Backend.SVG
import Diagrams.Prelude

sirDiagram :: Double -> [Colour Double] -> [[Double]] -> Diagram B
sirDiagram overallWidth colours sirData =
    mconcat
        $ map
            ( \(colour, (bottoms, tops)) ->
                fromVertices
                    (zipWith (curry p2) [0, w ..] bottoms <> reverse (zipWith (curry p2) [0, w ..] tops))
                    & closeLine
                    & strokeLoop
                    & translate (V2 0 (head bottoms))
                    & fc colour
                    & lcA transparent
            )
        $ zip (colours <> cycle [black, lightgrey])
        $ adjacentPairs
        $ transpose -- turns lists of all data at single timestep in to lists of all timestep values for single variable
        $ map (scanl (+) 0) -- accumulate y position from sums of preceding data points
        $ sirData
  where
    w = overallWidth / genericLength sirData
    adjacentPairs :: [a] -> [(a, a)]
    adjacentPairs xs = zip xs $ tail xs

sirDiagramDecorated :: Double -> [Colour Double] -> [String] -> [[Double]] -> Diagram B
sirDiagramDecorated w cs ns d =
  hsep
    0.1
    [ sirDiagram w cs d & centerY
    , key & alignL & centerY
    ]
 where
  key :: Diagram B
  key =
    vcat
      . map
        ( \(c, n) ->
            (rect 0.1 0.1 & fc c)
              ||| ((text n & scale 0.1) <> rect 1 0.1)
        )
      . reverse
      $ zip cs ns
