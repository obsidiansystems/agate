module Math.Agate.PetriNetDiagram where

import Data.List
import Diagrams.Backend.SVG
import Diagrams.Prelude

sirDiagram :: Double -> [Colour Double] -> [[Double]] -> Diagram B
sirDiagram overallWidth colours sirData =
    hcat $
        map
            ( alignB
                . vcat
                . map (\(c, h) -> rect w h & fc c & lcA transparent)
                . zip (colours <> cycle [black, lightgrey])
            )
            sirData
  where
    w = overallWidth / genericLength sirData
