module Math.Agate.PetriNetDiagram where

import Data.ByteString.Lazy qualified as BS
import Data.List
import Diagrams.Backend.SVG
import Diagrams.Prelude
import Graphics.Svg (renderBS)
import Math.Agate.Examples.ODE (runSolverSIR)

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

test :: IO ()
test =
    BS.writeFile "/tmp/out.svg"
        . renderBS
        . renderDia SVG (SVGOptions (mkSizeSpec (V2 (Just 1000) Nothing)) Nothing mempty [] True)
        $ sirDiagram 3 [red, blue, green]
        $ map (\(s, i, r) -> [i, s, r]) runSolverSIR
