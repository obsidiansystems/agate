{-# LANGUAGE BlockArguments #-}

module Math.Agate.PetriNetDiagram where

import Data.List
import Diagrams.Backend.SVG (renderSVG)
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude
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
    renderSVG
        "/tmp/out.svg"
        (mkSizeSpec (V2 (Just 1000) Nothing))
        $ sirDiagram 3 [red, blue, green]
        $ map (\(s, i, r) -> [i, s, r]) runSolverSIR
