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
            ( \ds ->
                alignB
                    . vcat
                    . map (\(h, c) -> rect w h & fc c & lcA transparent)
                    $ zip ds colours
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
