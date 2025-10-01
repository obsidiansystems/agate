{-# LANGUAGE BlockArguments #-}

module Math.Agate.PetriNetDiagram where

import Data.List
import Diagrams.Backend.SVG (renderSVG)
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude
import Math.Agate.Examples.ODE (runSolverSIR)

sirDiagram :: Double -> (Colour Double, Colour Double, Colour Double) -> [(Double, Double, Double)] -> Diagram B
sirDiagram overallWidth (sColour, iColour, rColour) sirData =
    hcat $
        map
            ( \(s, i, r) ->
                alignB
                    . vcat
                    $ map
                        (\(h, c) -> rect w h & fc c & lcA transparent)
                        [ (i, iColour)
                        , (s, sColour)
                        , (r, rColour)
                        ]
            )
            sirData
  where
    w = overallWidth / genericLength sirData

test :: IO ()
test =
    renderSVG
        "/tmp/out.svg"
        (mkSizeSpec (V2 (Just 1000) Nothing))
        $ sirDiagram 3 (blue, red, green)
        $ runSolverSIR
