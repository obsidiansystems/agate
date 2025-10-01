module Math.Agate.PetriNetDiagram where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Backend.SVG (renderSVG)

helloDiagram :: Diagram B
helloDiagram = text "hello again" <> circle 1

renderDiagram :: Diagram B -> IO ()
renderDiagram d = renderSVG "out.svg"
              (mkSizeSpec (V2 (Just 200) Nothing)) d

test = renderDiagram helloDiagram
