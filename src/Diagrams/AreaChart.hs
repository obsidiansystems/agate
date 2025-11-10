module Diagrams.AreaChart (areaChart, Variable (..)) where

import Diagrams.Backend.SVG
import Diagrams.Prelude
import Utils

data Variable = Variable
    { name :: String
    , colour :: Colour Double
    , values :: [Double]
    }

areaChartInner :: Double -> [Variable] -> Diagram B
areaChartInner aspectRatio vars =
    scaleToX aspectRatio . scaleToY 1 . mconcat $
        zipWith
            ( \(bottom, top) Variable{colour} ->
                (bottom `catLocTrails` reverseLocLine top)
                    & mapLoc closeLine
                    & strokeLocLoop
                    & fc colour
                    & lcA transparent
            )
            (adjacentPairs boundaries)
            vars
  where
    boundaries =
        map (fromVertices . zipWith (curry p2) [0 ..])
            . mapColumns (scanl (+) 0)
            . map (\Variable{values} -> values)
            $ vars

areaChart :: Maybe Int -> Double -> [Variable] -> Diagram B
areaChart animated w vars =
    maybe
        mempty
        ( \animationLength ->
            animate
                (TransformAnimation animationLength Nothing $ TranslateAnimation [V2 0 0, V2 w 0])
                (rect 0.005 1)
        )
        animated
        <> hsep
            0.1
            [ named "chartInner" $ areaChartInner w vars & centerY
            , key & alignL & centerY
            ]
  where
    key =
        vcat
            . map
                ( \Variable{name, colour} ->
                    (rect 0.15 0.15 & fc colour)
                        ||| ((text name & scale 0.1 & font "helvetica") <> (rect 0.8 0.15 & fc whitesmoke))
                )
            . reverse
            $ vars

-- see https://github.com/diagrams/diagrams-lib/pull/374
catLocTrails ::
    Located (Trail' Line V2 Double) ->
    Located (Trail' Line V2 Double) ->
    Located (Trail' Line V2 Double)
catLocTrails a@(Loc aLoc aLine) b@(Loc _ bLine) =
    Loc aLoc (aLine <> lineFromOffsets [atStart b .-. atEnd a] <> bLine)
