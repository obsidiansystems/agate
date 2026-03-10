module Diagrams.AreaChart (areaChart, lineChart, lineChartMulti, lineChartInner, Variable (..)) where

import Data.List (genericLength)
import Diagrams.Backend.SVG
import Diagrams.Prelude
import Utils

data Variable = Variable
    { name :: String
    , colour :: Colour Double
    , values :: [Double]
    , dashedLine :: Bool
    }

varLineStyle :: (HasStyle a, V a ~ V2, N a ~ Double) => Variable -> a -> a
varLineStyle Variable{colour, dashedLine} = lc colour . if dashedLine then dashingL [0.015, 0.008] 0 else id

lineChartInner :: Renderable (Path V2 Double) b => Double -> [Variable] -> QDiagram b V2 Double Any
lineChartInner aspectRatio = scaleToX aspectRatio . scaleToY 1 . mconcat . map (\v -> fromVertices (zipWith (curry p2) [0 ..] (values v)) & strokeLocLine & lw 5 & varLineStyle v)

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
            , areaKey vars & alignL & centerY
            ]

areaKey :: [Variable] -> Diagram B
areaKey vars =
    vcat
        . map
            ( \Variable{name, colour} ->
                (rect 0.15 0.15 & fc colour)
                    ||| ((text name & scale 0.1 & font "helvetica") <> (rect 0.8 0.15 & fc whitesmoke))
            )
        . reverse
        $ vars

lineChart :: Maybe Int -> Double -> [Variable] -> Diagram B
lineChart animated w vars =
    maybe
        mempty
        ( \animationLength ->
            animate
                (TransformAnimation animationLength Nothing $ TranslateAnimation [V2 0 0, V2 w 0])
                (rect 0.005 maxHeight)
        )
        animated
        <> hsep
            0.1
            [ named "chartInner" $ drawLines w 1 vars & centerY
            , lineKey vars & alignL & centerY
            ]
  where
    maxHeight = maximum [v | Variable{values} <- vars, v <- values]

-- | Animated multi-seed line chart. Overlays all seeds at the same position,
-- using TranslateAnimation to move each off-screen during inactive slots.
lineChartMulti :: Double -> [[Variable]] -> Diagram B
lineChartMulti w runs =
    hsep
        0.1
        [ named "chartInner" $ animatedOverlay & centerY
        , lineKey legendVars & alignL & centerY
        ]
  where
    n = length runs
    legendVars = (\(x:_) -> x) runs
    totalDuration = max 1 n -- 1 second per seed

    -- Use the first seed's envelope so off-screen seeds don't expand the bounding box
    animatedOverlay = withEnvelope (drawLines w 1 legendVars)
        $ mconcat
            [ animate (TransformAnimation totalDuration Nothing
                $ TranslateAnimation (seedKeyframes i))
                (drawLines w 1 vars)
            | (i, vars) <- zip [0 :: Int ..] runs
            ]

    -- Each seed is at V2 0 0 during its slot, far off-screen otherwise.
    -- Active windows overlap by 1 keyframe so the incoming seed is visible
    -- before the outgoing one departs, eliminating blink between transitions.
    holdFrames = 5 :: Int
    totalKeyframes = n * holdFrames
    offscreen = V2 0 100000
    seedKeyframes seedIdx =
        [ if isActive k then V2 0 0 else offscreen
        | k <- [0 .. totalKeyframes - 1]
        ]
      where
        slotStart = seedIdx * holdFrames
        isActive k
            -- First seed: normal window, plus last keyframe for seamless loop
            | seedIdx == 0 = k < holdFrames || k == totalKeyframes - 1
            -- Other seeds: start 1 keyframe early to overlap with previous
            | otherwise    = k >= slotStart - 1 && k < slotStart + holdFrames

drawLines :: Double -> Double -> [Variable] -> Diagram B
drawLines overallWidth alpha vars =
    mconcat
        [ fromVertices (zipWith (curry p2) [0, step ..] (values v))
            # strokeLocTrail
            # varLineStyle v
            # lwL 0.008
            # fcA transparent
            # opacity alpha
        | v <- vars
        ]
  where
    step = overallWidth / maximum [genericLength (values v) | v <- vars]

lineKey :: [Variable] -> Diagram B
lineKey vars =
    vcat
        . map
            ( \v ->
                (hrule 0.15 # varLineStyle v # lwL 0.02 # centerY)
                    ||| ((text (name v) & scale 0.1 & font "helvetica") <> (rect 0.8 0.15 & lcA transparent & fc whitesmoke)) # centerY
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
