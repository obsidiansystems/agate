module Diagrams.AreaChart (areaChart, lineChart, lineChartMulti, Variable (..)) where

import Control.Applicative
import Data.List
import Data.Maybe
import Data.Monoid.Extra
import Diagrams.Backend.SVG
import Diagrams.Prelude

data Variable = Variable
    { name :: String
    , colour :: Colour Double
    , values :: [Double]
    }

areaChartInner :: Double -> [Variable] -> Diagram B
areaChartInner overallWidth sirData =
    mconcat $
        zipWith
            ( \(bottoms, tops) Variable{colour} ->
                fromVertices
                    (zipWith (curry p2) [0, w ..] bottoms <> reverse (zipWith (curry p2) [0, w ..] tops))
                    & closeLine
                    & strokeLoop
                    & translate (maybe 0 (V2 0) $ listToMaybe bottoms)
                    & fc colour
                    & lcA transparent
            )
            -- accumulate y bounds from sums of preceding data points
            (map unzip $ mapColumns (adjacentPairs . scanl (+) 0) $ map (\Variable{values} -> values) sirData)
            sirData
  where
    w = overallWidth / maximum (map (genericLength . \Variable{values} -> values) sirData)
    adjacentPairs :: [a] -> [(a, a)]
    adjacentPairs = \case
        [] -> []
        x : xs -> zip (x : xs) xs
    mapColumns :: ([a] -> [b]) -> [[a]] -> [[b]]
    mapColumns f = transpose . zipWithN f
    zipWithN :: (Traversable t) => (t a -> b) -> t [a] -> [b]
    zipWithN f xs = getZipList $ f <$> traverse ZipList xs

areaChart :: Bool -> Double -> [Variable] -> Diagram B
areaChart animated w sirData =
    mwhen
        animated
        ( animate
            (TransformAnimation 15 Nothing $ TranslateAnimation [V2 0 0, V2 w 0])
            (rect 0.005 maxHeight)
        )
        <> hsep
            0.1
            [ named "chartInner" $ areaChartInner w sirData & centerY
            , areaKey sirData & alignL & centerY
            ]
  where
    maxHeight = maximum $ foldr1 (zipWith (+)) $ map (\Variable{values} -> values) sirData

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

lineChart :: Bool -> Double -> [Variable] -> Diagram B
lineChart animated w vars =
    mwhen
        animated
        ( animate
            (TransformAnimation 15 Nothing $ TranslateAnimation [V2 0 0, V2 w 0])
            (rect 0.005 maxHeight)
        )
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
    legendVars = head runs
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
            # lc (colour v)
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
            ( \Variable{name, colour} ->
                (hrule 0.15 # lc colour # lwL 0.02 # centerY)
                    ||| ((text name & scale 0.1 & font "helvetica") <> (rect 0.8 0.15 & lcA transparent & fc whitesmoke)) # centerY
            )
        . reverse
        $ vars
