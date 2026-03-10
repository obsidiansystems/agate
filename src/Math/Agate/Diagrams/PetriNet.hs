{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Math.Agate.Diagrams.PetriNet (
    PetriPlace (..),
    layoutPetri,
    LayoutOpts (..),
    defaultLayoutOpts,
    drawPetri,
    DrawOpts (..),
    defaultDrawOpts,
    layoutAndDrawPetri,
) where

import Data.Fixed (E3, showFixed)
import Data.Graph.Inductive (Gr, Node)
import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import Data.List.Extra (enumerate, transpose)
import Data.Map.Lazy qualified as M
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Set qualified as Set
import Diagrams.Backend.SVG
import Diagrams.Prelude hiding (p2)
import Diagrams.TwoD.GraphViz
import Math.Agate.PetriNet (PetriNetImpl (..))
import Prelude

class PetriPlace p where
    placeSymbol :: p -> String
    placeName :: p -> String
    placeColour :: p -> Colour Double
    default placeSymbol :: (Show p) => p -> String
    placeSymbol = show
    default placeName :: (Show p) => p -> String
    placeName = show
    default placeColour :: p -> Colour Double
    placeColour = const white

data Vertex p t
    = Transition Int t
    | Place p
    deriving (Ord, Eq, Show, Functor)
instance Bifunctor Vertex where
    bimap f g = \case
        Transition n t -> Transition n $ g t
        Place p -> Place $ f p

data LayoutOpts = LayoutOpts
    { aspectRatio :: Double
    , command :: GraphvizCommand
    }

defaultLayoutOpts :: LayoutOpts
defaultLayoutOpts =
    LayoutOpts
        { aspectRatio = 1
        , command = Neato
        }

data DrawOpts p t = DrawOpts
    { placeSize :: Double
    , fontName :: String
    , showPlace :: p -> String
    , showTransition :: t -> String
    , animation :: Maybe (p -> [Double], Int)
    }

defaultDrawOpts :: (Show p, Show t, Real t) => DrawOpts p t
defaultDrawOpts =
    DrawOpts
        { placeSize = 30
        , fontName = "Helvetica"
        , showPlace = show
        , showTransition = showFixed @E3 True . realToFrac
        , animation = Nothing
        }
layoutPetri ::
    (Ord p, Ord t, Show p, Show t) =>
    LayoutOpts ->
    PetriNetImpl p t ->
    IO (Gr (AttributeNode (Vertex p t)) (AttributeNode Int))
layoutPetri LayoutOpts{aspectRatio, command} petri = layoutGraph' params command $ mkGraph vertices edges
  where
    vertices =
        map (uncurry Transition) (M.toList t)
            ++ map Place (Set.toList $ places petri)
    edges =
        [(Place p, Transition i (t M.! i), w) | ((p, i), w) <- M.toList $ placeToTransitions petri]
            ++ [(Transition i (t M.! i), Place p, w) | ((i, p), w) <- M.toList $ transitionToPlaces petri]
    t = transitions petri
    params :: GraphvizParams Node (Vertex p t) Int () (Vertex p t)
    params =
        defaultParams
            { globalAttributes = [GraphAttrs [Ratio $ AspectRatio aspectRatio]]
            }

drawPetri ::
    (Bounded p, Enum p, Ord p, Show p, PetriPlace p, Ord t, Show t) =>
    DrawOpts p t ->
    Gr (AttributeNode (Vertex p t)) (AttributeNode Int) ->
    Diagram B
drawPetri drawOpts =
    drawGraph
        ( \v -> place $ case v of
            Place p ->
                mconcat
                    [ text (placeSymbol p) & font fname & fontSizeL (drawOpts.placeSize * (2 / 3)) & fc black
                    , circle drawOpts.placeSize & lw 0 & fc (placeColour p) & case drawOpts.animation of
                        Just (marking, animationLength) ->
                            animate . TransformAnimation animationLength Nothing . ScaleAnimation $
                                map (pure @V2 . sqrt) $
                                    normalise marking p
                        Nothing -> id
                    , circle drawOpts.placeSize & fc white
                    ]
            Transition _ t ->
                mconcat
                    [ text (drawOpts.showTransition t) & font fname & fontSizeL (drawOpts.placeSize * (1 / 2)) & fc white
                    , square (drawOpts.placeSize * 1.5) & fc black
                    ]
        )
        (\_ p1 _ p2 w p -> arrowBetween' (opts p w) p1 p2)
  where
    fname = drawOpts.fontName
    opts p w =
        with
            & gaps .~ local drawOpts.placeSize
            & arrowShaft .~ (unLoc . fromMaybe (error "arrow has no path") . listToMaybe $ pathTrails p)
            & headLength .~ local (0.5 * drawOpts.placeSize * fromIntegral w)
            & arrowHead .~ arrowheadThorn (150 @@ deg)
    normalise :: (Enum p, Bounded p) => (p -> [Double]) -> p -> [Double]
    normalise marking p = zipWith (/) (marking p) (map sum . transpose . fmap marking $ enumerate)

layoutAndDrawPetri ::
    (Bounded p, Enum p, Ord p, Show p, PetriPlace p, Ord t, Show t) =>
    LayoutOpts ->
    DrawOpts p t ->
    PetriNetImpl p t ->
    IO (Diagram B)
layoutAndDrawPetri layoutOpts drawOpts model = drawPetri drawOpts <$> layoutPetri layoutOpts model
