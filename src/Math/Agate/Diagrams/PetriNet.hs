{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

{- HLINT ignore "Use newtype instead of data" -}

module Math.Agate.Diagrams.PetriNet (layoutPetri, LayoutOpts (..), defaultLayoutOpts, DrawOpts (..), defaultDrawOpts, drawPetri, drawPetriDynamic) where

import Data.Fixed (E3, showFixed)
import Data.Graph.Inductive (Gr, Node)
import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import Data.Map.Lazy qualified as M
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Set qualified as Set
import Diagrams.Backend.SVG
import Diagrams.Prelude hiding (p2)
import Diagrams.TwoD.GraphViz
import Math.Agate.PetriNet (PetriNetImpl (..))
import Prelude hiding (id)

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
    , showPlace :: p -> String
    , showTransition :: t -> String
    }

defaultDrawOpts :: (Show p, Show t, Real t) => DrawOpts p t
defaultDrawOpts =
    DrawOpts
        { placeSize = 30
        , showPlace = show
        , showTransition = showFixed @E3 True . realToFrac
        }

layoutPetri ::
    (Ord p, Ord t, Show p, Show t) =>
    PetriNetImpl p t ->
    LayoutOpts ->
    IO (Gr (AttributeNode (Vertex p t)) (AttributeNode Int))
layoutPetri petri LayoutOpts{aspectRatio, command} = layoutGraph' params command $ mkGraph vertices edges
  where
    vertices =
        map (uncurry Transition) (M.toList t)
            ++ map Place (Set.toList $ places petri)
    edges =
        [(Place p, Transition id (t M.! id), w) | ((p, id), w) <- M.toList $ placeToTransitions petri]
            ++ [(Transition id (t M.! id), Place p, w) | ((id, p), w) <- M.toList $ transitionToPlaces petri]
    t = transitions petri
    params :: GraphvizParams Node (Vertex p t) Int () (Vertex p t)
    params =
        defaultParams
            { globalAttributes = [GraphAttrs [Ratio $ AspectRatio aspectRatio]]
            }

drawPetri ::
    (Show t, Show p, Ord t, Ord p) =>
    DrawOpts p t ->
    (p -> Colour Double) ->
    Gr (AttributeNode (Vertex p t)) (AttributeNode Int) ->
    Diagram B
drawPetri drawOpts vertexColour = drawPetri' drawOpts \p ->
    fc (vertexColour p) $ circle drawOpts.placeSize

drawPetriDynamic ::
    (Ord p, Ord t, Show p, Show t, Show t, Show p) =>
    DrawOpts p t ->
    (p -> Colour Double) ->
    (p -> [Double]) ->
    Gr (AttributeNode (Vertex p t)) (AttributeNode Int) ->
    Diagram B
drawPetriDynamic drawOpts vertexColour marking = drawPetri' drawOpts \p ->
    circle drawOpts.placeSize
        & lw 0
        & fc (vertexColour p)
        & animate (TransformAnimation 15 Nothing $ ScaleAnimation $ map ((\c -> V2 c c) . sqrt) $ marking p)

drawPetri' ::
    (Ord p, Show p, Ord t, Show t) =>
    DrawOpts p t ->
    (p -> Diagram B) ->
    Gr (AttributeNode (Vertex p t)) (AttributeNode Int) ->
    Diagram B
drawPetri' drawOpts renderPlace =
    drawGraph
        ( \v -> place $ case v of
            Place p ->
                mconcat
                    [ text (show p) & font fname & fontSizeL (drawOpts.placeSize * (2 / 3)) & fc black
                    , renderPlace p
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
    fname = "Helvetica" -- "Latin Modern Math"
    opts p w =
        with
            & gaps .~ local drawOpts.placeSize
            & arrowShaft .~ (unLoc . fromMaybe (error "arrow has no path") . listToMaybe $ pathTrails p)
            & headLength .~ local (0.5 * drawOpts.placeSize * fromIntegral w)
            & arrowHead .~ arrowheadThorn (150 @@ deg)
