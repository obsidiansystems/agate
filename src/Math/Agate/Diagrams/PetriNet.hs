{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Math.Agate.Diagrams.PetriNet (layoutPetri, drawPetri, drawPetriDynamic) where

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

class VertexShow a where
    vShow :: a -> String

instance {-# OVERLAPPING #-} VertexShow String where vShow s = s
instance {-# OVERLAPPING #-} VertexShow Double where
    vShow = showFixed @E3 True . realToFrac

instance {-# OVERLAPPABLE #-} (Show a) => VertexShow a where vShow = show

layoutPetri ::
    (Ord p, Ord t, VertexShow p, VertexShow t) =>
    PetriNetImpl p t ->
    Double ->
    GraphvizCommand ->
    IO (Gr (AttributeNode (Vertex p t)) (AttributeNode Int))
layoutPetri petri aspectRatio command = layoutGraph' params command $ mkGraph vertices edges
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
    (VertexShow t, VertexShow p, Ord t, Ord p) =>
    (p -> Colour Double) ->
    Gr (AttributeNode (Vertex p t)) (AttributeNode Int) ->
    Diagram B
drawPetri vertexColour = drawPetri' vShow \p ->
    fc (vertexColour p) $ circle 30

drawPetriDynamic ::
    (Ord p, Ord t, VertexShow p, VertexShow t, Show t, Show p) =>
    (p -> Colour Double) -> (p -> [Double]) -> Gr (AttributeNode (Vertex p t)) (AttributeNode Int) -> Diagram B
drawPetriDynamic vertexColour marking = drawPetri' vShow \p ->
    circle 30
        & lw 0
        & fc (vertexColour p)
        & animate (TransformAnimation 15 Nothing $ ScaleAnimation $ map (\c -> V2 c c) $ marking p)

drawPetri' ::
    (Ord p, Ord t, VertexShow t) =>
    (p -> String) ->
    (p -> Diagram B) ->
    Gr (AttributeNode (Vertex p t)) (AttributeNode Int) ->
    Diagram B
drawPetri' showPlace renderPlace =
    drawGraph
        ( \v -> place $ case v of
            Place p ->
                mconcat
                    [ text (showPlace p) & font fname & fontSizeL 20 & fc black
                    , renderPlace p
                    , circle 30 & fc white
                    ]
            Transition _ t ->
                mconcat
                    [ text (vShow t) & font fname & fontSizeL 10 & fc white
                    , square 45 & fc black
                    ]
        )
        (\_ p1 _ p2 w p -> arrowBetween' (opts p w) p1 p2)
  where
    fname = "Helvetica" -- "Latin Modern Math"
    opts p w =
        with
            & gaps .~ local 30
            & arrowShaft .~ (unLoc . fromMaybe (error "arrow has no path") . listToMaybe $ pathTrails p)
            & headLength .~ local (15 * fromIntegral w)
            & arrowHead .~ arrowheadThorn (150 @@ deg)
