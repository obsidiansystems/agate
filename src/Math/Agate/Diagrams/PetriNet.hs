{-# LANGUAGE UndecidableInstances #-}

module Math.Agate.Diagrams.PetriNet (layoutPetri, drawPetri, drawPetriDynamic) where

import Data.Graph.Inductive (Gr)
import Data.GraphViz (AttributeNode)
import Data.GraphViz.Commands
import Data.Map.Lazy qualified as M
import Data.Set qualified as Set
import Diagrams.Prelude hiding (p2)
import Diagrams.TwoD.GraphViz
import Diagrams.TwoD.Text qualified as DiagramsText
import Math.Agate.PetriNet (PetriNetImpl (..))
import Prelude hiding (id)
import Data.Maybe (listToMaybe, fromMaybe)

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
instance {-# OVERLAPPABLE #-} (Show a) => VertexShow a where vShow = show

layoutPetri :: (Ord p, Ord t, VertexShow p, VertexShow t) => PetriNetImpl p t -> GraphvizCommand -> IO (Gr (AttributeNode (Vertex p t)) (AttributeNode Int))
layoutPetri petri command = layoutGraph command $ mkGraph vertices edges
  where
    vertices =
        map (uncurry Transition) (M.toList t)
            ++ map Place (Set.toList $ places petri)
    edges =
        [(Place p, Transition id (t M.! id), w) | ((p, id), w) <- M.toList $ placeToTransitions petri]
        ++ [(Transition id (t M.! id), Place p, w) | ((id, p), w) <- M.toList $ transitionToPlaces petri]
    t = transitions petri

drawPetri ::
    ( V b ~ V2, N b ~ Double, Renderable (Path V2 Double) b, Renderable (DiagramsText.Text Double) b
    , Ord p, Ord t, VertexShow p, VertexShow t
    ) =>
    Gr (AttributeNode (Vertex p t)) (AttributeNode Int) -> Diagram b
drawPetri = drawPetri' vShow (const mempty)

drawPetriDynamic ::
    ( V b ~ V2, N b ~ Double, Renderable (Path V2 Double) b, Renderable (DiagramsText.Text Double) b
    , Ord p, Ord t, VertexShow p, VertexShow t, Show t, Show p
    ) =>
    (p -> Colour Double) -> Gr (AttributeNode (Vertex (p, Double) t)) (AttributeNode Int) -> Diagram b
drawPetriDynamic col = drawPetri' (vShow . fst) \(p, n) -> fc (col p) . circle $ n * 10

drawPetri' ::
    ( V b ~ V2, N b ~ Double, Renderable (Path V2 Double) b, Renderable (DiagramsText.Text Double) b
    , Ord p, Ord t, VertexShow t
    ) =>
    (p -> String) ->
    (p -> Diagram b) ->
    Gr (AttributeNode (Vertex p t)) (AttributeNode Int) ->
    Diagram b
drawPetri' showPlace renderPlace = drawGraph
        ( \v -> place $ case v of
            Place p -> fontSizeL 10 (fc black (text (showPlace p))) <> renderPlace p <> fc white (circle 10)
            Transition _ t -> fontSizeL 5 (fc white (text (vShow t))) <> fc black (square 10)
        )
        (\_ p1 _ p2 w p -> arrowBetween' (opts p w) p1 p2)
  where
    opts p w =
        with & gaps .~ local 10
            & arrowShaft .~ (unLoc . fromMaybe (error "arrow has no path") . listToMaybe $ pathTrails p)
            & headLength .~ local (5 * fromIntegral w)
