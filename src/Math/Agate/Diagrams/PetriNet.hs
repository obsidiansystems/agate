{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Math.Agate.Diagrams.PetriNet (layoutPetri, drawPetri, drawPetriDynamic) where

import Data.Graph.Inductive (Gr)
import Data.GraphViz (AttributeNode)
import Data.GraphViz.Commands
import Data.Map.Lazy qualified as M
import Data.Set qualified as Set
import Diagrams.Prelude hiding (p2)
import Diagrams.TwoD.GraphViz
import Diagrams.Backend.SVG
import Math.Agate.PetriNet (PetriNetImpl (..))
import Prelude hiding (id)
import Data.Maybe (listToMaybe, fromMaybe)
import qualified Graphics.Svg as SVG
import qualified Data.Text as T
import Data.Fixed (showFixed, E3)

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
    ( Ord p, Ord t, VertexShow p, VertexShow t
    ) =>
    Gr (AttributeNode (Vertex p t)) (AttributeNode Int) -> Diagram B
drawPetri = drawPetri' vShow (const mempty)

drawPetriDynamic ::
    ( Ord p, Ord t, VertexShow p, VertexShow t, Show t, Show p
    ) =>
    (p -> Colour Double) -> (p -> [Double]) -> Gr (AttributeNode (Vertex p t)) (AttributeNode Int) -> Diagram B
drawPetriDynamic vertexColour marking = drawPetri' vShow \p -> elementToDiagram $
        SVG.circle_
            [ SVG.Stroke_width_ SVG.<<- "0"
            , SVG.Fill_ SVG.<<- T.pack (sRGB24show $ vertexColour p)
            ]
            $ SVG.animate_
                [ SVG.AttributeName_ SVG.<<- "r"
                , SVG.Values_ SVG.<<- T.intercalate ";"
                    (map (T.pack . showFixed @E3 True . realToFrac . (* 10)) $ marking p)
                , SVG.Dur_ SVG.<<- "15s"
                , SVG.RepeatCount_ SVG.<<- "indefinite"
                ]

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
            [ text (showPlace p) & font fname & fontSizeL 10 & fc black,
              renderPlace p,
              circle 10 & fc white
            ]
        Transition _ t ->
          mconcat
            [ text (vShow t) & font fname & fontSizeL 5 & fc white,
              square 15 & fc black
            ]
    )
    (\_ p1 _ p2 w p -> arrowBetween' (opts p w) p1 p2)
  where
    fname = "Latin Modern Math"
    opts p w =
      with
        & gaps .~ local 15
        & arrowShaft .~ (unLoc . fromMaybe (error "arrow has no path") . listToMaybe $ pathTrails p)
        & headLength .~ local (5 * fromIntegral w)
        & arrowHead .~ arrowheadThorn (150 @@ deg)
