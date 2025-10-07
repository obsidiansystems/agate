module Math.Agate.Diagrams.PetriNet (layoutPetri, drawPetri) where

import Data.Graph.Inductive (Gr)
import Data.GraphViz (AttributeNode)
import Data.GraphViz.Commands
import Data.Map.Lazy qualified as M
import Data.Set qualified as Set
import Diagrams.Backend.SVG
import Diagrams.Prelude
import Diagrams.TwoD.GraphViz
import Math.Agate.PetriNet (PetriNetImpl (..))

data Vertex p t
    = Transition Int t
    | Place p
    deriving (Ord, Eq, Show)

-- testPetri1 :: PetriNetImpl String Double
-- testPetri1 = exampleSIR id 0.1 0.1

-- testPetri :: PetriNetImpl String Double
-- testPetri = madridNet

layoutPetri :: (Ord p, Ord t, Show p, Show t) => PetriNetImpl p t -> GraphvizCommand -> IO (Gr (AttributeNode (Vertex p t)) (AttributeNode Int))
layoutPetri petri command = layoutGraph command $ mkGraph vertices edges
  where
    vertices =
        map (\(id, w) -> Transition id w) (M.toList t)
            ++ map Place (Set.toList $ places petri)
    edges =
        [(Place p, Transition id (t M.! id), w) | ((p, id), w) <- M.toList $ placeToTransitions petri]
        ++ [(Transition id (t M.! id), Place p, w) | ((id, p), w) <- M.toList $ transitionToPlaces petri]
    t = transitions petri

drawPetri :: (Show p, Show t, Ord t, Ord p) => Gr (AttributeNode (Vertex p t)) (AttributeNode Int) -> Diagram B
drawPetri graph =
    drawGraph
        ( \v -> place $ case v of
            Place p -> fontSizeL 10 (fc black (text (show p))) <> fc white (circle 10)
            Transition _ t -> fontSizeL 5 (fc white (text (show t))) <> fc black (square 10)
        )
        (\_ p1 _ p2 w p -> arrowBetween' (opts p w) p1 p2)
        graph
  where
    opts p w =
        with & gaps .~ local 10
            & arrowShaft .~ (unLoc . head $ pathTrails p)
            & headLength .~ local (5 * fromIntegral w)

-- test = do
--     p <- layoutPetri testPetri Neato
--     writeDiag "out.svg" (drawPetri p)

-- writeDiag fName d =
--     BS.writeFile fName
--         . renderBS
--         $ renderDia SVG (SVGOptions (mkSizeSpec (V2 (Just 1000) Nothing)) Nothing mempty [] True) $
--             d
