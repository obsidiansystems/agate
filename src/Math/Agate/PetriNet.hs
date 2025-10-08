{-# LANGUAGE TupleSections #-}
module Math.Agate.PetriNet (PetriNet (..), AsODE (..), PetriNetImpl (..)) where

import Math.Agate.ODE
import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import Prelude hiding (id)

class Monoid net => PetriNet net where
  type Place net
  type Transition net
  -- | Express a basic building block Petri net that consists of a single transition with given input and output places.
  -- These can then be combined with the monoid instance to build larger nets.
  transition :: [Place net] -> Transition net -> [Place net] -> net

newtype AsODE system = AsODE { asODE :: system }
  deriving newtype (Semigroup, Monoid, Show)

instance forall system. ODESystem system => PetriNet (AsODE system) where
  type Place (AsODE system) = Var system
  type Transition (AsODE system) = Exp system
  transition inputs rate outputs = AsODE $
       mconcat [i += (- rate) * product (map (var @system) inputs) | i <- inputs]
    <> mconcat [o += rate * product (map (var @system) inputs)     | o <- outputs]

-- PETRI NET DEFINITION
type TransId  = Int
data PetriNetImpl p t = PetriNetImpl
  { numTransitions :: Int,
    transitions :: M.Map TransId t,
    places :: S.Set p,
    placeToTransitions :: M.Map (p, TransId) Int,
    transitionToPlaces :: M.Map (TransId, p) Int
  }
  deriving (Show)

instance (Ord p, Ord t) => Semigroup (PetriNetImpl p t) where
    p1 <> p2 = PetriNetImpl {
        numTransitions     = numP1 + numP2,
        transitions        = foldMap transitions         [p1', p2'],
        places             = foldMap places              [p1', p2'],
        placeToTransitions = foldMap placeToTransitions  [p1', p2'],
        transitionToPlaces = foldMap transitionToPlaces  [p1', p2']
    } where (numP1, numP2) = (numTransitions p1, numTransitions p2)
            p1' = p1
            p2' = p2 {
                transitions = (numP1 +) `M.mapKeys` transitions p2,
                placeToTransitions = (\(p, id) -> (p, id + numP1)) `M.mapKeys` placeToTransitions p2,
                transitionToPlaces = (\(id, p) -> (id + numP1, p)) `M.mapKeys` transitionToPlaces p2
            }

instance (Ord p, Ord t) => Monoid (PetriNetImpl p t) where
    mempty = PetriNetImpl 0 M.empty S.empty M.empty M.empty

instance (Ord p, Ord t) => PetriNet (PetriNetImpl p t) where
    type Place (PetriNetImpl p t) = p
    type Transition (PetriNetImpl p t) = t

    transition sources trans targets = PetriNetImpl {
        numTransitions = 1,
        places = S.fromList $ sources ++ targets,
        transitions = M.singleton 0 trans,
        placeToTransitions = M.fromListWith (+) [(pair, 1) | pair <- (,0) <$> sources],
        transitionToPlaces = M.fromListWith (+) [(pair, 1) | pair <- (0,) <$> targets]
    }
