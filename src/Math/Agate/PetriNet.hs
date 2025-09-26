module Math.Agate.PetriNet where

import Math.Agate.ODESystem
import qualified Data.Map.Lazy as M
import Data.List

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
       mconcat [i += (- rate) * product (map (var @system) outputs) | i <- inputs]
    <> mconcat [o += rate * product (map (var @system) inputs)      | o <- outputs]

exampleSIR :: (Fractional (Transition net), PetriNet net) => (String -> Place net) -> Transition net -> Transition net -> net
exampleSIR place recovery transmission =
  mconcat
    [ transition [place "I", place "S"] transmission [place "I", place "I"]
    , transition [place "I"] recovery [place "R"]
    ]

exampleSIRODE :: PolynomialODE Double String
exampleSIRODE = asODE $ exampleSIR id 0.02 0.1

-- PETRI NET DEFINITION
data Petri p t = Petri
  { transitions :: [t],
    places :: [p],
    placeToTransitions :: [(p, t)],
    transitionToPlaces :: [(t, p)]
  }
  deriving (Show)

instance (Eq p, Eq t) => Semigroup (Petri p t) where
    p1 <> p2 = Petri {
        transitions        = nub . concatMap transitions        $ [p1, p2],
        places             = nub . concatMap places             $ [p1, p2],
        placeToTransitions = nub . concatMap placeToTransitions $ [p1, p2],
        transitionToPlaces = nub . concatMap transitionToPlaces $ [p1, p2]
    }

instance (Eq p, Eq t) => Monoid (Petri p t) where
    mempty = Petri [] [] [] []

instance (Eq p, Eq t) => PetriNet (Petri p t) where
    type Place (Petri p t) = p
    type Transition (Petri p t) = t

    transition sources trans targets = Petri {
        places = nub $ sources ++ targets,
        transitions = [trans],
        placeToTransitions = [(s, trans) | s <- sources],
        transitionToPlaces = [(trans, t) | t <- targets]
    }

newtype PetriLabelling p = PetriLabelling (M.Map p Int) deriving (Show)

fireTransition :: (Ord p, Ord t, Eq t) => t -> Petri p t -> PetriLabelling p -> PetriLabelling p
fireTransition t (Petri _ ps p2t t2p) (PetriLabelling l) = newLabelling
  where
    sourceDelta = [(p', -1) | (p', t') <- p2t, t' == t]
    targetDelta = [(p', 1) | (t', p') <- t2p, t' == t]
    base = [(p', l M.! p') | p' <- ps]
    newLabelling = PetriLabelling $ M.fromListWith (+) (base ++ sourceDelta ++ targetDelta)
