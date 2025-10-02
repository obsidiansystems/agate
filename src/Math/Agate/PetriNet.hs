module Math.Agate.PetriNet where

import Math.Agate.ODESystem
import qualified Data.Map.Lazy as M
import qualified Data.Set as S

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
exampleSIRODE = asODE $ exampleSIR id recovery transmission where
  recovery = 0.03
  transmission = 0.4

-- PETRI NET DEFINITION
data Petri p t = Petri
  { transitions :: S.Set t,
    places :: S.Set p,
    placeToTransitions :: S.Set (p, t),
    transitionToPlaces :: S.Set (t, p)
  }
  deriving (Show)

instance (Ord p, Ord t) => Semigroup (Petri p t) where
    p1 <> p2 = Petri {
        transitions        = foldMap transitions        $ [p1, p2],
        places             = foldMap places             $ [p1, p2],
        placeToTransitions = foldMap placeToTransitions $ [p1, p2],
        transitionToPlaces = foldMap transitionToPlaces $ [p1, p2]
    }

instance (Ord p, Ord t) => Monoid (Petri p t) where
    mempty = Petri S.empty S.empty S.empty S.empty

instance (Ord p, Ord t) => PetriNet (Petri p t) where
    type Place (Petri p t) = p
    type Transition (Petri p t) = t

    transition sources trans targets = Petri {
        places = S.fromList $ sources ++ targets,
        transitions = S.singleton trans,
        placeToTransitions = S.fromList [(s, trans) | s <- sources],
        transitionToPlaces = S.fromList [(trans, t) | t <- targets]
    }

newtype PetriLabelling p = PetriLabelling (M.Map p Int) deriving (Show)

fireTransition :: (Ord p, Ord t) => t -> Petri p t -> PetriLabelling p -> PetriLabelling p
fireTransition t (Petri _ ps p2t t2p) (PetriLabelling l) = newLabelling
  where
    sourceDelta = [(p', -1) | (p', t') <- S.toList p2t, t' == t]
    targetDelta = [(p', 1) | (t', p') <- S.toList t2p, t' == t]
    base = [(p', l M.! p') | p' <- S.toList ps]
    newLabelling = PetriLabelling $ M.fromListWith (+) (base ++ sourceDelta ++ targetDelta)

-- Example: 
