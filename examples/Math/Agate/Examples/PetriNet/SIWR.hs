module Math.Agate.Examples.PetriNet.SIWR where

import Math.Agate.Diagrams.PetriNet
import Math.Agate.Examples.PetriNet.Colours qualified as Colours
import Math.Agate.PetriNet

data SIWRPlace
    = S
    | I
    | W
    | R
    deriving (Show, Eq, Ord, Enum, Bounded)
instance PetriPlace SIWRPlace where
    placeColour = \case
        S -> Colours.susceptible
        I -> Colours.infected
        W -> Colours.water
        R -> Colours.recovered
    placeName = \case
        S -> "susceptible"
        I -> "infected"
        W -> "water"
        R -> "recovered"

-- | SIWR model taken from [this](https://arxiv.org/pdf/2206.03269) paper
siwr :: (Place net ~ SIWRPlace, Fractional (Transition net), PetriNet net) => net
siwr =
    mconcat
        [ transition [] birth [S]
        , mconcat [transition [place] mortality [] | place <- [S, I, R]]
        , transition [W] decay []
        , transition [S, I] transmissionI [I, I]
        , transition [S, W] transmissionW [I]
        , transition [I] contamination [I, W]
        , transition [I] recovery [R]
        ]
  where
    birth = 0.05
    mortality = 0.1
    decay = 0.5
    transmissionI = 1
    transmissionW = 2
    recovery = 0.25
    contamination = 1.5
