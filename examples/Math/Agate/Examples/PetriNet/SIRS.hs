module Math.Agate.Examples.PetriNet.SIRS where

import Math.Agate.Diagrams.PetriNet
import Math.Agate.Examples.PetriNet.Colours qualified as Colours
import Math.Agate.PetriNet

data SIRSPlace
    = S
    | I
    | R
    deriving (Show, Eq, Ord, Enum, Bounded)
instance PetriPlace SIRSPlace where
    placeColour = \case
        S -> Colours.susceptible
        I -> Colours.infected
        R -> Colours.recovered
    placeName = \case
        S -> "susceptible"
        I -> "infected"
        R -> "recovered"

sirs :: (Place net ~ SIRSPlace, Fractional (Transition net), PetriNet net) => net
sirs =
    mconcat
        [ transition [I, S] transmission [I, I]
        , transition [I] recovery [R]
        , transition [R] susceptibility [S]
        ]
  where
    transmission = 0.4
    recovery = 0.03
    susceptibility = 0.1
