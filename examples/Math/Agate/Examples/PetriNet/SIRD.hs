module Math.Agate.Examples.PetriNet.SIRD where

import Math.Agate.Diagrams.PetriNet
import Math.Agate.Examples.PetriNet.Colours qualified as Colours
import Math.Agate.PetriNet

data SIRDPlace
    = S
    | I
    | R
    | D
    deriving (Show, Eq, Ord, Enum, Bounded)
instance PetriPlace SIRDPlace where
    placeColour = \case
        S -> Colours.susceptible
        I -> Colours.infected
        R -> Colours.recovered
        D -> Colours.deceased
    placeName = \case
        S -> "susceptible"
        I -> "infected"
        R -> "recovered"
        D -> "deceased"

sird :: (Place net ~ SIRDPlace, Fractional (Transition net), PetriNet net) => net
sird =
    mconcat
        [ transition [I, S] transmission [I, I]
        , transition [I] recovery [R]
        , transition [I] mortality [D]
        ]
  where
    transmission = 0.4
    recovery = 0.03
    mortality = 0.01
