module Math.Agate.Examples.PetriNet.SIR where

import Math.Agate.Diagrams.PetriNet
import Math.Agate.PetriNet
import qualified Math.Agate.Examples.PetriNet.Colours as Colours

data SIRPlace
    = S
    | I
    | R
    deriving (Show, Eq, Ord, Enum, Bounded)
instance PetriPlace SIRPlace where
    placeColour = \case
        S -> Colours.susceptible
        I -> Colours.infected
        R -> Colours.recovered
    placeName = \case
        S -> "susceptible"
        I -> "infected"
        R -> "recovered"

sir :: (Place net ~ SIRPlace, Fractional (Transition net), PetriNet net) => net
sir =
    mconcat
        [ transition [I, S] transmission [I, I]
        , transition [I] recovery [R]
        ]
  where
    transmission = 0.4
    recovery = 0.03
