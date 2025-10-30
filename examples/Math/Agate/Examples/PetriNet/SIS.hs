module Math.Agate.Examples.PetriNet.SIS where

import Math.Agate.Diagrams.PetriNet
import Math.Agate.PetriNet
import qualified Math.Agate.Examples.PetriNet.Colours as Colours


data SISPlace
    = S
    | I
    deriving (Show, Eq, Ord, Enum, Bounded)
instance PetriPlace SISPlace where
    placeColour = \case
        S -> Colours.susceptible
        I -> Colours.infected
    placeName = \case
        S -> "susceptible"
        I -> "infected"

sis :: (Place net ~ SISPlace, Fractional (Transition net), PetriNet net) => net
sis =
    mconcat
        [ transition [I, S] transmission [I, I]
        , transition [I] recovery [S]
        ]
  where
    transmission = 0.4
    recovery = 0.03
