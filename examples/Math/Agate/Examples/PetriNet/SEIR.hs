module Math.Agate.Examples.PetriNet.SEIR where

import Data.List.Extra (enumerate)
import Math.Agate.Diagrams.PetriNet
import Math.Agate.Examples.PetriNet.Colours qualified as Colours
import Math.Agate.PetriNet

data SEIRPlace
    = S
    | E
    | I
    | R
    deriving (Show, Eq, Ord, Enum, Bounded)
instance PetriPlace SEIRPlace where
    placeColour = \case
        S -> Colours.susceptible
        E -> Colours.exposed
        I -> Colours.infected
        R -> Colours.recovered
    placeName = \case
        S -> "susceptible"
        E -> "exposed"
        I -> "infected"
        R -> "recovered"

seir :: (Place net ~ SEIRPlace, Fractional (Transition net), PetriNet net) => net
seir =
    mconcat
        [ transition [] birth [S]
        , mconcat [transition [place] mortality [] | place <- enumerate]
        , transition [S, I] transmission [E, I]
        , transition [E] incubation [I]
        , transition [I] recovery [R]
        ]
  where
    birth = 0.1
    mortality = 0.05
    transmission = 1 -- 0.4
    recovery = 0.3
    incubation = 0.05
