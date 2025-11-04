module Math.Agate.Examples.PetriNet.SCIR where

import Data.List.Extra (enumerate)
import Math.Agate.Diagrams.PetriNet
import Math.Agate.Examples.PetriNet.Colours qualified as Colours
import Math.Agate.PetriNet

data SCIRPlace
    = S
    | C
    | I
    | R
    deriving (Show, Eq, Ord, Enum, Bounded)
instance PetriPlace SCIRPlace where
    placeColour = \case
        S -> Colours.susceptible
        C -> Colours.carrier
        I -> Colours.infected
        R -> Colours.recovered
    placeName = \case
        S -> "susceptible"
        C -> "carrier"
        I -> "infected"
        R -> "recovered"

-- | SCIR model taken from [this](https://arxiv.org/pdf/2206.03269) paper
scir :: (Place net ~ SCIRPlace, Fractional (Transition net), PetriNet net) => net
scir =
    mconcat
        [ transition [] birth [S]
        , mconcat [transition [place] mortality [] | place <- enumerate]
        , transition [S, I] transmission [C, I]
        , transition [S, C] (q * transmission) [C, C]
        , transition [C] carrierToInfected [I]
        , transition [C] carrierToRecovered [R]
        , transition [I] recovery [R]
        , transition [I] transmission [C]
        , transition [R] resusceptible [S]
        ]
  where
    birth = 0.1
    mortality = 0.05
    transmission = 1
    recovery = 1
    q = 0.7
    carrierToInfected = 0.4
    carrierToRecovered = 0.4
    resusceptible = 1
