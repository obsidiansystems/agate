module Math.Agate.Examples.PetriNet.SCIR where

import Math.Agate.Diagrams.PetriNet
import Math.Agate.PetriNet
import qualified Math.Agate.Examples.PetriNet.Colours as Colours
import Data.List.Extra (enumerate)

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

scir :: (Place net ~ SCIRPlace, Fractional (Transition net), PetriNet net) => net
scir = 
  let
    births = transition [] birth [S]
    deaths = mconcat [ transition [place] mortality [] | place <- enumerate ]
    in
    mconcat
        [ births 
        , deaths
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
