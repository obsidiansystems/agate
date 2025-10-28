module Math.Agate.Examples.PetriNet.SEAIR where

import Math.Agate.Diagrams.PetriNet
import Math.Agate.PetriNet
import qualified Math.Agate.Examples.PetriNet.Colours as Colours
import Data.List.Extra (enumerate)

data SEAIRPlace
    = S
    | E
    | A
    | I
    | R
    deriving (Show, Eq, Ord, Enum, Bounded)
instance PetriPlace SEAIRPlace where
    placeColour = \case
        S -> Colours.susceptible
        E -> Colours.exposed
        A -> Colours.asymptomatic
        I -> Colours.infected
        R -> Colours.recovered
    placeName = \case
        S -> "susceptible"
        E -> "exposed"
        A -> "asymptomatic"
        I -> "infected"
        R -> "recovered"

seair :: (Place net ~ SEAIRPlace, Fractional (Transition net), PetriNet net) => net
seair = 
  let
    births = transition [] birth [S]
    deaths = mconcat [ transition [place] mortality [] | place <- enumerate ]
    in
    mconcat
        [ births 
        , deaths
        , transition [S, I] transmission [E, I]
        , transition [S, A] (q * transmission) [E, A]
        , transition [E] (p * incubation) [I]
        , transition [E] ((1 - p) * incubation) [A]
        , transition [I] recoveryI [R]
        , transition [A] recoveryA [R]
        ] 
  where
    birth = 0.1
    mortality = 0.05
    transmission = 1
    p = 0.5
    q = 0.7
    recoveryI = 0.3
    recoveryA = 0.2
    incubation = 0.05
