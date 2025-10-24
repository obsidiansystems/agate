module Math.Agate.Examples.PetriNet.Malthusian where

import Math.Agate.Diagrams.PetriNet
import Diagrams.Prelude
import Math.Agate.PetriNet

data MalthusianPlace = N
    deriving (Show, Eq, Ord, Enum, Bounded)
instance PetriPlace MalthusianPlace where
    placeColour N = yellow
    placeName N = "population"

malthusian :: (Place net ~ MalthusianPlace, Fractional (Transition net), PetriNet net) => net
malthusian = mconcat
        [ transition [] alpha [N]
        , transition [N] mu []
        ]
  where
    alpha = 0.2
    mu = 0.1
