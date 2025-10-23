module Math.Agate.Examples.PetriNet.SIRD where

import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSL
import Diagrams.Prelude hiding (outer)
import Math.Agate.Diagrams.PetriNet
import Math.Agate.PetriNet

data SIRDPlace
    = S
    | I
    | R
    | D
    deriving (Show, Eq, Ord, Enum, Bounded)
instance PetriPlace SIRDPlace where
    placeColour = \case
        S -> uncurryRGB sRGB $ hsl 240 0.7 0.4
        I -> uncurryRGB sRGB $ hsl 0 0.7 0.55
        R -> uncurryRGB sRGB $ hsl 120 0.7 0.32
        D -> uncurryRGB sRGB $ hsl 0 0 0.3
    placeName = \case
        S -> "susceptible"
        I -> "infected"
        R -> "recovered"
        D -> "deceased"

generalSIRD :: (Place net ~ SIRDPlace, Fractional (Transition net), PetriNet net) => net
generalSIRD =
    mconcat
        [ transition [I, S] transmission [I, I]
        , transition [I] recovery [R]
        , transition [I] mortality [D]
        ]
  where
    transmission = 0.4
    recovery = 0.03
    mortality = 0.01
