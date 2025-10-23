module Math.Agate.Examples.PetriNet.SIR where

import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSL
import Diagrams.Prelude hiding (outer)
import Math.Agate.Diagrams.PetriNet
import Math.Agate.PetriNet

data SIRPlace
    = S
    | I
    | R
    deriving (Show, Eq, Ord, Enum, Bounded)
instance PetriPlace SIRPlace where
    placeColour = \case
        S -> uncurryRGB sRGB $ hsl 240 0.7 0.4
        I -> uncurryRGB sRGB $ hsl 0 0.7 0.55
        R -> uncurryRGB sRGB $ hsl 120 0.7 0.32
    placeName = \case
        S -> "susceptible"
        I -> "infected"
        R -> "recovered"

generalSIR :: (Place net ~ SIRPlace, Fractional (Transition net), PetriNet net) => net
generalSIR =
    mconcat
        [ transition [I, S] transmission [I, I]
        , transition [I] recovery [R]
        ]
  where
    transmission = 0.4
    recovery = 0.03
