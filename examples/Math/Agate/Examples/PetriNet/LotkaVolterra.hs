module Math.Agate.Examples.PetriNet.LotkaVolterra where

import Data.Colour.RGBSpace (uncurryRGB)
import Data.Colour.RGBSpace.HSL (hsl)
import Diagrams.Prelude (sRGB)
import Math.Agate.Diagrams.PetriNet
import Math.Agate.PetriNet

data LotkaVolterraPlace
    = Prey
    | Predator
    deriving (Show, Eq, Ord, Enum, Bounded)

instance PetriPlace LotkaVolterraPlace where
    placeColour = \case
        Prey -> uncurryRGB sRGB $ hsl 120 0.7 0.4
        Predator -> uncurryRGB sRGB $ hsl 30 0.8 0.5
    placeName = \case
        Prey -> "prey"
        Predator -> "predator"

lotkaVolterra :: (Place net ~ LotkaVolterraPlace, Fractional (Transition net), PetriNet net) => net
lotkaVolterra =
    mconcat
        [ transition [Prey] preyBirth [Prey, Prey]
        , transition [Prey, Predator] predation [Predator, Predator]
        , transition [Predator] predatorDeath []
        ]
  where
    preyBirth = 0.2
    predation = 0.4
    predatorDeath = 0.2
