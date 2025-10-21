module Math.Agate.Examples.PetriNet.SIR where

import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSL
import Diagrams.Prelude hiding (outer)
import Math.Agate.ODE.Polynomial (PolynomialODE)
import Math.Agate.PetriNet

generalSIR :: (Place net ~ String, Fractional (Transition net), PetriNet net) => net
generalSIR =
    mconcat
        [ transition ["I", "S"] transmission ["I", "I"]
        , transition ["I"] recovery ["R"]
        ]
  where
    transmission = 0.4
    recovery = 0.03

exampleSIRODE :: PolynomialODE Double String
exampleSIRODE = asODE generalSIR

sirColour :: String -> Colour Double
sirColour = \case
    "R" -> uncurryRGB sRGB $ hsl 120 0.7 0.32
    "I" -> uncurryRGB sRGB $ hsl 0 0.7 0.55
    "S" -> uncurryRGB sRGB $ hsl 240 0.7 0.4
    _ -> black
