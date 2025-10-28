module Math.Agate.Examples.PetriNet.Colours (
    susceptible,
    infected,
    deceased,
    recovered,
    exposed,
) where

import Data.Colour.RGBSpace (uncurryRGB)
import Data.Colour.RGBSpace.HSL (hsl)
import Diagrams.Prelude (Colour, sRGB, yellow)

susceptible :: Colour Double
susceptible = uncurryRGB sRGB $ hsl 240 0.7 0.4

exposed :: Colour Double
exposed = yellow

infected :: Colour Double
infected = uncurryRGB sRGB $ hsl 0 0.7 0.55

recovered :: Colour Double
recovered = uncurryRGB sRGB $ hsl 120 0.7 0.32

deceased :: Colour Double
deceased = uncurryRGB sRGB $ hsl 0 0 0.3
