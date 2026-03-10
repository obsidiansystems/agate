module Math.Agate.Examples.PetriNet.Colours (
    susceptible,
    infected,
    deceased,
    recovered,
    exposed,
    asymptomatic,
    carrier,
    water,
) where

import Data.Colour.RGBSpace (uncurryRGB)
import Data.Colour.RGBSpace.HSL (hsl)
import Diagrams.Prelude (Colour, sRGB)

susceptible :: Colour Double
susceptible = uncurryRGB sRGB $ hsl 240 0.7 0.4

exposed :: Colour Double
exposed = uncurryRGB sRGB $ hsl 60 0.7 0.5

asymptomatic :: Colour Double
asymptomatic = uncurryRGB sRGB $ hsl 277 0.7 0.4

carrier :: Colour Double
carrier = uncurryRGB sRGB $ hsl 277 0.7 0.4

infected :: Colour Double
infected = uncurryRGB sRGB $ hsl 0 0.7 0.55

recovered :: Colour Double
recovered = uncurryRGB sRGB $ hsl 120 0.7 0.32

water :: Colour Double
water = uncurryRGB sRGB $ hsl 207 0.7 0.39

deceased :: Colour Double
deceased = uncurryRGB sRGB $ hsl 0 0 0.3
