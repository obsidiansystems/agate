module Math.Agate.Examples.ODE.Pendulum where

import Data.Map.Lazy qualified as Map
import Math.Agate.ODE
import Math.Agate.ODE.Solver
import Math.Agate.ODE.RealValued
import Math.Agate.Diagrams.PetriNet (PetriPlace (..))
import Diagrams.Prelude (yellow, gold, pink, purple)

data PendulumVar = Theta1 | Theta1' | Theta2 | Theta2'
    deriving (Show, Eq, Ord, Enum, Bounded)

instance PetriPlace PendulumVar where
    placeSymbol = \case
        Theta1 -> "θ_1"
        Theta1' -> "dθ_1/dt"
        Theta2 -> "θ_2"
        Theta2' -> "dθ_2/dt"
    placeColour = \case
        Theta1 -> yellow
        Theta1' -> gold
        Theta2 -> pink
        Theta2' -> purple

-- | ODE system governing the mechanics of a double pendulum.
-- For details on the derivation, see [here](https://en.wikipedia.org/wiki/Double_pendulum)
-- Equations 1 and 2 are used and then the ODE is linearised via the subsitution u = Theta1', v = Theta2'
doublePendulumODE :: RealValuedODE PendulumVar Double
doublePendulumODE =
    mconcat
        [ Theta1 += var' Theta1'
        , Theta2 += var' Theta2'
        , Theta1' += (b2 * f1 - b1 * f2) * (1 / k)
        , Theta2' += (a1 * f2 - a2 * f1) * (1 / k)
        ]
  where
    var' = var @(RealValuedODE PendulumVar Double)
    k = a1 * b2 - a2 * b1
    a1 = (4 / 3) * l
    b1 = (1 / 2) * l * (cos <$> delta)
    a2 = (1 / 2) * l * (cos <$> delta)
    b2 = (1 / 3) * l
    f1 = (1 / 2) * l * (var' Theta2' ^^ (2 :: Integer)) * (sin <$> delta) + (3 / 2) * g * (sin <$> var' Theta1)
    f2 = -((1 / 2) * (var' Theta1' ^^ (2 :: Integer)) * (sin <$> delta)) + (1 / 2) * g * (sin <$> var' Theta2)
    delta = var' Theta1 - var' Theta2
    g = 9.81
    l = 1

runSolverDoublePendulum :: [Map.Map PendulumVar Double]
runSolverDoublePendulum =
    odeSolve doublePendulumODE (ODEParams 0.001) $
        Map.fromList
            [ (Theta1, 0.01) -- pi / 4)
            , (Theta2, -0.02) -- pi / 4)
            , (Theta1', 0.1)
            , (Theta2', 0)
            ]
