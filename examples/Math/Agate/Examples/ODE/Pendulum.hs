module Math.Agate.Examples.ODE.Pendulum where

import Data.Map.Lazy qualified as Map
import Math.Agate.ODE
import Math.Agate.ODE.Polynomial.Solver
import Math.Agate.ODE.RealValued

data PendulumVar = Theta1 | Theta1' | Theta2 | Theta2'
    deriving (Show, Eq, Ord, Enum, Bounded)

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
    a1 = (m1 + m2) * l1
    b1 = m2 * l2 * (cos <$> delta)
    a2 = m2 * l2
    b2 = m2 * l1 * (cos <$> delta)
    f1 = (m1 * l2 * (var' Theta2' ^^ 2) * (sin <$> delta)) + ((m1 + m2) * g * (sin <$> var' Theta1))
    f2 = (m2 * g * (sin <$> var' Theta2)) - (m2 * l1 * (var' Theta1' ^^ 2) * (sin <$> delta))
    delta = var' Theta1 - var' Theta2
    m1 = 1
    m2 = 1
    g = 9.81
    l1 = 1
    l2 = 1

runSolverDoublePendulum :: [Map.Map PendulumVar Double]
runSolverDoublePendulum =
        odeSolve doublePendulumODE (ODEParams 0.001)
        $ Map.fromList
            [ (Theta1, 0.01) --pi / 4)
            , (Theta2, -0.02) --pi / 4)
            , (Theta1', 0.1)
            , (Theta2', 0)
            ]
