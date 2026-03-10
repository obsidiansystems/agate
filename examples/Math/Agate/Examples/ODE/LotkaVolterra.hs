module Math.Agate.Examples.ODE.LotkaVolterra where

import Data.Map (Map)
import Math.Agate.Examples.PetriNet.LotkaVolterra
import Math.Agate.ODE.Polynomial.Solver

runSolverLotkaVolterra :: [Map LotkaVolterraPlace Double]
runSolverLotkaVolterra = solvePetri lotkaVolterra \case
    Prey -> 0.8
    Predator -> 0.2
