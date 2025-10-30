module Math.Agate.Examples.ODE.SIS where

import Data.Map (Map)
import Math.Agate.Examples.PetriNet.SIS
import Math.Agate.ODE.Polynomial.Solver

runSolverSIS :: [Map SISPlace Double]
runSolverSIS = solvePetri sis \case
    S -> 0.95
    I -> 0.05
