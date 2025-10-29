module Math.Agate.Examples.ODE.SIRS where

import Data.Map (Map)
import Math.Agate.Examples.PetriNet.SIRS
import Math.Agate.ODE.Polynomial.Solver

runSolverSIRS :: [Map SIRSPlace Double]
runSolverSIRS = solvePetri sirs \case
    S -> 0.95
    I -> 0.05
    R -> 0
