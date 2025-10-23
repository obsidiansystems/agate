module Math.Agate.Examples.ODE.SIRD where

import Data.Map (Map)
import Math.Agate.Examples.PetriNet.SIRD
import Math.Agate.ODE.Polynomial.Solver

runSolverSIRD :: [Map SIRDPlace Double]
runSolverSIRD = solvePetri generalSIRD \case
    S -> 0.95
    I -> 0.05
    R -> 0
    D -> 0
