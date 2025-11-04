module Math.Agate.Examples.ODE.SIRD where

import Data.Map (Map)
import Math.Agate.Examples.PetriNet.SIRD
import Math.Agate.ODE.Polynomial.Solver

-- | SIRD model taken from [this](https://arxiv.org/pdf/2206.03269) paper
runSolverSIRD :: [Map SIRDPlace Double]
runSolverSIRD = solvePetri sird \case
    S -> 0.95
    I -> 0.05
    R -> 0
    D -> 0
