module Math.Agate.Examples.ODE.SIRS where

import Data.Map (Map)
import Math.Agate.Examples.PetriNet.SIRS
import Math.Agate.ODE.Polynomial.Solver

-- | SIRS model taken from [this](https://arxiv.org/pdf/2206.03269) paper
runSolverSIRS :: [Map SIRSPlace Double]
runSolverSIRS = solvePetri sirs \case
    S -> 0.95
    I -> 0.05
    R -> 0
