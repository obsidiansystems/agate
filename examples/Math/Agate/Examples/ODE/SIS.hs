module Math.Agate.Examples.ODE.SIS where

import Data.Map (Map)
import Math.Agate.Examples.PetriNet.SIS
import Math.Agate.ODE.Polynomial.Solver

-- | SIS model taken from [this](https://arxiv.org/pdf/2206.03269) paper
runSolverSIS :: [Map SISPlace Double]
runSolverSIS = solvePetri sis \case
    S -> 0.95
    I -> 0.05
