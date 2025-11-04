module Math.Agate.Examples.ODE.SEIR where

import Data.Map (Map)
import Math.Agate.Examples.PetriNet.SEIR
import Math.Agate.ODE.Polynomial.Solver

-- | SEIR model taken from [this](https://arxiv.org/pdf/2206.03269) paper
runSolverSEIR :: [Map SEIRPlace Double]
runSolverSEIR = solvePetri seir \case
    S -> 0.95
    E -> 0.05
    I -> 0
    R -> 0
