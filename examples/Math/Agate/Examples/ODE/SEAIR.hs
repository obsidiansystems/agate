module Math.Agate.Examples.ODE.SEAIR where

import Data.Map (Map)
import Math.Agate.Examples.PetriNet.SEAIR
import Math.Agate.ODE.Solver

-- | SEAIR model taken from [this](https://arxiv.org/pdf/2206.03269) paper
runSolverSEAIR :: [Map SEAIRPlace Double]
runSolverSEAIR = solvePetri seair \case
    S -> 0.95
    E -> 0.05
    A -> 0
    I -> 0
    R -> 0
