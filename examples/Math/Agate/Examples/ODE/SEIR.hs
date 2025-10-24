module Math.Agate.Examples.ODE.SEIR where

import Data.Map (Map)
import Math.Agate.Examples.PetriNet.SEIR
import Math.Agate.ODE.Polynomial.Solver

runSolverSEIR :: [Map SEIRPlace Double]
runSolverSEIR = solvePetri seir \case
    S -> 0.95
    E -> 0
    I -> 0.05
    R -> 0
