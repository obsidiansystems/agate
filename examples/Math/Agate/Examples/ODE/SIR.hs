module Math.Agate.Examples.ODE.SIR where

import Data.Map (Map)
import Math.Agate.Examples.PetriNet.SIR
import Math.Agate.ODE.Polynomial.Solver

runSolverSIR :: [Map SIRPlace Double]
runSolverSIR = solvePetri sir \case
    S -> 0.95
    I -> 0.05
    R -> 0
