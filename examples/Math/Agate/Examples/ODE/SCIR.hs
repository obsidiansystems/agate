module Math.Agate.Examples.ODE.SCIR where

import Data.Map (Map)
import Math.Agate.Examples.PetriNet.SCIR
import Math.Agate.ODE.Solver

runSolverSCIR :: [Map SCIRPlace Double]
runSolverSCIR = solvePetri scir \case
    S -> 0.95
    C -> 0.05
    I -> 0
    R -> 0
