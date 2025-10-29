module Math.Agate.Examples.ODE.SIWR where

import Data.Map (Map)
import Math.Agate.Examples.PetriNet.SIWR
import Math.Agate.ODE.Polynomial.Solver

runSolverSIWR :: [Map SIWRPlace Double]
runSolverSIWR = solvePetri siwr \case
    S -> 0.95
    I -> 0.05
    W -> 0
    R -> 0
