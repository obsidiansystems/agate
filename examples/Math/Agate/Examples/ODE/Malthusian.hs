module Math.Agate.Examples.ODE.Malthusian where

import Data.Map (Map)
import Math.Agate.Examples.PetriNet.Malthusian
import Math.Agate.ODE.Solver

runSolverMalthusian :: [Map MalthusianPlace Double]
runSolverMalthusian = solvePetri malthusian $ const 1
