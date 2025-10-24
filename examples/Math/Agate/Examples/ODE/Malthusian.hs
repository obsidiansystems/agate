module Math.Agate.Examples.ODE.Malthusian where
import Data.Map (Map)
import Math.Agate.Examples.PetriNet.Malthusian
import Math.Agate.ODE.Polynomial.Solver

runSolverMalthusian :: [Map MalthusianPlace Double]
runSolverMalthusian = solvePetri malthusian $ const 1
