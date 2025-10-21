{-# LANGUAGE OverloadedStrings #-}

module Math.Agate.Examples.ODE.SIR where

import Data.Functor
import Data.Map (Map)
import Data.Map qualified as Map
import Math.Agate.Examples.PetriNet.SIR
import Math.Agate.ODE.Polynomial.Solver

runSolverSIR :: Map String [Double]
runSolverSIR =
    (\ls -> Map.fromList $ ["S", "I", "R"] <&> \v -> (v, map (Map.! v) ls)) $
        odeSolve exampleSIRODE (ODEParams 0.1) $
            Map.fromList [("S", 0.95), ("I", 0.05), ("R", 0)]
