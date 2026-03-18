{-# OPTIONS_GHC -Wno-type-defaults #-}

module Tests.Stochastic.Convergence (convergenceTests) where

import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit

import Math.Agate.Diagrams.PetriNet (PetriPlace (..))
import Math.Agate.Stochastic.MarkovKernel (StateKernel)
import Tests.Stochastic.Charts
    ( simulateStochastic, independentGens, mkState
    , sirStochastic, sisStochastic, sirdStochastic, lotkaVolterraStochastic
    )

import Math.Agate.Examples.ODE.SIR (runSolverSIR)
import Math.Agate.Examples.ODE.SIS (runSolverSIS)
import Math.Agate.Examples.ODE.SIRD (runSolverSIRD)
import Math.Agate.Examples.ODE.LotkaVolterra (runSolverLotkaVolterra)
import Math.Agate.Examples.PetriNet.SIR qualified as SIR
import Math.Agate.Examples.PetriNet.SIS qualified as SIS
import Math.Agate.Examples.PetriNet.SIRD qualified as SIRD
import Math.Agate.Examples.PetriNet.LotkaVolterra qualified as LV

import Diagrams.AreaChart (Variable (..), lineChart)
import TestUtils (diagToSVGBS)

import Data.Functor ((<&>))
import Data.List (foldl1')
import Data.List.Extra (enumerate)
import Data.Map.Lazy qualified as Map
import Data.Map.Monoidal (MonoidalMap)
import Data.Monoid (Sum (..))
import System.Random (mkStdGen)

convergenceTests :: TestTree
convergenceTests = testGroup "Stochastic-ODE Convergence"
    [ convergenceTest "sir" True (sirStochastic 1000)
        (mkState \case SIR.S -> 950; SIR.I -> 50; SIR.R -> 0)
        runSolverSIR 1000 100
    , convergenceTest "sis" True (sisStochastic 1000)
        (mkState \case SIS.S -> 950; SIS.I -> 50)
        runSolverSIS 1000 100
    , convergenceTest "sird" True (sirdStochastic 1000)
        (mkState \case SIRD.S -> 950; SIRD.I -> 50; SIRD.R -> 0; SIRD.D -> 0)
        runSolverSIRD 1000 100
    , convergenceTest "lotka-volterra" False (lotkaVolterraStochastic 1000)
        (mkState \case LV.Prey -> 800; LV.Predator -> 200)
        runSolverLotkaVolterra 1000 50
    ]

numSeeds :: Int
numSeeds = 200

-- | Expected max deviation of the stochastic mean from the ODE solution.
--
-- From the diffusion approximation for density-dependent Markov chains:
-- single-run fluctuations at time t scale as √(t/N), averaging K runs
-- reduces by √K, and taking the max over T steps × P places inflates
-- by √(2 ln(TP)).
expectedMaxDeviation :: Int -> Double -> Int -> Double
expectedMaxDeviation numPlaces n steps =
    sqrt (fromIntegral steps / (n * fromIntegral numSeeds))
    * sqrt (2 * log (fromIntegral (steps * numPlaces)))

convergenceTest
    :: (PetriPlace p, Bounded p, Enum p, Ord p)
    => String
    -> Bool
    -> StateKernel Double p
    -> MonoidalMap p (Sum Double)
    -> [Map.Map p Double]
    -> Double
    -> Int
    -> TestTree
convergenceTest name checkDeviation kernel state0 odeSolution n numSteps = testGroup name
    $ [ testCase "mean converges to ODE" $
            assertBool ("Max deviation " ++ show dev ++ " exceeds " ++ show tol) (dev < tol)
      | checkDeviation
      ]
    ++ [ goldenVsString "convergence chart"
        ("test/outputs/stochastic/convergence-" ++ name ++ "/chart.svg")
        . pure . diagToSVGBS
        $ lineChart Nothing 3
        $  (enumerate <&> \p ->
                Variable
                    { name = placeName p ++ " (ODE)"
                    , colour = placeColour p
                    , values = map (Map.! p) (take numSteps odeSampled)
                    , dashedLine = True
                    })
        ++ (enumerate <&> \p ->
                Variable
                    { name = placeName p ++ " (mean)"
                    , colour = placeColour p
                    , values = map (Map.! p) (take numSteps mean)
                    , dashedLine = False
                    })
    ]
  where
    -- ODE solution sampled at matching time points (ODE step = 0.1, stochastic step = 1)
    odeSampled = [odeSolution !! (10 * j) | j <- [0 .. numSteps]]
    mean =
        map (Map.map (/ (n * fromIntegral numSeeds)))
        . foldl1' (zipWith (Map.unionWith (+)))
        $ [ simulateStochastic kernel state0 gen numSteps
          | gen <- take numSeeds (independentGens (mkStdGen 42))
          ]
    numPlaces = Map.size ((\(x:_) -> x) odeSampled)
    tol = 3 * expectedMaxDeviation numPlaces n numSteps
    dev = maximum
        [ abs (m Map.! p - o Map.! p)
        | (m, o) <- zip mean odeSampled
        , p <- enumerate
        ]
