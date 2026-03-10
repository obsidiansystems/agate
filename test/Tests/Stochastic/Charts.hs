{-# OPTIONS_GHC -Wno-type-defaults #-}

module Tests.Stochastic.Charts where

import Test.Tasty
import Test.Tasty.Golden

import Math.Agate.Diagrams.PetriNet (PetriPlace (..))
import Math.Agate.PetriNet (PetriNet (..))
import Math.Agate.Stochastic.MarkovKernel

import Math.Agate.Examples.PetriNet.SIR qualified as SIR
import Math.Agate.Examples.PetriNet.SIS qualified as SIS
import Math.Agate.Examples.PetriNet.SIRD qualified as SIRD
import Math.Agate.Examples.PetriNet.LotkaVolterra qualified as LV

import Diagrams.AreaChart
import TestUtils (diagToSVGBS)

import LazyPPL (runProb, randomTree)

import Data.Functor ((<&>))
import Data.List.Extra (enumerate)
import Data.Map.Lazy qualified as Map
import Data.Map.Monoidal (MonoidalMap, getMonoidalMap)
import Data.Map.Monoidal qualified as MMap
import Data.Monoid (Sum (..))
import System.Random (mkStdGen)

-- | Run a stochastic simulation for a given number of steps.
simulateStochastic :: Ord v
    => StateKernel Double v
    -> MonoidalMap v (Sum Double)
    -> Int -- ^ initial seed
    -> Int -- ^ number of steps
    -> [Map.Map v Double]
simulateStochastic sk state0 seed steps =
    map (fmap getSum . getMonoidalMap) . take (steps + 1)
    $ scanl (\st s -> runProb (runStateKernel sk st) (randomTree (mkStdGen s)))
            state0
            [seed ..]

-- | Build an initial state map from a function on places.
mkState :: (Enum v, Bounded v, Ord v) => (v -> Double) -> MonoidalMap v (Sum Double)
mkState f = MMap.fromList [(v, Sum (f v)) | v <- [minBound..maxBound]]

-- Stochastic Petri net models with rates scaled for discrete populations.
-- Bimolecular rates (e.g. transmission) are divided by total population N
-- to match the mass-action kinetics of the corresponding ODE model.

sirStochastic :: Double -> StateKernel Double SIR.SIRPlace
sirStochastic n = mconcat
    [ transition [SIR.I, SIR.S] (return (0.4 / n)) [SIR.I, SIR.I]
    , transition [SIR.I] (return 0.03) [SIR.R]
    ]

sisStochastic :: Double -> StateKernel Double SIS.SISPlace
sisStochastic n = mconcat
    [ transition [SIS.I, SIS.S] (return (0.4 / n)) [SIS.I, SIS.I]
    , transition [SIS.I] (return 0.03) [SIS.S]
    ]

sirdStochastic :: Double -> StateKernel Double SIRD.SIRDPlace
sirdStochastic n = mconcat
    [ transition [SIRD.I, SIRD.S] (return (0.4 / n)) [SIRD.I, SIRD.I]
    , transition [SIRD.I] (return 0.03) [SIRD.R]
    , transition [SIRD.I] (return 0.01) [SIRD.D]
    ]

lotkaVolterraStochastic :: Double -> StateKernel Double LV.LotkaVolterraPlace
lotkaVolterraStochastic n = mconcat
    [ transition [LV.Prey] (return 0.2) [LV.Prey, LV.Prey]
    , transition [LV.Prey, LV.Predator] (return (0.4 / n)) [LV.Predator, LV.Predator]
    , transition [LV.Predator] (return 0.2) []
    ]

stochasticChartTests :: TestTree
stochasticChartTests = testGroup "Stochastic Charts"
    [ chartTest "sir" (sirStochastic 1000)
        (mkState \case SIR.S -> 950; SIR.I -> 50; SIR.R -> 0)
    , chartTest "sis" (sisStochastic 1000)
        (mkState \case SIS.S -> 950; SIS.I -> 50)
    , chartTest "sird" (sirdStochastic 1000)
        (mkState \case SIRD.S -> 950; SIRD.I -> 50; SIRD.R -> 0; SIRD.D -> 0)
    , chartTestN "lotka-volterra" 200 (lotkaVolterraStochastic 10000)
        (mkState \case LV.Prey -> 5000; LV.Predator -> 5000)
    ]

chartTest :: (PetriPlace p, Bounded p, Enum p, Ord p)
    => FilePath
    -> StateKernel Double p
    -> MonoidalMap p (Sum Double)
    -> TestTree
chartTest name = chartTestN name 1000

numSeeds :: Int
numSeeds = 20

chartTestN :: (PetriPlace p, Bounded p, Enum p, Ord p)
    => FilePath
    -> Int
    -> StateKernel Double p
    -> MonoidalMap p (Sum Double)
    -> TestTree
chartTestN modelName numSteps kernel state0 =
    goldenVsString modelName
        ("test/outputs/stochastic/" ++ modelName ++ "/chart.svg")
    . pure . diagToSVGBS
    $ lineChartMulti 3 runs
  where
    totalPop = getSum . foldMap id $ state0
    runs =
        [ enumerate <&> \p ->
            Variable
                { name = placeName p
                , colour = placeColour p
                , values = take numSteps $ map ((/ totalPop) . (Map.! p)) solution
                }
        | seed <- [42 .. 42 + numSeeds - 1]
        , let solution = simulateStochastic kernel state0 seed numSteps
        ]
