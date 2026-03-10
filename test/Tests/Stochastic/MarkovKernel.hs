module Tests.Stochastic.MarkovKernel where

import Test.Tasty
import Test.Tasty.QuickCheck

import Math.Agate.PetriNet (PetriNet (..))
import Math.Agate.Stochastic.MarkovKernel

import Math.Agate.Examples.PetriNet.SIR qualified as SIR
import Math.Agate.Examples.PetriNet.SIS qualified as SIS
import Math.Agate.Examples.PetriNet.SIRD qualified as SIRD

import LazyPPL (runProb, randomTree)

import Data.Map.Monoidal (MonoidalMap)
import Data.Map.Monoidal qualified as Map
import Data.Monoid (Sum (..))
import System.Random (mkStdGen)

-- | Run a StateKernel one step with a deterministic seed.
runStep :: Ord v => StateKernel Double v -> MonoidalMap v (Sum Double) -> Int -> MonoidalMap v (Sum Double)
runStep sk state seed =
    runProb (runStateKernel sk state) (randomTree (mkStdGen seed))

-- | Run a StateKernel for n steps, returning all intermediate states.
runSteps :: Ord v => StateKernel Double v -> MonoidalMap v (Sum Double) -> Int -> Int -> [MonoidalMap v (Sum Double)]
runSteps sk state0 seed n =
    scanl (\st step -> runStep sk st (seed + step)) state0 [1..n]

-- | Build a state map from place-value pairs.
mkState :: Ord v => [(v, Double)] -> MonoidalMap v (Sum Double)
mkState = Map.fromList . map (fmap Sum)

-- | Total token count across all places.
totalPop :: MonoidalMap v (Sum Double) -> Double
totalPop = getSum . foldMap id

-- | Check that all token counts are non-negative.
allNonNeg :: MonoidalMap v (Sum Double) -> Bool
allNonNeg = all ((>= 0) . getSum)

stochasticTests :: TestTree
stochasticTests = testGroup "Stochastic"
    [ sirTests
    , sisTests
    , sirdTests
    , kernelTests
    ]

-- SIR model tests

sirTests :: TestTree
sirTests = testGroup "SIR"
    [ testProperty "conserves total population" $
        \(NonNegative s) (NonNegative i) (NonNegative r) seed ->
            let state0 = sirState s i r
                state1 = runStep sirNet state0 (seed :: Int)
            in totalPop state1 === totalPop state0
    , testProperty "maintains non-negative populations" $
        \(NonNegative s) (NonNegative i) (NonNegative r) seed ->
            let state0 = sirState s i r
                state1 = runStep sirNet state0 (seed :: Int)
            in counterexample (show state1) $ allNonNeg state1
    , testProperty "conserves population over multiple steps" $
        \(NonNegative s) (NonNegative i) (NonNegative r) seed (Positive n) ->
            let state0 = sirState s i r
                pop0 = totalPop state0
                states = runSteps sirNet state0 (seed :: Int) (min n 20)
            in conjoin
                [ counterexample ("step " ++ show k)
                    $ totalPop st === pop0
                | (k, st) <- zip [0 :: Int ..] states
                ]
    , testProperty "maintains non-negative populations over multiple steps" $
        \(NonNegative s) (NonNegative i) (NonNegative r) seed (Positive n) ->
            let state0 = sirState s i r
                states = runSteps sirNet state0 (seed :: Int) (min n 20)
            in conjoin
                [ counterexample ("step " ++ show k ++ ": " ++ show st)
                    $ property (allNonNeg st)
                | (k, st) <- zip [0 :: Int ..] states
                ]
    ]
  where
    sirNet = SIR.sir :: StateKernel Double SIR.SIRPlace
    sirState s i r = mkState
        [ (SIR.S, fromIntegral (s :: Int))
        , (SIR.I, fromIntegral i)
        , (SIR.R, fromIntegral r)
        ]

-- SIS model tests

sisTests :: TestTree
sisTests = testGroup "SIS"
    [ testProperty "conserves total population" $
        \(NonNegative s) (NonNegative i) seed ->
            let state0 = sisState s i
                state1 = runStep sisNet state0 (seed :: Int)
            in totalPop state1 === totalPop state0
    , testProperty "maintains non-negative populations" $
        \(NonNegative s) (NonNegative i) seed ->
            let state0 = sisState s i
                state1 = runStep sisNet state0 (seed :: Int)
            in counterexample (show state1) $ allNonNeg state1
    ]
  where
    sisNet = SIS.sis :: StateKernel Double SIS.SISPlace
    sisState s i = mkState
        [ (SIS.S, fromIntegral (s :: Int))
        , (SIS.I, fromIntegral i)
        ]

-- SIRD model tests

sirdTests :: TestTree
sirdTests = testGroup "SIRD"
    [ testProperty "conserves total population" $
        \(NonNegative s) (NonNegative i) (NonNegative r) (NonNegative d) seed ->
            let state0 = sirdState s i r d
                state1 = runStep sirdNet state0 (seed :: Int)
            in totalPop state1 === totalPop state0
    -- Non-negativity not guaranteed: recovery [I]→[R] and death [I]→[D]
    -- independently cap firings at I, so they can together over-consume I.
    ]
  where
    sirdNet = SIRD.sird :: StateKernel Double SIRD.SIRDPlace
    sirdState s i r d = mkState
        [ (SIRD.S, fromIntegral (s :: Int))
        , (SIRD.I, fromIntegral i)
        , (SIRD.R, fromIntegral r)
        , (SIRD.D, fromIntegral d)
        ]

-- General MarkovKernel tests

kernelTests :: TestTree
kernelTests = testGroup "MarkovKernel"
    [ testProperty "mempty is identity" $
        \(NonNegative s) (NonNegative i) (NonNegative r) seed ->
            let state0 = mkState
                    [ (SIR.S, fromIntegral (s :: Int))
                    , (SIR.I, fromIntegral i)
                    , (SIR.R, fromIntegral r)
                    ]
                state1 = runStep (mempty :: StateKernel Double SIR.SIRPlace) state0 (seed :: Int)
            in state1 === state0
    , testProperty "zero-rate transition preserves state" $
        \(NonNegative s) (NonNegative i) (NonNegative r) seed ->
            let zeroNet :: StateKernel Double SIR.SIRPlace
                zeroNet = transition [SIR.I, SIR.S] (return 0) [SIR.I, SIR.I]
                       <> transition [SIR.I] (return 0) [SIR.R]
                state0 = mkState
                    [ (SIR.S, fromIntegral (s :: Int))
                    , (SIR.I, fromIntegral i)
                    , (SIR.R, fromIntegral r)
                    ]
                state1 = runStep zeroNet state0 (seed :: Int)
            in state1 === state0
    ]
