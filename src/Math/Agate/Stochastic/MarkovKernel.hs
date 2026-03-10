{-# OPTIONS_GHC -Wno-orphans #-}
module Math.Agate.Stochastic.MarkovKernel (MarkovKernel (..), StateKernel (..), runStateKernel) where

import Math.Agate.PetriNet

import LazyPPL
import LazyPPL.Distributions

import Control.Monad
import Data.Monoid
import Data.Map.Monoidal qualified as Map
import Data.Map.Monoidal (MonoidalMap)

instance Num a => Num (Prob a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  (-) = liftA2 (-)
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance Fractional a => Fractional (Prob a) where
  (/) = liftA2 (/)
  fromRational = pure . fromRational

newtype MarkovKernel t = MarkovKernel { unMarkovKernel :: t -> Prob t }

instance Semigroup t => Semigroup (MarkovKernel t) where
  m1 <> m2 = MarkovKernel (\t -> liftA2 (<>) (unMarkovKernel m1 t) (unMarkovKernel m2 t))

instance Monoid t => Monoid (MarkovKernel t) where
  mempty = MarkovKernel (const (return mempty))

{- |
We assume that transitions occur at an expected rate
  lambda = r * i_1 * ... * i_n where
  r is a given rate parameter, and i_1,...,i_n are the values of available inputs
and so that the number of firings of the transition in a unit of time is Poisson distributed with rate parameter lambda.

However, there's an additional constraint that we shouldn't consume more of the inputs than is available, so the distribution
is adjusted so that if we might have consumed more, we instead perform the transition
-}
newtype StateKernel k v = StateKernel { unStateKernel :: MarkovKernel (MonoidalMap v (Sum k)) }
  deriving newtype (Semigroup, Monoid)

-- | Given a time step and a StateKernel, produce the Markov kernel describing the probability distribution of updates to the state map.
--timeStepStateKernel :: k -> StateKernel v k -> MarkovKernel (Map v k)

-- | Apply a StateKernel to a state, producing the new state by adding the combined delta.
runStateKernel :: Ord v => StateKernel Double v -> MonoidalMap v (Sum Double) -> Prob (MonoidalMap v (Sum Double))
runStateKernel sk state = Map.unionWith (<>) state <$> unMarkovKernel (unStateKernel sk) state

instance (Ord v, k ~ Double) => PetriNet (StateKernel k v) where
  type Place (StateKernel k v) = v
  type Transition (StateKernel k v) = Prob k
  transition inputs rate outputs = StateKernel . MarkovKernel $ \m -> do
    let curInputs = [getSum $ Map.findWithDefault 0 i m | i <- inputs]
        occurrences = Map.fromListWith (+) [(i, 1) | i <- inputs]
        maximumReaction = minimum [getSum (Map.findWithDefault 0 i m) / Map.findWithDefault 1 i occurrences | i <- inputs]
    r <- rate
    firings <- fmap (min maximumReaction . fromInteger) $ poisson (r * product curInputs)
    return $ Map.fromListWith (+) $ [(i, Sum (-firings)) | i <- inputs] <> [(o, Sum firings) | o <- outputs]
