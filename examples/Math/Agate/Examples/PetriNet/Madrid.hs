{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module Math.Agate.Examples.PetriNet.Madrid (madridNet) where

import Data.List.NonEmpty qualified as NE
import Math.Agate.PetriNet

madridNet :: (Place net ~ String, Transition net ~ Double, PetriNet net) => net
madridNet =
    mconcat
        [ transition [s] 1 [t] <> transition [t] 1 [s]
        | (s, t) <- (("C",) <$> outer) ++ zip outer (NE.tail (NE.fromList (cycle outer)))
        ]
  where
    outer = ["N", "E", "SE", "S", "W", "NW"]
