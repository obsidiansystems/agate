{-# LANGUAGE TupleSections #-}
module Tests.PetriNet where

import qualified Data.Map.Lazy as M
import Math.Agate.PetriNet
import Test.Tasty
import Test.Tasty.HUnit
import Math.Agate.ODE.Polynomial (PolynomialODE)

petriTests :: TestTree
petriTests =
    testGroup
        "Petri Nets Implementation"
        [ testGroup
            "SIR Model"
            [ testCase "Transitions Correct" $
                assert $
                    length (transitions exampleSIR) == 2
            ]
        ]

exampleSIR :: PetriNetImpl String Double
exampleSIR =  generalSIR id recovery transmission where
      recovery = 0.03
      transmission = 0.4


generalSIR :: (Fractional (Transition net), PetriNet net) => (String -> Place net) -> Transition net -> Transition net -> net
generalSIR place recovery transmission =
  mconcat
    [ transition [place "I", place "S"] transmission [place "I", place "I"]
    , transition [place "I"] recovery [place "R"]
    ]

exampleSIRODE :: PolynomialODE Double String
exampleSIRODE = asODE $ generalSIR id recovery transmission where
  recovery = 0.03
  transmission = 0.4


madridNet :: (Place net ~ String, Transition net ~ Double, PetriNet net) => net
madridNet =
  mconcat
    [ transition [s] 1 [t] <> transition [t] 1 [s]
      | (s, t) <- (("C",) <$> outer) ++ (zip outer (tail $ cycle outer))
      ]
  where
    outer = ["N", "E", "SE", "S", "W", "NW"]

-- exampleSIR :: (Fractional (Transition net), PetriNet net) => (String -> Place net) -> Transition net -> Transition net -> net
-- exampleSIR place recovery transmission =
--   mconcat
--     [ transition [place "I", place "S"] transmission [place "I", place "I"]
--     , transition [place "I"] recovery [place "R"]
--     ]
