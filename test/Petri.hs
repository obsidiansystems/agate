module Petri where

import qualified Data.Map.Lazy as M
import Math.Agate.ODESystem
import Math.Agate.PetriNet
import Test.Tasty
import Test.Tasty.HUnit

petriTests :: TestTree
petriTests =
    testGroup
        "Petri Nets Implementation"
        [ testGroup
            "SIR Model"
            [ testCase "Transitions Correct" $
                assert $
                    transitions exampleSIRNet == M.fromList []
            ]
        ]

exampleSIRNet :: (Place net ~ String, Transition net ~ Double, PetriNet net) => net
exampleSIRNet =
    mconcat
        [ transition ["I", "S"] 0.1 ["I", "I"]
        , transition ["I"] 0.1 ["R"]
        ]

exampleSIRNetImpl :: Petri String Double
exampleSIRNetImpl = exampleSIRNet

-- exampleSIRODE :: PolynomialODE Double String
-- exampleSIRODE = asODE $ exampleSIR id recovery transmission where
--   recovery = 0.03
--   transmission = 0.4
