module Main (main, mainAcceptAll) where

import Test.Tasty
import Tests.PetriNet (petriTests)
import Tests.PetriNet.Chart (petriChartTest)
import Test.Tasty.Golden.Manage (AcceptTests(AcceptTests))

main :: IO ()
main = defaultMain tests

-- This is useful for regenerating outputs with GHCID.
mainAcceptAll :: IO ()
mainAcceptAll = defaultMain $ localOption (AcceptTests True) tests

tests :: TestTree
tests =
    testGroup
        "Tests"
        [ petriChartTest,
          petriTests
        ]
