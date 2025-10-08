module Main (main, mainAcceptAll) where

import Test.Tasty
import Test.Tasty.Golden.Manage (AcceptTests(AcceptTests))

import Tests.PetriNet (petriTests)
import Tests.PetriNet.Chart (petriChartTest)
import Tests.ODE.Solver (odeSolverTests)

main :: IO ()
main = defaultMain tests

-- This is useful for regenerating outputs with GHCID.
mainAcceptAll :: IO ()
mainAcceptAll = defaultMain $ localOption (AcceptTests True) tests

tests :: TestTree
tests =
    testGroup
        "Tests"
        [
            odeSolverTests,
            petriChartTest,
            petriTests
        ]
