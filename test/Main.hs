module Main (main, mainAcceptAll) where

import Test.Tasty
import Test.Tasty.Golden.Manage (AcceptTests (AcceptTests))

import Test.Tasty.Ingredients.ConsoleReporter (UseColor (Always))
import Tests.ODE.Solver (odeSolverTests)
import Tests.PetriNet (petriTests)

main :: IO ()
main = defaultMain tests

-- This is useful for regenerating outputs with GHCID.
mainAcceptAll :: IO ()
mainAcceptAll =
    defaultMain $
        localOption (AcceptTests True) $
            localOption
                (Always :: UseColor)
                tests

tests :: TestTree
tests =
    testGroup
        "Tests"
        [ odeSolverTests
        , petriTests
        ]
