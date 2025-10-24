module Main (main) where

import Test.Tasty
import Tests.ODE.Solver (odeSolverTests)
import Tests.PetriNet (petriTests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "Tests"
        [ odeSolverTests
        , petriTests
        ]
