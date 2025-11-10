module Main (main) where

import Test.Tasty
import Tests.ODE.Solver (odeSolverTests)
import Tests.OgPoset.OgPosetSpec (ogPosetTests)
import Tests.PetriNet (petriTests)

main :: IO ()
main =
    defaultMain $
        testGroup
            "Tests"
            [ odeSolverTests
            , petriTests
            , ogPosetTests
            ]
