module Main (main) where

import Test.Tasty
import Tests.ODE.Solver (odeSolverTests)
import Tests.OgPoset.OgPosetSpec (ogPosetTests)
import Tests.Olog.Olog2Spec (olog2Tests)
import Tests.Olog.OlogSpec (ologTests)
import Tests.PetriNet (petriTests)

main :: IO ()
main =
    defaultMain $
        testGroup
            "Tests"
            [ odeSolverTests
            , petriTests
            , ologTests
            , olog2Tests
            , ogPosetTests
            ]
