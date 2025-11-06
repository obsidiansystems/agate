module Main (main) where

import Test.Tasty
import Tests.ODE.Solver (odeSolverTests)
import Tests.PetriNet (petriTests)

import Tests.Olog.OlogSpec (ologTests)
import Tests.Olog.Olog2Spec (olog2Tests)
import Tests.OgPoset.OgPosetSpec (ogPosetTests)
-- import Tests.PetriNet.Chart (petriChartTest)

main :: IO ()
main =
    defaultMain $
        testGroup
            "Tests"
            [
                odeSolverTests,
                -- petriChartTest,
                petriTests,
                ologTests,
                olog2Tests,
                ogPosetTests
            ]
