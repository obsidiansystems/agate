module Main (main) where

import Test.Tasty
import Tests.PetriNet (petriTests)
import Tests.PetriNet.Chart (petriChartTest)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "Tests"
        [ petriChartTest,
          petriTests
        ]
