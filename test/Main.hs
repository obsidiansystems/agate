module Main (main) where

import Test.Tasty
import Tests.ODE.Solver (odeSolverTests)
import Tests.OgPoset.OgPosetSpec (ogPosetTests)
import Tests.PetriNet (petriTests)
import Tests.Stochastic.Charts (stochasticChartTests)
import Tests.Stochastic.Convergence (convergenceTests)
import Tests.Stochastic.MarkovKernel (stochasticTests)

main :: IO ()
main =
    defaultMain $
        testGroup
            "Tests"
            [ odeSolverTests
            , petriTests
            , stochasticTests
            , stochasticChartTests
            , convergenceTests
            , ogPosetTests
            ]
