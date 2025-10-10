{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
module Tests.Olog.OlogSpec where

import Test.Tasty
import Test.Tasty.HUnit

import Tests.PetriNet (exampleSIRODE)
import Math.Agate.Olog.Olog(Arc(..), Identity(..), Olog(..), makeOlog, MakeOlogError(..))

type MaybeOlog = Either (MakeOlogError Int) (Olog Int)

ologTests :: TestTree
ologTests =
    testGroup
        "Ologs basic properties"
        [ testGroup
            "basic identities"
            [ testCase "Transitions Correct" $
                assertBool "2 Transitions present" $
                    (2 :: Int) == ((1 + 1) :: Int)
            ],
            testGroup
            "further basic identities"
            [ testCase "Transitions Correct" $
                assertBool "2 Transitions present" $
                    (4 :: Int) == ((2 + 2) :: Int)
            ],
            testGroup "olog sanity checks" [
                testCase "arcs must have a source of a known dot" $
                    let badOlog :: MaybeOlog
                        badOlog =
                            makeOlog [1] [("source", 0, 1)] []
                    in badOlog @?= Left (UnknownSource "source" 0)
            ]
        ]
