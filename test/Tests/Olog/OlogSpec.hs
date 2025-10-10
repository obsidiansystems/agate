{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
module Tests.Olog.OlogSpec where

import Test.Tasty
import Test.Tasty.HUnit

ologTests :: TestTree
ologTests =
    testGroup
        "Ologs basic properties"
        [ testGroup
            "basic identities"
            [ testCase "Transitions Correct" $
                assertBool "2 Transitions present" $
                    (1 :: Int) == ((1 + 1) :: Int)
            ]
        ]
