{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Tests.Olog.Olog2Spec where

import Test.Tasty
import Test.Tasty.HUnit

import Tests.PetriNet (exampleSIRODE)
import Math.Agate.Olog.Olog2(Arrow(..), (~>))
import Data.Either (isRight)

olog2Tests :: TestTree
olog2Tests =
    testCase "Can create arrows" $ do
        arrow.source @?= 1
        arrow.target @?= 2
        where
            arrow :: Arrow Int
            arrow = 1 ~> 2
            arrow2 :: Arrow Int
            arrow2 = Arrow 1 2

            -- arrow = Arrow @Int 1 2
        -- ((1 + 1) :: Int) @?= 2
