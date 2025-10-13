{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
module Tests.Olog.Olog2Spec where

import Test.Tasty
import Test.Tasty.HUnit

import Tests.PetriNet (exampleSIRODE)
import Math.Agate.Olog.Olog2(Arrow(..), (~>))
import Data.Either (isRight)

olog2Tests :: TestTree
olog2Tests =
    testCase "Can create arrows" $ do
        source arrow @?= 1
        target arrow @?= 2
        where
            arrow :: Arrow Int
            arrow = 1 ~> 2
            -- arrow = Arrow @Int 1 2
        -- ((1 + 1) :: Int) @?= 2
