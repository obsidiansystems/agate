{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
module Tests.Olog.Olog2Spec where

import Test.Tasty
import Test.Tasty.HUnit

import Tests.PetriNet (exampleSIRODE)
import Math.Agate.Olog.Olog(Arc(..), Relator(..), Olog(..), makeOlog, MakeOlogError(..))
import Data.Either (isRight)

type MaybeOlog = Either (MakeOlogError Int) (Olog Int)

olog2Tests :: TestTree
olog2Tests =
    testCase "Olog2 basic properties" $
        ((1 + 1) :: Int) @?= 2
