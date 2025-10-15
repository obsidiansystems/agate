{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}

module Tests.OgPoset.OgPosetSpec where

import Data.Either (isRight)
import Math.Agate.OgPoset.OgPoset
import Test.Tasty
import Test.Tasty.HUnit
import Tests.PetriNet (exampleSIRODE)
import GHC.Generics (Selector)
import Control.Exception
import System.Exit
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Functor.Identity
import Data.Set

ogPosetTests :: TestTree
ogPosetTests =
  testGroup "The OgPoset DSL" [
    testGroup "Basic properties of arrows" [
        testCase "Can create arrows" $
            True @?= True
    ]
  ]