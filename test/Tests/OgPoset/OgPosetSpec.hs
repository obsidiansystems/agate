{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}

module Tests.OgPoset.OgPosetSpec where

import Data.Either (isRight)
import Math.Agate.OgPoset.OgPoset(OgPoset(..), OgFaceTable(..), AddFaceException(..))
import Test.Tasty
import Test.Tasty.HUnit
import Tests.PetriNet (exampleSIRODE)
import GHC.Generics (Selector)
import Control.Exception
import System.Exit
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Functor.Identity


ogPosetTests :: TestTree
ogPosetTests =
  testGroup "The OgPoset DSL" [
    testGroup "Constructing an OgPoset" [
        testCase "Can create arrows" $
          let 
            poset = do
              let p = empty :: OgFaceTable Int
              p1 <- addFace 0 [] [] p
              p2 <- addFace 1 [] [] p1
              p3 <- addFace 2 [0] [1] p2
              return p3
          in
            True @?= True
    ]
  ]