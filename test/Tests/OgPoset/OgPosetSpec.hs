{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}

module Tests.OgPoset.OgPosetSpec where

import Data.Either (isRight)
import Math.Agate.OgPoset.OgPoset(
  OgPoset(..), OgFaceTable(..), AddFaceException(..), buildOgPoset, Graded (grades),
  )
import Test.Tasty
import Test.Tasty.HUnit
import Tests.PetriNet (exampleSIRODE)
import GHC.Generics (Selector)
import Control.Exception
import System.Exit
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Functor.Identity
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Math.Agate.OgPoset.OgPoset (Graded(..), HasFaces(..), HasCofaces(..))

ogPosetTests :: TestTree
ogPosetTests =
  testGroup "The OgPoset DSL" [
    let
      checkOneArcPoset :: Either (AddFaceException Int) (OgFaceTable Int) -> Assertion
      checkOneArcPoset maybePoset = do
        case maybePoset of
          Left err -> assertFailure $ "Failed to create OgPoset: " ++ show err
          Right poset -> do
            grades poset @?= Map.fromList [(0, Set.fromList [0,1]), (1, Set.fromList [2])]
            grade poset 0 @?= Just 0
            grade poset 1 @?= Just 0
            grade poset 2 @?= Just 1
            infaces poset @?= Map.fromList [(0, Set.empty), (1, Set.empty), (2, Set.fromList [0])]
            outfaces poset @?= Map.fromList [(0, Set.empty), (1, Set.empty), (2, Set.fromList [1])]
            incofaces poset @?= Map.fromList [(0, Set.fromList [2]), (1, Set.empty), (2, Set.empty)]
            outcofaces poset @?= Map.fromList [(0, Set.empty), (1, Set.fromList [2]), (2, Set.empty)]
            True @?= True
    in
    testGroup "Constructing a one-arc OgPoset" [
        testCase "Can create an OgPoset" $
          let
            maybePoset :: (Either (AddFaceException Int) (OgFaceTable Int)) =
              do
                let p :: OgFaceTable Int = empty
                p1 <- addFace 0 [] [] p
                p2 <- addFace 1 [] [] p1
                p3 <- addFace 2 [0] [1] p2
                return p3
          in
            checkOneArcPoset maybePoset
        ,
        testCase "Can create an OgPoset more conveniently" $
          let
            maybePoset :: (Either (AddFaceException Int) (OgFaceTable Int)) =
              buildOgPoset [ (0, [], []), (1, [], []), (2, [0], [1]) ]
          in
            checkOneArcPoset maybePoset
    ]
  ]


