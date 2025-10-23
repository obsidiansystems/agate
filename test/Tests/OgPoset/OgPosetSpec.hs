{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant return" #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Tests.OgPoset.OgPosetSpec where

import Control.Exception
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Either (isRight)
import Data.Foldable
import Data.Functor.Identity
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics (Selector)
import Math.Agate.OgPoset.OgPoset (
  AddFaceException (..), GradedPoset (..), HasCofaces (..), HasFaces (..), 
  OgFaceTable (..), OgPoset (..), buildOgPoset, predecessors)
import System.Exit
import Test.Tasty
import Test.Tasty.HUnit


type FancyInt = (Int, Int)

ogPosetTests :: TestTree
ogPosetTests =
  testGroup
    "The OgPoset DSL"
    [ let
        checkOneArcPoset :: Either (AddFaceException Int) (OgFaceTable Int) -> Assertion
        checkOneArcPoset maybePoset = do
          case maybePoset of
            Left err -> assertFailure $ "Failed to create OgPoset: " ++ show err
            Right poset -> do
              grades poset @?= Map.fromList [(0, Set.fromList [0, 1]), (1, Set.fromList [2])]
              grade poset 0 @?= Just 0
              grade poset 1 @?= Just 0
              grade poset 2 @?= Just 1
              infaces poset @?= Map.fromList [(0, Set.empty), (1, Set.empty), (2, Set.fromList [0])]
              outfaces poset @?= Map.fromList [(0, Set.empty), (1, Set.empty), (2, Set.fromList [1])]
              incofaces poset @?= Map.fromList [(0, Set.fromList [2]), (1, Set.empty), (2, Set.empty)]
              outcofaces poset @?= Map.fromList [(0, Set.empty), (1, Set.fromList [2]), (2, Set.empty)]
              predecessors poset 0 @?= Set.fromList [0]
              predecessors poset 1 @?= Set.fromList [1]
              predecessors poset 2 @?= Set.fromList [0, 1, 2]
              True @?= True
        checkExample11 :: Either (AddFaceException FancyInt) (OgFaceTable FancyInt) -> Assertion
        checkExample11 maybePoset = do
          case maybePoset of
            Left err -> assertFailure $ "Failed to create OgPoset: " ++ show err
            Right poset ->
              let
                verifyXFaces ::
                  (OgFaceTable FancyInt -> Map FancyInt (Set FancyInt)) ->
                  [(FancyInt, [FancyInt])] -> Assertion
                verifyXFaces xfaces expectedValues =
                  xfaces poset @?= expectedMap where
                    expectedMap :: (Map FancyInt (Set FancyInt)) =
                      Set.fromList <$> Map.fromList expectedValues
               in
                do
                  grades poset
                    @?= Map.fromList
                      [ (0, Set.fromList [(0, 0), (0, 1), (0, 2), (0, 3)])
                      , (1, Set.fromList [(1, 0), (1, 1), (1, 2), (1, 3)])
                      , (2, Set.fromList [(2, 0)])
                      ]
                  for_ [0, 1, 2, 3] (\n -> grade poset (0, n) @?= Just 0)
                  for_ [0, 1, 2, 3] (\n -> grade poset (1, n) @?= Just 1)
                  grade poset (2, 0) @?= Just 2
                  verifyXFaces infaces [
                    ((0, 0), []), 
                    ((0, 1), []), 
                    ((0, 2), []),
                    ((0, 3), []),
                    ((1, 0), [(0, 0)]),
                    ((1, 1), [(0, 1)]),
                    ((1, 2), [(0, 2)]),
                    ((1, 3), [(0, 0)]),
                    ((2, 0), [(1, 0), (1, 1)])
                    ]
                  verifyXFaces outfaces [
                    ((0, 0), []), 
                    ((0, 1), []), 
                    ((0, 2), []),
                    ((0, 3), []),
                    ((1, 0), [(0, 1)]),
                    ((1, 1), [(0, 2)]),
                    ((1, 2), [(0, 3)]),
                    ((1, 3), [(0, 2)]),
                    ((2, 0), [(1, 3)])
                    ]
                  verifyXFaces incofaces [
                    ((0, 0), [(1, 0), (1, 3)]),
                    ((0, 1), [(1, 1)]),
                    ((0, 2), [(1, 2)]),
                    ((0, 3), []),
                    ((1, 0), [(2, 0)]),
                    ((1, 1), [(2, 0)]),
                    ((1, 2), []),
                    ((1, 3), []),
                    ((2, 0), [])
                    ]
                  verifyXFaces outcofaces [
                    ((0, 0), []),
                    ((0, 1), [(1, 0)]),
                    ((0, 2), [(1, 1), (1, 3)]),
                    ((0, 3), [(1, 2)]),
                    ((1, 0), []),
                    ((1, 1), []),
                    ((1, 2), []),
                    ((1, 3), [(2, 0)]),
                    ((2, 0), [])
                    ]
                  predecessors poset (0, 0) @?= Set.fromList [(0, 0)]
                  predecessors poset (0, 1) @?= Set.fromList [(0, 1)]
                  predecessors poset (0, 2) @?= Set.fromList [(0, 2)]
                  predecessors poset (0, 3) @?= Set.fromList [(0, 3)]
                  predecessors poset (1, 0) @?= Set.fromList [(0, 0), (0, 1), (1, 0)]
                  predecessors poset (1, 1) @?= Set.fromList [(0, 1), (0, 2), (1, 1)]
                  predecessors poset (1, 2) @?= Set.fromList [(0, 2), (0, 3), (1, 2)]
                  predecessors poset (1, 3) @?= Set.fromList [(0, 0), (0, 2), (1, 3)]
                  predecessors poset (2, 0) @?= Set.fromList [
                    (0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 3), (2, 0)
                    ]
       in
        testGroup
          "Constructing OgPoset's"
          [ testCase "Can create a one-arc OgPoset" $
              checkOneArcPoset do
                let p :: OgFaceTable Int = empty
                p1 <- addFace 0 [] [] p
                p2 <- addFace 1 [] [] p1
                p3 <- addFace 2 [0] [1] p2
                return p3
          , testCase "Can create a one-arc OgPoset more conveniently" $
              checkOneArcPoset $
                buildOgPoset [(0, [], []), (1, [], []), (2, [0], [1])]
          , testCase "Can create an entry level pasting diagram, Amar's Example 11" $
              checkExample11 $
                buildOgPoset
                  [ ((0, 0), [], [])
                  , ((0, 1), [], [])
                  , ((0, 2), [], [])
                  , ((0, 3), [], [])
                  , ((1, 0), [(0, 0)], [(0, 1)])
                  , ((1, 1), [(0, 1)], [(0, 2)])
                  , ((1, 2), [(0, 2)], [(0, 3)])
                  , ((1, 3), [(0, 0)], [(0, 2)])
                  , ((2, 0), [(1, 0), (1, 1)], [(1, 3)])
                  ]
          ]
    ]
