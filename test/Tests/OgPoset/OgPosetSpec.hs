{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# HLINT ignore "Redundant return" #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Tests.OgPoset.OgPosetSpec where

import Data.Foldable
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Math.Agate.OgPoset.OgPoset (
  AddFaceException (..),
  GradedPoset (..),
  HasCofaces (..),
  HasFaces (..),
  OgFaceTable (..),
  OgPoset (..),
  buildOgPoset,
  closure,
  predecessors,
  dimension
 )
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
              closure poset Set.empty @?= Set.empty
              closure poset (Set.fromList [0]) @?= Set.fromList [0]
              closure poset (Set.fromList [1]) @?= Set.fromList [1]
              closure poset (Set.fromList [0, 1]) @?= Set.fromList [0, 1]
              closure poset (Set.fromList [2]) @?= Set.fromList [0, 1, 2]
              dimension poset Set.empty @?= -1
              dimension poset (Set.fromList [0, 1]) @?= 0
              dimension poset (Set.fromList [2]) @?= 1
        checkExample11 :: Either (AddFaceException FancyInt) (OgFaceTable FancyInt) -> Assertion
        checkExample11 maybePoset = do
          case maybePoset of
            Left err -> assertFailure $ "Failed to create OgPoset: " ++ show err
            Right poset -> do
              grades poset
                @?= Map.fromList
                  [ (0, Set.fromList [(0, 0), (0, 1), (0, 2), (0, 3)])
                  , (1, Set.fromList [(1, 0), (1, 1), (1, 2), (1, 3)])
                  , (2, Set.fromList [(2, 0)])
                  ]
              for_ [0, 1, 2, 3] (\n -> grade poset (0, n) @?= Just 0)
              for_ [0, 1, 2, 3] (\n -> grade poset (1, n) @?= Just 1)
              grade poset (2, 0) @?= Just 2
              verifyXFaces
                (infaces poset)
                [ ((0, 0), [])
                , ((0, 1), [])
                , ((0, 2), [])
                , ((0, 3), [])
                , ((1, 0), [(0, 0)])
                , ((1, 1), [(0, 1)])
                , ((1, 2), [(0, 2)])
                , ((1, 3), [(0, 0)])
                , ((2, 0), [(1, 0), (1, 1)])
                ]
              verifyXFaces
                (outfaces poset)
                [ ((0, 0), [])
                , ((0, 1), [])
                , ((0, 2), [])
                , ((0, 3), [])
                , ((1, 0), [(0, 1)])
                , ((1, 1), [(0, 2)])
                , ((1, 2), [(0, 3)])
                , ((1, 3), [(0, 2)])
                , ((2, 0), [(1, 3)])
                ]
              verifyXFaces
                (incofaces poset)
                [ ((0, 0), [(1, 0), (1, 3)])
                , ((0, 1), [(1, 1)])
                , ((0, 2), [(1, 2)])
                , ((0, 3), [])
                , ((1, 0), [(2, 0)])
                , ((1, 1), [(2, 0)])
                , ((1, 2), [])
                , ((1, 3), [])
                , ((2, 0), [])
                ]
              verifyXFaces
                (outcofaces poset)
                [ ((0, 0), [])
                , ((0, 1), [(1, 0)])
                , ((0, 2), [(1, 1), (1, 3)])
                , ((0, 3), [(1, 2)])
                , ((1, 0), [])
                , ((1, 1), [])
                , ((1, 2), [])
                , ((1, 3), [(2, 0)])
                , ((2, 0), [])
                ]
              predecessors poset (0, 0) @?= Set.fromList [(0, 0)]
              predecessors poset (0, 1) @?= Set.fromList [(0, 1)]
              predecessors poset (0, 2) @?= Set.fromList [(0, 2)]
              predecessors poset (0, 3) @?= Set.fromList [(0, 3)]
              predecessors poset (1, 0) @?= Set.fromList [(0, 0), (0, 1), (1, 0)]
              predecessors poset (1, 1) @?= Set.fromList [(0, 1), (0, 2), (1, 1)]
              predecessors poset (1, 2) @?= Set.fromList [(0, 2), (0, 3), (1, 2)]
              predecessors poset (1, 3) @?= Set.fromList [(0, 0), (0, 2), (1, 3)]
              predecessors poset (2, 0)
                @?= Set.fromList
                  [ (0, 0)
                  , (0, 1)
                  , (0, 2)
                  , (1, 0)
                  , (1, 1)
                  , (1, 3)
                  , (2, 0)
                  ]
              closure poset Set.empty @?= Set.empty
              closure poset (Set.fromList [(1, 0)]) @?= Set.fromList [(0, 0), (0, 1), (1, 0)]
              closure poset (Set.fromList [(1, 0), (1, 1)])
                @?= Set.fromList [(0, 0), (0, 1), (0, 2), (1, 0), (1, 1)]
              dimension poset Set.empty @?= -1
              dimension poset (Set.fromList [(0, 0)]) @?= 0
              dimension poset (Set.fromList [(0, 0), (1, 1)]) @?= 1
              dimension poset (Set.fromList [(1, 0), (1, 3)]) @?= 1
              dimension poset (Set.fromList [(1, 0), (2, 0)]) @?= 2
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

verifyXFaces ::
  Map FancyInt (Set FancyInt) ->
  [(FancyInt, [FancyInt])] ->
  Assertion
verifyXFaces xfaces expectedValues =
  xfaces @?= expectedMap
 where
  expectedMap :: (Map FancyInt (Set FancyInt)) =
    Set.fromList <$> Map.fromList expectedValues
