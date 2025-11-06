{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Use concatMap" #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# HLINT ignore "Move catMaybes" #-}
{-# HLINT ignore "Use mapMaybe" #-}
{-# HLINT ignore "Use list comprehension" #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use mapM" #-}

module Math.Agate.OgPoset.OgPoset where

import Control.Monad (foldM)
import Data.Containers.ListUtils (nubOrd)
import Data.Either.Extra
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set

class GradedPoset p where
  grades :: p dot -> Map Int (Set dot)
  grade ::
    (Ord dot) =>
    p dot -> dot -> Maybe Int
  predecessors ::
    (Ord dot) =>
    p dot -> dot -> Set dot

class (GradedPoset p) => HasFaces p where
  infaces :: p dot -> Map dot (Set dot)
  outfaces :: p dot -> Map dot (Set dot)

class (GradedPoset p) => HasCofaces p where
  incofaces :: p dot -> Map dot (Set dot)
  outcofaces :: p dot -> Map dot (Set dot)

class (HasFaces p, HasCofaces p) => OgPoset p where
  empty :: p dot
  addFace :: (Ord dot) => dot -> [dot] -> [dot] -> p dot -> Either (AddFaceException dot) (p dot)

data OgFaceTable dot = OgFaceTable
  { grades :: Map Int (Set dot)
  , grade :: Map dot Int
  , infaces :: Map dot (Set dot)
  , outfaces :: Map dot (Set dot)
  , incofaces :: Map dot (Set dot)
  , outcofaces :: Map dot (Set dot)
  , predecessors :: Map dot (Set dot)
  }
  deriving (Show, Eq, Ord)

instance GradedPoset OgFaceTable where
  grades = (.grades)
  grade ogFaceTable d =
    Map.lookup d ogFaceTable.grade
  predecessors ogFaceTable d =
    Map.findWithDefault Set.empty d ogFaceTable.predecessors

instance HasFaces OgFaceTable where
  infaces = (.infaces)
  outfaces = (.outfaces)

instance HasCofaces OgFaceTable where
  incofaces = (.incofaces)
  outcofaces = (.outcofaces)

data AddFaceException dot
  = UnknownFace dot
  | MismatchedGrades [Int]
  deriving (Show, Eq)

instance OgPoset OgFaceTable where
  empty =
    OgFaceTable
      { grades = Map.empty
      , grade = Map.empty
      , infaces = Map.empty
      , outfaces = Map.empty
      , incofaces = Map.empty
      , outcofaces = Map.empty
      , predecessors = Map.empty
      }
  addFace ::
    forall dot.
    (Ord dot) =>
    dot -> [dot] -> [dot] -> OgFaceTable dot -> Either (AddFaceException dot) (OgFaceTable dot)
  addFace newDot newInfaces newOutfaces ogPoset =
    let lookupGrade :: dot -> Either (AddFaceException dot) Int
        lookupGrade f = maybeToEither (UnknownFace f) $ grade ogPoset f
     in do
          gradedFaces :: [Int] <-
            traverse lookupGrade (newInfaces <> newOutfaces)
          newGrade :: Int <-
            case gradedFaces of
              [] -> pure 0
              x : xs ->
                if all (== x) xs
                  then pure $ x + 1
                  else Left $ MismatchedGrades gradedFaces
          pure
            OgFaceTable
              { grades = Map.insertWith Set.union newGrade (Set.singleton newDot) (grades ogPoset)
              , grade = Map.insert newDot newGrade ogPoset.grade
              , infaces = Map.insert newDot (Set.fromList newInfaces) (infaces ogPoset)
              , outfaces = Map.insert newDot (Set.fromList newOutfaces) (outfaces ogPoset)
              , incofaces =
                  foldl'
                    (\m f -> Map.insertWith Set.union f (Set.singleton newDot) m)
                    (Map.insert newDot Set.empty (incofaces ogPoset))
                    newInfaces
              , outcofaces =
                  foldl'
                    (\m f -> Map.insertWith Set.union f (Set.singleton newDot) m)
                    (Map.insert newDot Set.empty (outcofaces ogPoset))
                    newOutfaces
              , predecessors =
                  Map.insert
                    newDot
                    ( Set.insert newDot $
                        Set.unions
                          (predecessors ogPoset <$> (newInfaces <> newOutfaces))
                    )
                    ogPoset.predecessors
              }

buildOgPoset ::
  (OgPoset og, Ord dot) =>
  [(dot, [dot], [dot])] ->
  Either (AddFaceException dot) (og dot)
buildOgPoset =
  foldM
    ( \ogPoset (d, infs, outfs) ->
        addFace d infs outfs ogPoset
    )
    empty

closure ::
  forall dot p.
  (Ord dot, GradedPoset p) =>
  p dot -> Set dot -> Set dot
closure poset dots =
  Set.unions $ map (predecessors poset) $ Set.toList dots

dimension ::
  forall dot p.
  (Ord dot, GradedPoset p) =>
  p dot -> Set dot -> Int
dimension poset dots
  | null dots = -1
  | otherwise = maximum $ catMaybes theGrades
 where
  dotsL :: [dot] = Set.toList dots
  theGrades :: [Maybe Int] = grade poset <$> dotsL

inPreBoundary ::
  forall dot p.
  (Ord dot, OgPoset p) =>
  p dot -> Int -> Set dot -> Set dot
inPreBoundary poset n =
  preBoundaryFor poset n outcofaces

outPreBoundary ::
  forall dot p.
  (Ord dot, OgPoset p) =>
  p dot -> Int -> Set dot -> Set dot
outPreBoundary poset n =
  preBoundaryFor poset n incofaces

preBoundaryFor ::
  forall dot p.
  (Ord dot, OgPoset p) =>
  p dot -> Int -> (p dot -> Map dot (Set dot)) -> Set dot -> Set dot
preBoundaryFor poset n xFaces setU =
  Set.filter boundary setU
 where
  faces :: Map dot (Set dot)
  faces = xFaces poset
  doLookup :: dot -> Set dot
  doLookup d = Map.findWithDefault Set.empty d faces
  boundary :: dot -> Bool
  boundary d =
    (grade poset d == Just n)
      && Set.null (Set.intersection (doLookup d) setU)

inBoundary ::
  forall dot p.
  (Ord dot, OgPoset p) =>
  p dot -> Int -> Set dot -> Set dot
inBoundary poset n =
  boundaryFor poset n inPreBoundary

outBoundary ::
  forall dot p.
  (Ord dot, OgPoset p) =>
  p dot -> Int -> Set dot -> Set dot
outBoundary poset n =
  boundaryFor poset n outPreBoundary

boundaryFor ::
  forall dot p.
  (Ord dot, OgPoset p) =>
  p dot -> Int -> (p dot -> Int -> Set dot -> Set dot) -> Set dot -> Set dot
boundaryFor poset n xPreBoundary setU =
  Set.unions $
    closure poset
      <$> (preBdr : maxClosures)
 where
  preBdr :: Set dot =
    xPreBoundary poset n setU
  maxU :: Set dot =
    maximals poset setU
  maxClosures :: [Set dot] =
    (\k -> level poset k maxU)
      <$> [0 .. (n - 1)]

maximals ::
  forall dot p.
  (Ord dot, OgPoset p) =>
  p dot -> Set dot -> Set dot
maximals poset set =
  Set.filter maximal set
 where
  maximal :: dot -> Bool
  maximal x =
    disjoint incofaces && disjoint outcofaces
   where
    disjoint :: (p dot -> Map dot (Set dot)) -> Bool
    disjoint xFaces =
      null $ Set.intersection set $ Map.findWithDefault Set.empty x $ xFaces poset

level ::
  forall dot p.
  (Ord dot, OgPoset p) =>
  p dot -> Int -> Set dot -> Set dot
level poset n =
  Set.filter (\x -> grade poset x == Just n)

elements ::
  forall dot p.
  (Ord dot, OgPoset p) =>
  p dot -> Set dot
elements poset =
  Set.unions $ Map.elems $ grades poset

sublists :: forall a. [a] -> [[a]]
sublists [] = [[]]
sublists (x : xs) =
  recursed ++ map (x :) recursed
 where
  recursed :: [[a]]
  recursed = sublists xs

closedSubsets ::
  forall dot p.
  (Ord dot, OgPoset p) =>
  p dot -> Set (Set dot)
closedSubsets poset =
  Set.fromList $ nubOrd allUnions
 where
  allUnions :: [Set dot]
  allUnions = Set.unions <$> sublists (nubOrd allCyclics)
  allCyclics :: [Set dot]
  allCyclics = predecessors poset <$> Set.toList (elements poset)
