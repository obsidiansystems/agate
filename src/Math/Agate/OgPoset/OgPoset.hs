{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Use concatMap" #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# HLINT ignore "Move catMaybes" #-}
{-# HLINT ignore "Use mapMaybe" #-}
{-# HLINT ignore "Use list comprehension" #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use mapM" #-}

module Math.Agate.OgPoset.OgPoset where

import Control.Monad (foldM)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Maybe

class GradedPoset p where
  grades :: p dot -> Map Int (Set dot)
  grade ::
    (Ord dot) =>
    p dot -> dot -> Maybe Int
  predecessors ::
    (Ord dot) =>
    p dot -> dot -> Set dot

class HasFaces p where
  infaces :: p dot -> Map dot (Set dot)
  outfaces :: p dot -> Map dot (Set dot)

class HasCofaces p where
  incofaces :: p dot -> Map dot (Set dot)
  outcofaces :: p dot -> Map dot (Set dot)

class (GradedPoset p, HasFaces p, HasCofaces p) => OgPoset p where
  empty :: p dot
  addFace :: (Ord dot) => dot -> [dot] -> [dot] -> p dot -> Either (AddFaceException dot) (p dot)

data OgFaceTable dot = OgFaceTable
  { _grades :: Map Int (Set dot)
  , _grade :: Map dot Int
  , _infaces :: Map dot (Set dot)
  , _outfaces :: Map dot (Set dot)
  , _incofaces :: Map dot (Set dot)
  , _outcofaces :: Map dot (Set dot)
  , _predecessors :: Map dot (Set dot)
  }
  deriving (Show, Eq, Ord)

instance GradedPoset OgFaceTable where
  grades = _grades
  grade ogFaceTable d =
    Map.lookup d (_grade ogFaceTable)
  predecessors ogFaceTable d =
    Map.findWithDefault Set.empty d (_predecessors ogFaceTable)

instance HasFaces OgFaceTable where
  infaces = _infaces
  outfaces = _outfaces

instance HasCofaces OgFaceTable where
  incofaces = _incofaces
  outcofaces = _outcofaces

data AddFaceException dot
  = UnknownFace dot
  | MismatchedGrades [Int]
  deriving (Show, Eq)

instance OgPoset OgFaceTable where
  empty =
    OgFaceTable
      { _grades = Map.empty
      , _grade = Map.empty
      , _infaces = Map.empty
      , _outfaces = Map.empty
      , _incofaces = Map.empty
      , _outcofaces = Map.empty
      , _predecessors = Map.empty
      }
  addFace ::
    forall dot.
    (Ord dot) =>
    dot -> [dot] -> [dot] -> OgFaceTable dot -> Either (AddFaceException dot) (OgFaceTable dot)
  addFace newDot newInfaces newOutfaces ogPoset =
    let lookupGrade :: dot -> Either (AddFaceException dot) Int
        lookupGrade f =
          case grade ogPoset f of
            Nothing -> Left $ UnknownFace f
            Just g -> Right g
     in -- Right ogPoset
        do
          gradedFaces :: [Int] <-
            sequence $ lookupGrade <$> (newInfaces <> newOutfaces)
          newGrade :: Int <-
            case gradedFaces of
              [] -> pure 0
              (x : xs) ->
                if all (== x) xs
                  then pure $ x + 1
                  else Left $ MismatchedGrades gradedFaces
          pure
            OgFaceTable
              { _grades = Map.insertWith Set.union newGrade (Set.singleton newDot) (_grades ogPoset)
              , _grade = Map.insert newDot newGrade (_grade ogPoset)
              , _infaces = Map.insert newDot (Set.fromList newInfaces) (_infaces ogPoset)
              , _outfaces = Map.insert newDot (Set.fromList newOutfaces) (_outfaces ogPoset)
              , _incofaces =
                  foldl'
                    (\m f -> Map.insertWith Set.union f (Set.singleton newDot) m)
                    (Map.insert newDot Set.empty (_incofaces ogPoset))
                    newInfaces
              , _outcofaces =
                  foldl'
                    (\m f -> Map.insertWith Set.union f (Set.singleton newDot) m)
                    (Map.insert newDot Set.empty (_outcofaces ogPoset))
                    newOutfaces
              , _predecessors =
                  Map.insert
                    newDot
                    ( Set.insert newDot $
                        Set.unions
                          (predecessors ogPoset <$> (newInfaces <> newOutfaces))
                    )
                    (_predecessors ogPoset)
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
      (grade poset d == Just n) &&
        Set.null (Set.intersection (doLookup d) setU)

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
