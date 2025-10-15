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

module Math.Agate.OgPoset.OgPoset where

import Control.Exception (Exception, throw)
import Data.Functor.Identity
import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

class Graded p where
  grades :: p dot -> Map Int (Set dot)
  grade :: (Ord dot) => p dot -> dot -> Maybe Int

class HasFaces p where
  infaces :: p dot -> Map dot (Set dot)
  outfaces :: p dot -> Map dot (Set dot)

class HasCofaces p where
  incofaces :: p dot -> Map dot (Set dot)
  outcofaces :: p dot -> Map dot (Set dot)

class (Graded p, HasFaces p, HasCofaces p) => OgPoset p where
  empty :: p dot
  addFace :: (Ord dot) => dot -> [dot] -> [dot] -> p dot -> Either (AddFaceException dot) (p dot)

-- addCoface :: dot -> [dot] -> [dot] -> p dot -> p dot -> Either AddCofaceException (p dot)

-- data OgPosetException =
--     AddFaceException dot
--     | OtherIdentityException
--     deriving (Show, Eq)

-- instance Exception AddFaceException

data OgFaceTable dot = OgFaceTable
  { _grades :: Map Int (Set dot)
  , _grade :: Map dot Int
  , _infaces :: Map dot (Set dot)
  , _outfaces :: Map dot (Set dot)
  , _incofaces :: Map dot (Set dot)
  , _outcofaces :: Map dot (Set dot)
  }
  deriving (Show, Eq, Ord)

instance Graded OgFaceTable where
  grades = _grades
  grade ogFaceTable d =
    Map.lookup d (_grade ogFaceTable)

instance HasFaces OgFaceTable where
  infaces = _infaces
  outfaces = _outfaces

instance HasCofaces OgFaceTable where
  incofaces = _incofaces
  outcofaces = _outcofaces

data AddFaceException dot
  = UnknownFace dot
  | MismatchedGrades [Int]

instance OgPoset OgFaceTable where
  empty =
    OgFaceTable
      { _grades = Map.empty
      , _infaces = Map.empty
      , _outfaces = Map.empty
      , _incofaces = Map.empty
      , _outcofaces = Map.empty
      }
  addFace :: forall dot.(Ord dot) => 
    dot -> [dot] -> [dot] -> OgFaceTable dot -> Either (AddFaceException dot) (OgFaceTable dot)
  addFace newDot infaces outfaces ogPoset =
    let lookupGrade :: dot -> Either (AddFaceException dot) Int
        lookupGrade f =
          case grade ogPoset f of
            Nothing -> Left $ UnknownFace f
            Just g -> Right g
     in -- Right ogPoset
        do
          gradedFaces :: [Int] <-
            sequence $ lookupGrade <$> (infaces <> outfaces)
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
              , _infaces = Map.insert newDot (Set.fromList infaces) (_infaces ogPoset)
              , _outfaces = Map.insert newDot (Set.fromList outfaces) (_outfaces ogPoset)
              , _incofaces =
                  foldl'
                    (\m f -> Map.insertWith Set.union f (Set.singleton newDot) m)
                    (_incofaces ogPoset)
                    infaces
              , _outcofaces =
                  foldl'
                    (\m f -> Map.insertWith Set.union f (Set.singleton newDot) m)
                    (_outcofaces ogPoset)
                    outfaces
              }

-- currentGrades = _grades ogft
-- in
--   Right OgFaceTable {
--     _grades = updatedGrades,
--     _infaces = updatedInfaces,
--     _outfaces = updatedOutfaces,
--     _incofaces = updatedIncofaces,
--     _outcofaces = updatedOutcofaces
--   }

-- data OgPoset dot = Arrow {
--     grades :: Map Int (Set dot),

-- }   deriving (Show, Eq, Ord)

-- data OgPoset dot = OgPoset {
--   faceTableRep :: OgFaceTable dot
--   ...
-- }
