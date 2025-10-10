{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
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

module Math.Agate.Olog.Olog 
  ( Arc,
    Identity,
    Olog,
    makeOlog,
    MakeOlogError (..),
  )
where

import Control.Monad
import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Traversable
import Debug.Trace

data Arc dot = Arc
  { name :: String,
    source :: dot,
    target :: dot
  }
  deriving (Show, Eq)

data Identity = Identity
  { lhs :: [String],
    rhs :: [String]
  }
  deriving (Show, Eq)

data Olog dot = Olog
  { dots :: [dot],
    arcs :: [Arc dot],
    identities :: [Identity]
  }
  deriving (Show, Eq)

makeOlogOld ::
  forall dot.
  (Eq dot, Show dot) =>
  [dot] ->
  [(String, dot, dot)] ->
  [([String], [String])] ->
  Either (MakeOlogError dot) (Olog dot)
makeOlogOld dots preArcs preIdentities =
  case errors of
    [] ->
      Right $
        Olog
          dots
          (map (\(name, src, tgt) -> Arc {name = name, source = src, target = tgt}) preArcs)
          ( (\(path1, path2) -> Identity {lhs = path1, rhs = path2}) <$> preIdentities
          )
    err : _ -> Left err
  where
    errorUnless b e = if b then Nothing else Just e
    errors :: [MakeOlogError dot] = arcErrors <> identityErrors
    arcErrors =
      concat . concat $
        map
          ( fmap maybeToList . \(dotMapper, errorPrefix) ->
              map (\arc@(name, _, _) -> (\dot -> errorUnless (dot `elem` dots) $ errorPrefix name dot) $ dotMapper arc) preArcs
          )
          [ (\(_, src, _) -> src, UnknownSource),
            (\(_, _, tgt) -> tgt, UnknownTarget)
          ]
    knownArcNames = map (\(name, _, _) -> name) preArcs
    identityErrors :: [MakeOlogError dot] =
      identityKnownArcErrors <> identityLhsJoinErrors <> identityRhsJoinErrors <> identityMismatchErrors
    identityKnownArcErrors =
      concat $
        map
          ( \(lhs, rhs) ->
              -- TODO: don't need to check triviality here
              (if null lhs && null rhs then [ForbiddenTrivialIdentity] else [])
                <> catMaybes
                  ( map
                      (\arcName -> errorUnless (arcName `elem` knownArcNames) $ UnknownArc arcName)
                      $ lhs <> rhs
                  )
          )
          preIdentities
    namesToArcs :: Map String (dot, dot) = Map.fromList $ (\(s, src, tgt) -> (s, (src, tgt))) <$> preArcs
    identityLhsJoinErrors = identityXhsJoinErrors NonJoiningExpressionLhs fst
    identityRhsJoinErrors = identityXhsJoinErrors NonJoiningExpressionRhs snd
    identityXhsJoinErrors ::
      ([String] -> MakeOlogError dot) ->
      (([String], [String]) -> [String]) ->
      [MakeOlogError dot]
    identityXhsJoinErrors errorFactory picker = catMaybes $ map (checkTerm errorFactory . picker) preIdentities
    checkTerm :: ([String] -> MakeOlogError dot) -> [String] -> Maybe (MakeOlogError dot)
    checkTerm errorFactory arcNames = errorUnless (targets == sources) $ errorFactory arcNames
      where
        arcs :: [(dot, dot)] = catMaybes $ flip Map.lookup namesToArcs <$> arcNames
        targets :: [dot] = tail $ snd <$> arcs
        sources :: [dot] = init $ fst <$> arcs
    identityMismatchErrors = catMaybes $ checkMismatch <$> preIdentities
    checkMismatch :: ([String], [String]) -> Maybe (MakeOlogError dot)
    checkMismatch (lhs, rhs) = do
      nonEmptyLhsAndSig <- case nonEmpty lhs of
        Nothing ->
          -- lhs empty
          Just Nothing
        Just nonEmptyLhs -> do
          -- lhs non-empty
          sig <- signature nonEmptyLhs
          Just $ Just sig
      nonEmptyRhsAndSig <- case nonEmpty rhs of
        Nothing ->
          -- rhs empty
          Just Nothing
        Just nonEmptyRhs -> do
          -- rhs non-empty
          sig <- signature nonEmptyRhs
          Just $ Just sig
      case (nonEmptyLhsAndSig, nonEmptyRhsAndSig) of
        (Nothing, Nothing) ->
          -- both empty
          Just ForbiddenTrivialIdentity
        (Just (src, tgt), Nothing) ->
          -- right empty
          errorUnless (src == tgt) $ NotALoop lhs
        (Nothing, Just (src, tgt)) ->
          -- left empty
          errorUnless (src == tgt) $ NotALoop rhs
        (Just lSig, Just rSig) ->
          -- both non-empty
          errorUnless (lSig == rSig) $ IdentityMismatch lhs rhs lSig rSig
      where
        signature :: NonEmpty String -> Maybe (dot, dot)
        signature terms =
          case Map.lookup (NE.last terms) namesToArcs of
            Nothing -> Nothing
            Just (src, _) ->
              case Map.lookup (NE.head terms) namesToArcs of
                Nothing -> Nothing
                Just (_, tgt) -> Just (src, tgt)

-- type f $ x = f x
-- type ($) f x = f x

data MakeOlogError dot
  = UnknownSource String dot
  | UnknownTarget String dot
  | ForbiddenTrivialIdentity
  | UnknownArc String
  | NonJoiningExpressionLhs [String]
  | NonJoiningExpressionRhs [String]
  | IdentityMismatch [String] [String] (dot, dot) (dot, dot)
  | NotALoop [String]
  deriving (Show, Eq)

makeOlog ::
  forall dot.
  (Eq dot) =>
  [dot] ->
  [(String, dot, dot)] ->
  [([String], [String])] ->
  Either (MakeOlogError dot) (Olog dot)
makeOlog dots preArcs preIdentities = do
  arcs <- for preArcs \(name, source, target) -> do
    -- TODO reuse `namesToArcs`?
    unless (source `elem` dots) $ Left $ UnknownSource name source
    unless (target `elem` dots) $ Left $ UnknownTarget name target
    pure Arc{name, source, target}
  identities <- for preIdentities \(lhs, rhs) -> do
    lhs' :: [(String, (dot, dot))] <- for lhs \arcName -> case Map.lookup arcName namesToArcs of
      Nothing -> Left $ UnknownArc arcName
      Just srcAndTgt -> pure (arcName, srcAndTgt)
    rhs' :: [(String, (dot, dot))] <- for rhs \arcName -> case Map.lookup arcName namesToArcs of
      Nothing -> Left $ UnknownArc arcName
      Just srcAndTgt -> pure (arcName, srcAndTgt)
    case (nonEmpty lhs', nonEmpty rhs') of
      (Nothing, Nothing) ->
        Left ForbiddenTrivialIdentity
      (Just l, Nothing) -> do
        checkTerm NonJoiningExpressionLhs l
        let (_, (_, tgt)) = NE.head l
        let (_, (src, _)) = NE.last l
        errorWhen (src /= tgt) $ NotALoop lhs
      (Nothing, Just r) -> do
        checkTerm NonJoiningExpressionRhs r
        let (_, (_, tgt)) = NE.head r
        let (_, (src, _)) = NE.last r
        errorWhen (src /= tgt) $ NotALoop rhs
      (Just l, Just r) -> do
        checkTerm NonJoiningExpressionLhs l
        checkTerm NonJoiningExpressionRhs r
        let combinedSrcTgt xhs = (fst $ snd $ NE.last xhs, snd $ snd $ NE.head xhs)
            l' = combinedSrcTgt l
            r' = combinedSrcTgt r
        errorWhen (l' /=  r') $ IdentityMismatch lhs rhs l' r'
    pure Identity {lhs, rhs}
  pure Olog {dots, arcs, identities}
  where
    checkTerm errorFactory arcs = do
      let
        targets = NE.tail $ snd . snd <$> (arcs :: (NonEmpty (String, (dot, dot))))
        sources = NE.init $ fst . snd <$> arcs
      errorWhen (sources /= targets) $ errorFactory $ map fst $ NE.toList arcs 
    errorWhen b e = if b then Left e else Right ()
    namesToArcs = Map.fromList $ (\(name, src, tgt) -> (name, (src, tgt))) <$> preArcs

-- checkArc :: (PreArc dot -> dot, String) -> PreArc dot -> Maybe String
-- checkArc (mapper, errorPrefix) preArc =
--   if mapper preArc `elem` dots then Nothing else Just $ errorPrefix <> show dot
-- checkers :: [(PreArc dot -> dot, String)] =
--   [ (\(_, src, _) -> src, "bad source: "),
--     (\(_, _, tgt) -> tgt, "bad target: ")
--   ]
-- applyChecker :: (PreArc dot -> dot, String) -> Maybe String
-- applyChecker (mapper, prefix) =
--   map mapper arcs
-- rawStrings :: [Maybe String] = map applyChecker [
--   (\(_, src, _) -> src, "bad source: "),
--     (\(_, _, tgt) -> tgt, "bad target: ")
--   ]
-- errors = []
