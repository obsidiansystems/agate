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
{-# LANGUAGE ExistentialQuantification #-}

module Math.Agate.Olog.Olog2(Arrow(..), Path (..), toPath, 
    identityPath, (~), PathException(..), (===), Relator(..),RelatorException(..),
    Olog(..), makeOlog, makeOlogWithExtras  )
where
import Control.Exception (throw, Exception)
import Data.Functor.Identity
import Data.List
import Data.Set

data Arrow dot = Arrow {
    name :: String,
    source :: dot,
    target :: dot
}   deriving (Show, Eq, Ord)

data Path dot = Path {
    source :: dot,
    target :: dot,
    arrows :: [Arrow dot]
}   deriving (Show, Eq, Ord)

class IsPath p where
    toPath :: p dot -> Path dot

instance IsPath Path where
    toPath = id

instance IsPath Arrow where
    toPath = arrowToPath

arrowToPath :: Arrow dot -> Path dot
arrowToPath arrow = Path {
        source = arrow.source,
        target = arrow.target,
        arrows = [ arrow ]
    }

instance IsPath Identity where
    toPath (Identity dot) = identityPath dot

(~) :: (IsPath p1, IsPath p2, Eq dot, Show dot) =>
     p1 dot -> p2 dot -> Path dot
p1 ~ p2 =
    let
        p1' = toPath p1
        p2' = toPath p2
    in if p1'.source == p2'.target then
        Path {
            source = p2'.source,
            target = p1'.target,
            arrows = (toPath p1).arrows ++ (toPath p2).arrows
        }
    else
        throw $ CompositionException p1' p2'

instance Show PathException  where
    show (CompositionException p1 p2) = "Cannot compose paths: " ++ show p1 ++ " and " ++ show p2
    show OtherException = "Other error"

data PathException = forall a b . (Show a, Show b) => 
    CompositionException (Path a) (Path b) | OtherException

instance Exception PathException

identityPath :: forall dot . dot -> Path dot
identityPath aDot = Path {
    source = aDot,
    target = aDot,
    arrows = []
}

data Relator dot = Relator {
    -- name :: String,
    lhs :: Path dot,
    rhs :: Path dot
}   deriving (Show, Eq, Ord)

(===) :: (IsPath p1, IsPath p2, Eq dot, Show dot) =>
     p1 dot -> p2 dot -> Relator dot
p1 === p2 
    | p1' == p2'               = throw TrivialIdentityException
    | p1'.source /= p2'.source = throw MismatchedSourceException
    | p1'.target /= p2'.target = throw MismatchedTargetException
    | otherwise                = Relator { lhs = toPath p1, rhs = toPath p2 }
    where
        (p1', p2') = (toPath p1, toPath p2)

data RelatorException = 
    TrivialIdentityException
    | MismatchedSourceException
    | MismatchedTargetException
    | OtherIdentityException
    deriving (Show, Eq)
     
instance Exception RelatorException 

data Olog dot = Olog {
    relators :: Set (Relator dot),
    arrows :: Set (Arrow dot),
    dots :: Set dot
}   deriving (Show, Eq)

makeOlogWithExtras :: forall dot . (Show dot, Ord dot) =>
    [Relator dot] -> [Arrow dot] -> [dot] -> Olog dot
makeOlogWithExtras relators arrows dots = Olog {
    relators = fromList relators,
    arrows = fromList abridgedArrows,
    dots = fromList abridgedDots
} where
    arrowsInRelators = concatMap (\r -> r.lhs.arrows ++ r.rhs.arrows) relators
    abridgedArrows = nub $ arrows ++ arrowsInRelators
    dotsInArrows = concatMap (\a -> [a.source, a.target]) abridgedArrows
    abridgedDots = nub $ dots ++ dotsInArrows

-- (concatMap arrows (relators >>= \r -> [r.lhs, r.rhs])),

makeOlog :: forall dot . (Show dot, Ord dot) => [Relator dot] -> Olog dot
makeOlog relators = 
    makeOlogWithExtras relators [] []
