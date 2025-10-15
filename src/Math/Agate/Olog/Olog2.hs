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
    identityPath, (~), PathException(..), (===), Relator(..) )
where
import Control.Exception (throw, Exception)
import Data.Functor.Identity

data Arrow dot = Arrow {
    name :: String,
    source :: dot,
    target :: dot
}   deriving (Show, Eq)

data Path dot = Path {
    source :: dot,
    target :: dot,
    arrows :: [Arrow dot]
}   deriving (Show, Eq)

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
}   deriving (Show, Eq)

(===) :: (IsPath p1, IsPath p2, Eq dot, Show dot) =>
     p1 dot -> p2 dot -> Relator dot
p1 === p2 
    | p1' == p2'               = throw TrivialIdentityException
    | p1'.source /= p2'.source = throw MismatchedSourceException
    | p1'.target /= p2'.target = throw MismatchedTargetException
    | otherwise                = Relator { lhs = toPath p1, rhs = toPath p2 }
    where
        (p1', p2') = (toPath p1, toPath p2)

data RelationException = 
    TrivialIdentityException
    | MismatchedSourceException
    | MismatchedTargetException
    | OtherIdentityException
    deriving (Show)
     
instance Exception RelationException 

