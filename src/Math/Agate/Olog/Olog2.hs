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

module Math.Agate.Olog.Olog2(Arrow(..), Path (..), toPath, identityPath, (~), PathException(..) )
where
import Control.Exception (throw, Exception)
import Control.Lens (Identity)
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
    toPath arrow = Path {
        source = arrow.source,
        target = arrow.target,
        arrows = [ arrow ]
    }

-- class MyIdentity dot where
--     me :: dot

-- instance MyIdentity dot where 
--     me = undefined
    
-- instance IsPath MyIdentity where
--     toPath (MyIdentity dot) = Path {
--         source = dot,
--         target = dot,
--         arrows = [ ]
--     }

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

data PathException = forall a b . (Show a, Show b) => CompositionException (Path a) (Path b) | OtherException

instance Exception PathException

-- -- Simplify the specification of an olog as bunch of identities
--    retraction a->b
--    section b->a
-- >   r . s = IdentityPath b

-- arrowToPath :: forall dot . Arrow dot -> Path dot
-- arrowToPath arrow = Path {
--     source = arrow.source,
--     target = arrow.target,
--     arrows = [ arrow ]
-- }

identityPath :: forall dot . dot -> Path dot
identityPath aDot = Path {
    source = aDot,
    target = aDot,
    arrows = []
}


-- Path dot
--     source
--     target
--     arrows :: [Arrow dot]

-- IdentityPath x -> x
-- SingleArrowPath a -> b
-- CompoundPath a -> b -> c -..... 

-- r: a -> b
-- s: b -> a
-- r . s = IdentityPath b

-- data Arrow' dot = Arrow' dot dot

-- source' (Arrow s t) = s
-- target' (Arrow s t)  = t


-- identityArrow :: (Eq dot) => dot -> dot -> Arrow dot
-- s ~> t = Arrow s t

-- Arrow { source = s, target = t }

-- data MagazineInfo = Magazine Int String [String]
--                     deriving (Show)