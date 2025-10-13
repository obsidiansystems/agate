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

module Math.Agate.Olog.Olog2(Arrow(..), (~>))
where

data (Eq dot) =>Arrow dot = Arrow {
    source :: dot,
    target :: dot
}   deriving (Show, Eq)

-- data Arrow' dot = Arrow' dot dot

-- source' (Arrow s t) = s
-- target' (Arrow s t)  = t

(~>) :: (Eq dot) => dot -> dot -> Arrow dot
s ~> t = Arrow { source = s, target = t }

-- data MagazineInfo = Magazine Int String [String]
--                     deriving (Show)