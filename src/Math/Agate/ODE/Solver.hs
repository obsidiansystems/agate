{-# LANGUAGE ScopedTypeVariables #-}
{- HLINT ignore "Use newtype instead of data" -}

module Math.Agate.ODE.Solver (odeSolve, ODEParams (..), solvePetri) where

import Data.Functor ((<&>))
import Data.List.Extra (enumerate)
import Data.Map.Lazy
import Data.Map.Lazy qualified as Map
import Math.Agate.ODE
import Math.Agate.ODE.Polynomial (PolynomialODE)
import Math.Agate.PetriNet

data ODEParams k = ODEParams
    { stepSize :: k
    }

odeSolve :: forall system k v. (ODESystem system, Field system ~ k, Var system ~ v, Ord v) => system -> ODEParams k -> Map v k -> [Map v k]
odeSolve sys params initialValues =
    newValues : odeSolve sys params newValues
  where
    newValues :: Map v k
    newValues = flip mapWithKey initialValues $ \v k ->
        (+) k
            . (*) (stepSize params)
            . eval @system (initialValues !)
            $ derivative sys v

solvePetri ::
    ( net ~ AsODE (PolynomialODE k p)
    , Enum p
    , Bounded p
    , Ord p
    , Show p
    , Eq k
    , Fractional k
    ) =>
    net -> (p -> k) -> [Map p k]
solvePetri p x0 =
    odeSolve (asODE p) (ODEParams 0.1)
        . Map.fromList
        $ enumerate <&> \v -> (v, x0 v)
