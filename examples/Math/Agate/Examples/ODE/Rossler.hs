module Math.Agate.Examples.ODE.Rossler where

import Data.Map.Lazy qualified as Map
import Math.Agate.ODE
import Math.Agate.ODE.Solver
import Math.Agate.ODE.RealValued

data RosslerVar = X | Y | Z
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | ODE system for the rossler attractor
-- see [here](https://en.wikipedia.org/wiki/R%C3%B6ssler_attractor)
-- values for $ (a, b, c) $ are the values Rossler initially studied that
-- exhibited chaotic behavior
rosslerODE :: RealValuedODE RosslerVar Double
rosslerODE =
    let
        var' = var @(RealValuedODE RosslerVar Double)
     in
        mconcat
            [ X += -(var' Y + var' Z)
            , Y += var' X + a * var' Y
            , Z += b + var' Z * (var' X - c)
            ]
  where
    a = 0.2
    b = 0.6
    c = 5.7

runSolverRossler :: [Double]
runSolverRossler =
    fmap (Map.! X)
        . odeSolve rosslerODE (ODEParams 0.0001)
        $ Map.fromList [(X, 0), (Y, 1), (Z, 0)]
