module Math.Agate.Examples.ODE.Rossler where

import Math.Agate.ODE
import Math.Agate.ODE.RealValued

data RosslerVar = X | Y | Z
    deriving (Show, Eq, Ord, Enum, Bounded)

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
