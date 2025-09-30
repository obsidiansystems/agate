{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Math.Agate.Examples.ODE where

import Math.Agate.ODESystem
import Data.Map as M
import Math.CommutativeAlgebra.Polynomial as Poly

-- x' = x
exponential :: PolynomialODE Double String
exponential = PolynomialODE $ M.fromList [
        ("x", -x)
    ]

x :: Num k => GlexPoly k [Char]
x = Poly.var "x"

data ODEParams = ODEParams {
     stepSize :: Double
}

odeSolve :: Num k => PolynomialODE k v -> ODEParams -> [(k, v)] -> [k]
odeSolve p params x0 = x: ()
