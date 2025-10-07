{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

module Math.Agate.ODE.Polynomial.Solver where

import Data.Map as Map
import Math.Agate.ODE.Polynomial
import Math.CommutativeAlgebra.Polynomial as Poly
import Data.Maybe

-- import qualified Data.IntMap as Map

-- x' = x
-- exponential :: PolynomialODE Double String
-- exponential = PolynomialODE $ Map.fromList [
--         ("x", -x)
--     ]

-- x :: Num k => GlexPoly k [Char]
-- x = Poly.var "x"

data ODEParams k = ODEParams
  { stepSize :: k
  }

odeSolve :: forall v k. (Num k, Ord v, Show v, Eq k) => PolynomialODE k v -> ODEParams k -> Map v k -> [Map v k]
odeSolve s@(PolynomialODE p) params x0 =
  newValues : odeSolve s params newValues
  where
    varList :: [(GlexPoly k v, k)]
    varList = [(Poly.var vv, value) | (vv, value) <- Map.toList x0]
    newValues :: Map v k
    newValues =
      (flip Map.mapWithKey) p $ \v e ->
        case Map.lookup v x0 of
          Just e' -> e' + (stepSize params) * (Poly.eval e varList)
          Nothing -> error "key not found"

-- // x' = -1/x^2
-- // x = A/t
-- -1 * (x ^ 2)
-- simple ODE example where x' = x
sampleODE :: PolynomialODE Double String
sampleODE =
  PolynomialODE $
    Map.fromList
      [ ("x", x)
      ]
  where
    x = Poly.var "x"

-- 3 * x ^ 2
-- catMaybes ((Map.lookup "x") <$> Prelude.take 10 (odeSolve sampleODE (ODEParams 0.1) (Map.fromList [("x", 1)])))

runSolverExponential :: [Double]
runSolverExponential =
    catMaybes ((Map.lookup "x") <$> Prelude.take 10000 (odeSolve sampleODE (ODEParams 0.0001) (Map.fromList [("x", 1)])))

-- runSolverSIR :: [(Double, Double, Double)]
-- runSolverSIR =
--     catMaybes (lookupSir <$> Prelude.take 100 (
--         odeSolve exampleSIRODE (ODEParams 0.1) (Map.fromList [("S", 0.95), ("I", 0.05), ("R", 0)]))
--         ) where
--             lookupSir m = do
--                 s <- Map.lookup "S" m
--                 i <- Map.lookup "I" m
--                 r <- Map.lookup "R" m
--                 return (s, i, r)


-- forM_ runSolverSIR $ \(s, i, r) -> putStrLn . unwords $ [show s, show i, show r]

-- PolynomialODE (fromList [("I",-0.5I^2+1.0IS+(-2.0e-2)R),("R",(2.0e-2)I),("S",-0.5I^2)])
