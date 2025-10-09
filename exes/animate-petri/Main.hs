module Main (main) where

import Data.Bifunctor (first)
import Data.GraphViz
import Data.Map qualified as Map
import Data.Maybe
import Diagrams.Backend.Rasterific (GifLooping (LoopingForever), animatedGif)
import Diagrams.Backend.Rasterific.CmdLine ()
import Diagrams.Prelude hiding (outer, place)
import Math.Agate.Diagrams.PetriNet
import Math.Agate.ODE.Polynomial (PolynomialODE)
import Math.Agate.ODE.Polynomial.Solver
import Math.Agate.PetriNet
import Data.Colour.RGBSpace.HSL
import Data.Colour.RGBSpace (uncurryRGB)

main :: IO ()
main = runRasterificStuff generalSIR

runRasterificStuff :: (forall net. (Place net ~ String, Fractional (Transition net), PetriNet net) => net) -> IO ()
runRasterificStuff pn = do
    pl <- layoutPetri (pn :: PetriNetImpl String Double) Neato
    animatedGif "/tmp/out.gif" (mkSizeSpec $ V2 Nothing $ Just 100) LoopingForever 50
        . take 5
        . map snd
        . filter ((==(1::Integer)) . fst)
        . zip (cycle [1..30])
        $ odeResult pn <&> \m ->
            bg white . drawPetriDynamic sirColour $
                first
                    (fmap $ first \place -> (place, fromMaybe (error "place not found") $ Map.lookup place m))
                    pl

-- TODO DRY
odeResult :: AsODE (PolynomialODE Double String) -> [Map.Map String Double]
odeResult pn =
    odeSolve
        (asODE pn :: PolynomialODE Double String)
        (ODEParams 0.1)
        (Map.fromList [("S", 0.95), ("I", 0.05), ("R", 0)])
generalSIR :: (Place net ~ String, Fractional (Transition net), PetriNet net) => net
generalSIR =
    mconcat
        [ transition ["I", "S"] transmission ["I", "I"]
        , transition ["I"] recovery ["R"]
        ]
  where
    transmission = 0.4
    recovery = 0.03
sirColour :: String -> Colour Double
sirColour = \case
    "R" -> uncurryRGB sRGB $ hsl 120 0.7 0.32
    "I" -> uncurryRGB sRGB $ hsl 0 0.7 0.55
    "S" -> uncurryRGB sRGB $ hsl 240 0.7 0.4
    _ -> black

-- runGlossStuff :: (forall net. (Place net ~ String, Fractional (Transition net), PetriNet net) => net) -> IO ()
-- runGlossStuff pn = do
--     pl <- layoutPetri (pn :: PetriNetImpl String Double) Neato
--     let ls =
--             map
--                 ( \m ->
--                     renderDia
--                         Gloss
--                         ( GlossOptions
--                             { sizeSpec = mkSizeSpec (V2 Nothing (Just 0.001))
--                             , samples = 2
--                             }
--                         )
--                         $ drawPetri'
--                         $ first
--                             ( fmap
--                                 ( first \place ->
--                                     (place, fromMaybe (error "place not found") $ Map.lookup place m)
--                                 )
--                             )
--                             pl
--                 )
--                 $ take 5 $ odeResult pn
--     G.display (InWindow "agate" (1000, 1000) (0, 0)) G.white $
--         head ls
--   where
--     -- animate (InWindow "agate" (1000, 1000) (0, 0)) G.white \t ->
--     --     cycle ls !! floor t
--     -- G.text "hello?"
