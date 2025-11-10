module Utils where

import Control.Applicative
import Data.List
import Data.List.Extra
import Data.Maybe

adjacentPairs :: [a] -> [(a, a)]
adjacentPairs = \case
    [] -> []
    x : xs -> zip (x : xs) xs

mapColumns :: ([a] -> [b]) -> [[a]] -> [[b]]
mapColumns f = transpose . zipWithN f

zipWithN :: (Traversable t) => (t a -> b) -> t [a] -> [b]
zipWithN f xs = getZipList $ f <$> traverse ZipList xs

-- | Samples every n'th element. Argument must be positive.
takeEvery :: Int -> [a] -> [a]
takeEvery n = map (fromMaybe (error "takeEvery: empty chunk") . listToMaybe) . chunksOf n
