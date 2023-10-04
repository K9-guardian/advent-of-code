module Day_02 where

import Data.List
import Data.Map qualified as Map

input :: IO [String]
input = lines <$> readFile "input/d2.txt"

exactly2Letters xs = 2 `elem` vals
  where
    vals = map snd $ Map.toList $ Map.fromListWith (+) [(c, 1) | c <- xs]

exactly3Letters xs = 3 `elem` vals
  where
    vals = map snd $ Map.toList $ Map.fromListWith (+) [(c, 1) | c <- xs]

p1 xs = length (filter exactly2Letters xs) * length (filter exactly3Letters xs)

-- can we use p1 here?
p2 = undefined

main = do
  input >>= print . p1

-- input >>= print . p2
