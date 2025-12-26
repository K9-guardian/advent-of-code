{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Day_02 where

import Data.List
import qualified Data.Map as Map

input :: IO [String]
input = lines <$> readFile "input/d2.txt"

groupWith f = Map.toList . Map.fromListWith f

counts xs = groupWith (+) [(x, 1) | x <- xs]

exactlyNLetters n xs = n `elem` map snd (counts xs)

p1 xs = length (filter (exactlyNLetters 2) xs) * length (filter (exactlyNLetters 3) xs)

diffBy1 = diffBy1' 0

diffBy1' n [] []
  | n == 1 = True
  | otherwise = False
diffBy1' n (x : xs) (y : ys)
  | x == y = diffBy1' n xs ys
  | otherwise = diffBy1' (succ n) xs ys

-- can we do better than brute force?
p2 xs = find (uncurry diffBy1) pairs
  where
    pairs = [(x, y) | x <- xs, y <- xs]

main = do
  print . p1 =<< input
  print . p2 =<< input
