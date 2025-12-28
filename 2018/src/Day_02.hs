module Day_02 where

import Data.List
import Util

input :: IO [String]
input = lines <$> readFile "input/d2.txt"

exactlyNLetters :: (Ord a) => Int -> [a] -> Bool
exactlyNLetters n xs = n `elem` map snd (counts xs)

p1 :: [String] -> Int
p1 xs = length (filter (exactlyNLetters 2) xs) * length (filter (exactlyNLetters 3) xs)

diffBy1 :: String -> String -> Bool
diffBy1 = diffBy1' 0

diffBy1' :: Int -> String -> String -> Bool
diffBy1' n [] []
  | n == 1 = True
  | otherwise = False
diffBy1' n (x : xs) (y : ys)
  | x == y = diffBy1' n xs ys
  | otherwise = diffBy1' (succ n) xs ys
diffBy1' _ _ _ = undefined

-- Can we do better than brute force?
p2 :: [String] -> Maybe (String, String)
p2 xs = find (uncurry diffBy1) pairs
  where
    pairs = [(x, y) | x <- xs, y <- xs]

main :: IO ()
main = do
  print . p1 =<< input
  print . p2 =<< input
