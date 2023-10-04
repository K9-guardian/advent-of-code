module Day_01 where

import Data.List
import Data.Set qualified as Set
import Text.Read

input :: IO [Int]
input = map parseInt . lines <$> readFile "input/d1.txt"

parseInt :: String -> Int
parseInt ('+' : xs) = read xs
parseInt xs = read xs

firstDup :: Ord a => [a] -> a
firstDup = firstDup' Set.empty

firstDup' s (x : xs)
  | x `Set.member` s = x
  | otherwise = firstDup' (Set.insert x s) xs

p1 = sum

p2 = firstDup . scanl (+) 0 . cycle

main = do
  input >>= print . p1
  input >>= print . p2
