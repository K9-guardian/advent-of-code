module Day_01 where

import qualified Data.Set as Set

input :: IO [Int]
input = map parseInt . lines <$> readFile "input/d1.txt"

parseInt :: String -> Int
parseInt = read . dropWhile (== '+')

firstDup :: (Ord a) => [a] -> Maybe a
firstDup = firstDup' Set.empty

firstDup' :: (Ord a) => Set.Set a -> [a] -> Maybe a
firstDup' s (x : xs)
  | x `Set.member` s = Just x
  | otherwise = firstDup' (Set.insert x s) xs
firstDup' _ [] = Nothing

p1 :: [Int] -> Int
p1 = sum

p2 :: [Int] -> Maybe Int
p2 = firstDup . scanl (+) 0 . cycle

main :: IO ()
main = do
  print . p1 =<< input
  print . p2 =<< input
