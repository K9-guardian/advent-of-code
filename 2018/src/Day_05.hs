module Day_05 where

import Data.Char

input :: IO String
input = head . lines <$> readFile "input/d5.txt"

react :: String -> String
react = foldr react' []
  where
    react' x [] = [x]
    react' x (y : ys)
      | x /= y && toLower x == toLower y = ys
      | otherwise = x : y : ys

p1 :: String -> Int
p1 = length . react

p2 :: String -> Int
p2 polymer = minimum [length $ react $ removeUnit c $ react polymer | c <- ['a' .. 'z']]
  where
    removeUnit c = filter (\c' -> c /= toLower c')

main :: IO ()
main = do
  print . p1 =<< input
  print . p2 =<< input
