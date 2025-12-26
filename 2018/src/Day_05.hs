{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Day_05 where

import Data.Char (toLower)

input :: IO String
input = head . lines <$> readFile "input/d5.txt"

react = foldr react' []
  where
    react' x [] = [x]
    react' x (y : ys)
      | x /= y && toLower x == toLower y = ys
      | otherwise = x : y : ys

p1 = length . react

p2 polymer =
  minimum
    [ length $
        react $
          filter
            (\c' -> c /= toLower c')
            (react polymer)
      | c <- ['a' .. 'z']
    ]

main :: IO ()
main = do
  print . p1 =<< input
  print . p2 =<< input
