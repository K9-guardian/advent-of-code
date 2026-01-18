{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day_08 where

import Control.Monad
import Control.Monad.State
import Data.Maybe
import Util

data License = License
  { childNodes :: [License],
    metadataEntries :: [Int]
  }
  deriving (Show)

makeLicense :: [Int] -> License
makeLicense = evalState makeLicense'
  where
    makeLicense' :: State [Int] License
    makeLicense' = do
      header <- gets (take 2)
      modify (drop 2)
      childNodes <- replicateM (head header) makeLicense'
      metadataEntries <- gets (take (last header))
      modify (drop (last header))
      return License {childNodes, metadataEntries}

input :: IO [Int]
input = map read . words <$> readFile "input/d8.txt"

p1 :: [Int] -> Int
p1 = sum . concat . extractMetadataEntries . makeLicense
  where
    extractMetadataEntries :: License -> [[Int]]
    extractMetadataEntries License {childNodes, metadataEntries} =
      metadataEntries : concatMap extractMetadataEntries childNodes

p2 :: [Int] -> Int
p2 = value . makeLicense
  where
    value :: License -> Int
    value License {childNodes = [], metadataEntries} = sum metadataEntries
    value License {childNodes, metadataEntries} =
      sum (0 : map value (mapMaybe ((childNodes !?) . pred) metadataEntries))

main :: IO ()
main = do
  print . p1 =<< input
  print . p2 =<< input
