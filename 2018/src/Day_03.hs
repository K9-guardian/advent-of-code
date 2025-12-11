{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Day_03 where

import qualified Data.IntMap as IntMap
import Data.List (find, foldl')
import Data.Maybe
import Text.Parsec
import Text.Parsec.String (Parser)

size = 1000

data Rect = Rect
  { i :: Int,
    r :: Int,
    c :: Int,
    h :: Int,
    w :: Int
  }
  deriving (Show)

natural :: Parser Int
natural = read <$> many1 digit

parseIndex = char '#' >> natural

parsePosition = do
  c <- natural
  _ <- char ','
  r <- natural
  return (r, c)

parseSize = do
  w <- natural
  _ <- char 'x'
  h <- natural
  return (h, w)

parseRect = do
  i <- parseIndex
  _ <- string " @ "
  (r, c) <- parsePosition
  _ <- string ": "
  (h, w) <- parseSize
  return Rect {i, r, c, h, w}

input :: IO (Either ParseError [Rect])
input = mapM (parse parseRect "") . lines <$> readFile "input/d3.txt"

grid = IntMap.fromAscList [(i, 0 :: Int) | i <- [0 .. (size * size - 1)]]

coords Rect {r, c, h, w} = [i * 1000 + j | i <- [r .. (r + h - 1)], j <- [c .. (c + w - 1)]]

-- increment each square corresponding to a claim
claim grid rect = foldl' (flip (IntMap.update (Just . succ))) grid (coords rect)

p1 = length . IntMap.filter (> 1) . foldl' claim grid

noOverlap grid rect = all ((== 1) . fromMaybe (-1) . (`IntMap.lookup` grid)) (coords rect)

p2 rects = maybe (-1) i $ find (noOverlap grid') rects
  where
    grid' = foldl' claim grid rects

main :: IO ()
main = do
  input >>= either print (print . p1)
  input >>= either print (print . p2)
