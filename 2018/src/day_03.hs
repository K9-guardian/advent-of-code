module Day_03 where

import Data.IntMap qualified as IntMap
import Data.List
import Data.Maybe
import GHC.Real (infinity)
import Text.Parsec

size = 1000

data Rect = Rect
  { i :: Int,
    r :: Int,
    c :: Int,
    h :: Int,
    w :: Int
  }
  deriving (Show)

parseRect = do
  i <- char '#' >> many digit
  c <- string " @ " >> many digit
  r <- char ',' >> many digit
  w <- string ": " >> many digit
  h <- char 'x' >> many digit
  return Rect {i = read i, r = read r, c = read c, h = read h, w = read w}

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
