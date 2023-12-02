module Day_03 where

import qualified Data.IntMap as IntMap
import Data.List
import Data.Maybe
import GHC.Real (infinity)
import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)

size = 1000

data Rect = Rect
  { i :: Int,
    r :: Int,
    c :: Int,
    h :: Int,
    w :: Int
  }
  deriving (Show)

lexer = P.makeTokenParser haskellDef
natural = fromIntegral <$> P.natural lexer

parseIndex = char '#' >> natural

parsePosition = do
  c <- natural
  char ','
  r <- natural
  return (r, c)

parseSize = do
  w <- natural
  char 'x'
  h <- natural
  return (h, w)

parseRect = do
  i <- parseIndex
  string " @ "
  (r, c) <- parsePosition
  string ": "
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
