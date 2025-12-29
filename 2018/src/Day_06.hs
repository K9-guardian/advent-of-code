module Day_06 where

import Data.Function
import Data.List
import Data.Maybe
import Debug.Trace
import Text.Parsec
import Text.Parsec.String
import Util

type Coordinate = (Int, Int)

parseCoordinate :: Parser Coordinate
parseCoordinate = do
  x <- natural
  _ <- string ", "
  y <- natural
  return (x, y)

input :: IO (Either ParseError [Coordinate])
input = mapM (parse parseCoordinate "") . lines <$> readFile "input/d6.txt"

distance :: Coordinate -> Coordinate -> Int
distance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

closestPoint :: [Coordinate] -> Coordinate -> Maybe Coordinate
closestPoint coordinates c = case closestPoints of
  [c'] -> Just c'
  _ -> Nothing
  where
    (_, closestPoints) = minimumBy (compare `on` fst) $ groupBy' (distance c) coordinates

boundaryPoints :: [Coordinate] -> [Coordinate]
boundaryPoints coordinates =
  deleteDuplicates $
    mapMaybe
      (closestPoint coordinates)
      (topEdge ++ bottomEdge ++ leftEdge ++ rightEdge)
  where
    topEdge = [(x, 0) | x <- [0 .. 400]]
    bottomEdge = [(x, 400) | x <- [0 .. 400]]
    leftEdge = [(0, y) | y <- [0 .. 400]]
    rightEdge = [(400, y) | y <- [0 .. 400]]

p1 :: [Coordinate] -> Int
p1 coordinates = length largestRegion
  where
    (_, largestRegion) =
      maximumBy (compare `on` (length . snd)) $
        filter (\(c, _) -> c `elem` innerCoordinates) voronoi
    voronoi =
      [ (c, region)
        | (Just c, region) <-
            groupBy'
              (closestPoint coordinates)
              [(x, y) | x <- [0 .. 400], y <- [0 .. 400]]
      ]
    innerCoordinates = coordinates \\ boundaryPoints coordinates

p2 :: [Coordinate] -> Int
p2 coordinates =
  length $
    filter
      (< 10000)
      [sum $ map (distance (x, y)) coordinates | x <- [0 .. 400], y <- [0 .. 400]]

main :: IO ()
main = do
  either (error . show) (print . p1) =<< input
  either (error . show) (print . p2) =<< input
