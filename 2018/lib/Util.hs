module Util where

import qualified Data.Map as Map
import Debug.Trace
import Text.Parsec
import Text.Parsec.String

traceMsg :: (Show a) => String -> a -> a
traceMsg s x = trace (s ++ ": " ++ show x) x

natural :: Parser Int
natural = read <$> many1 digit

groupWith :: (Ord k) => (a -> a -> a) -> [(k, a)] -> [(k, a)]
groupWith f = Map.toList . Map.fromListWith f

groupBy' :: (Ord k) => (a -> k) -> [a] -> [(k, [a])]
groupBy' f xs = groupWith (++) [(f x, [x]) | x <- xs]

counts :: (Ord k) => [k] -> [(k, Int)]
counts xs = groupWith (+) [(x, 1) | x <- xs]

assoc :: (Eq k) => k -> v -> [(k, v)] -> [(k, v)]
assoc _ _ [] = []
assoc k v ((k', v') : kvs)
  | k == k' = (k, v) : kvs
  | otherwise = (k', v') : assoc k v kvs

update :: (Eq k) => k -> (v -> v) -> [(k, v)] -> [(k, v)]
update _ _ [] = []
update k f ((k', v) : kvs)
  | k == k' = (k, f v) : kvs
  | otherwise = (k', v) : update k f kvs
