module Util where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Parsec
import Text.Parsec.String

natural :: Parser Int
natural = read <$> many1 digit

groupWith :: (Ord k) => (a -> a -> a) -> [(k, a)] -> [(k, a)]
groupWith f = Map.toList . Map.fromListWith f

groupBy' :: (Ord k) => (a -> k) -> [a] -> [(k, [a])]
groupBy' f xs = Map.toList $ Map.fromListWith (++) [(f x, [x]) | x <- xs]

deleteDuplicates :: (Ord a) => [a] -> [a]
deleteDuplicates = Set.toList . Set.fromList

counts :: (Ord k) => [k] -> [(k, Int)]
counts xs = groupWith (+) [(x, 1) | x <- xs]
