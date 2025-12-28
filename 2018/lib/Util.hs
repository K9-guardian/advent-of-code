module Util where

import qualified Data.Map as Map
import Text.Parsec
import Text.Parsec.String

natural :: Parser Int
natural = read <$> many1 digit

groupWith :: (Ord k) => (a -> a -> a) -> [(k, a)] -> [(k, a)]
groupWith f = Map.toList . Map.fromListWith f

counts :: (Ord k) => [k] -> [(k, Int)]
counts xs = groupWith (+) [(x, 1) | x <- xs]
