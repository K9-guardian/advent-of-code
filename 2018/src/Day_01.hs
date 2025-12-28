module Day_01 where

import qualified Data.Set as Set
import Text.Parsec
import Text.Parsec.String
import Util

parseChange :: Parser Int
parseChange = do
  sign <- (1 <$ char '+') <|> (-1 <$ char '-')
  change <- natural
  return $ sign * change

input :: IO (Either ParseError [Int])
input = mapM (parse parseChange "") . lines <$> readFile "input/d1.txt"

firstDup :: (Ord a) => [a] -> Maybe a
firstDup = firstDup' Set.empty

firstDup' :: (Ord a) => Set.Set a -> [a] -> Maybe a
firstDup' s (x : xs)
  | x `Set.member` s = Just x
  | otherwise = firstDup' (Set.insert x s) xs
firstDup' _ [] = Nothing

p1 :: [Int] -> Int
p1 = sum

p2 :: [Int] -> Maybe Int
p2 = firstDup . scanl (+) 0 . cycle

main :: IO ()
main = do
  either (error . show) (print . p1) =<< input
  either (error . show) (print . p2) =<< input
