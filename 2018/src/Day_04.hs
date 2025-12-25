{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Day_04 where

import Data.Bifunctor (first)
import Data.Function (on)
import Data.Ix (range)
import Data.List (foldl', maximumBy, sortBy)
import Data.List.Split (chunksOf)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Text.Parsec
import Text.Parsec.String (Parser)

natural :: Parser Int
natural = read <$> many1 digit

type Date = String

type Hour = Int

type Minute = Int

data Timestamp = Timestamp
  { date :: String,
    hour :: Hour,
    minute :: Minute
  }
  deriving (Eq, Ord, Show)

parseTimestamp :: Parser Timestamp
parseTimestamp = do
  _ <- char '['
  date <- many1 (noneOf " ")
  _ <- char ' '
  hour <- natural
  _ <- char ':'
  minute <- natural
  _ <- char ']'
  return Timestamp {date, hour, minute}

type Guard = Int

data Action = WakeUp | FallAsleep | BeginShift Guard
  deriving (Eq, Show)

parseAction :: Parser Action
parseAction = parseWakeUp <|> parseFallAsleep <|> parseBeginShift
  where
    parseWakeUp = WakeUp <$ string "wakes up"
    parseFallAsleep = FallAsleep <$ string "falls asleep"
    parseBeginShift = BeginShift <$> (string "Guard #" *> natural <* string " begins shift")

type Record = (Timestamp, Action)

parseRecord :: Parser Record
parseRecord = do
  timestamp <- parseTimestamp
  _ <- char ' '
  action <- parseAction
  return (timestamp, action)

input :: IO (Either ParseError [Record])
input = mapM (parse parseRecord "") . lines <$> readFile "input/d4.txt"

type SleepTimeline = [(Action, Minute)]

type GuardSleepPatterns = Map (Date, Guard) SleepTimeline

updateGuardData :: (GuardSleepPatterns, Guard) -> Record -> (GuardSleepPatterns, Guard)
updateGuardData (guardData, _) (Timestamp {date}, BeginShift guard) = (guardData, guard)
updateGuardData (guardData, currentGuard) (Timestamp {date, minute}, WakeUp) =
  (Map.insertWith (flip (++)) (date, currentGuard) [(WakeUp, minute)] guardData, currentGuard)
updateGuardData (guardData, currentGuard) (Timestamp {date, minute}, FallAsleep) =
  (Map.insertWith (flip (++)) (date, currentGuard) [(FallAsleep, minute)] guardData, currentGuard)

sleepRanges :: SleepTimeline -> [[Int]]
sleepRanges timeline = [range (i, j - 1) | [(FallAsleep, i), (WakeUp, j)] <- chunksOf 2 timeline]

p1 :: [Record] -> Int
p1 records = sleepiestGuard * sleepiestMinute
  where
    (sleepiestMinute, _) =
      maximumBy (compare `on` snd) $
        Map.toList $
          Map.fromListWith
            (+)
            [ (x, 1)
              | x <-
                  concat $
                    Map.fromListWith
                      (++)
                      [(guard, sleepRanges timeline) | ((_, guard), timeline) <- guardData]
                      ! sleepiestGuard
            ]
    (sleepiestGuard, _) =
      maximumBy
        (compare `on` snd)
        $ Map.toList
        $ Map.fromListWith
          (+)
          [(guard, sum $ map length $ sleepRanges timeline) | ((_, guard), timeline) <- guardData]
    (guardData, _) = first Map.toList $ foldl' updateGuardData (Map.empty, firstGuard) sortedRecords
    firstGuard = case head sortedRecords of (_, BeginShift guard) -> guard
    sortedRecords = sortBy (compare `on` fst) records

p2 :: [Record] -> Int
p2 records = sleepiestGuard * sleepiestMinute
  where
    (sleepiestGuard, (sleepiestMinute, _)) =
      maximumBy (compare `on` snd . snd)
        $ Map.toList
        $ Map.map
          ( \ranges ->
              maximumBy (compare `on` snd) $
                Map.toList $
                  Map.fromListWith (+) [(x, 1) | x <- concat ranges]
          )
        $ Map.fromListWith
          (++)
          [(guard, sleepRanges timeline) | ((_, guard), timeline) <- guardData]
    (guardData, _) = first Map.toList $ foldl' updateGuardData (Map.empty, firstGuard) sortedRecords
    firstGuard = case head sortedRecords of (_, BeginShift guard) -> guard
    sortedRecords = sortBy (compare `on` fst) records

main :: IO ()
main = do
  either (error . show) (print . p1) =<< input
  either (error . show) (print . p2) =<< input
