{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Day_04 where

import Data.List (sortOn)
import Data.Ord (Down (Down))
import Data.Time (TimeOfDay (todMin), UTCTime (utctDayTime), timeToTimeOfDay)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Text.Parsec
import Text.Parsec.String (Parser)

natural :: Parser Int
natural = read <$> many1 digit

parseTimestamp :: Parser UTCTime
parseTimestamp = do
  timestamp <- between (char '[') (char ']') (many1 (noneOf "]"))
  maybe (fail $ "Invalid timestamp: " ++ timestamp) return $
    let [date, hourMinute] = words timestamp
     in iso8601ParseM $ concat [date, "T", hourMinute, ":00Z"]

data Action = WakeUp | FallAsleep | BeginShift Int
  deriving (Eq, Show)

parseAction :: Parser Action
parseAction = parseWakeUp <|> parseFallAsleep <|> parseBeginShift
  where
    parseWakeUp = WakeUp <$ string "wakes up"
    parseFallAsleep = FallAsleep <$ string "falls asleep"
    parseBeginShift =
      BeginShift . read
        <$> (string "Guard #" *> many1 digit <* string " begins shift")

parseRecord :: Parser (UTCTime, Action)
parseRecord = do
  timestamp <- parseTimestamp
  _ <- char ' '
  action <- parseAction
  return (timestamp, action)

input :: IO (Either ParseError [(UTCTime, Action)])
input = mapM (parse parseRecord "") . lines <$> readFile "input/d4.txt"

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen p xs = chunk : splitWhen p (dropWhile p rest)
  where
    (chunk, rest) = break p xs

p1 records =
  snd $
    maximum
      [ (sum $ map recordToDuration actions, n)
        | ((_, BeginShift n), actions) <- guardTimelines
      ]
  where
    recordToDuration = \case
      (timestamp, WakeUp) -> todMin $ timeToTimeOfDay $ utctDayTime timestamp
      (timestamp, FallAsleep) -> negate $ todMin $ timeToTimeOfDay $ utctDayTime timestamp
    sortedRecords = take 20 $ sortOn fst records
    isBeginShift = \case BeginShift _ -> True; _ -> False
    guardTimelines =
      zip
        (filter (isBeginShift . snd) sortedRecords)
        (splitWhen (isBeginShift . snd) (tail sortedRecords))

main :: IO ()
main = do
  either (error . show) (print . p1) =<< input
