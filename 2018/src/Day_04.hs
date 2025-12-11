{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Day_04 where

import Data.Time (UTCTime)
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
  deriving (Show)

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

-- TODO: sort actions by timestamp. calculate largest diff

main :: IO ()
main = do
  input >>= print
