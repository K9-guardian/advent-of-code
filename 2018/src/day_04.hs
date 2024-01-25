module Day_04 where

import Data.List
import Text.Parsec
import Text.Parsec.Language (haskellDef)
import Text.Parsec.Token qualified as P

data TimeStamp = TimeStamp
  { year :: Int,
    month :: Int,
    day :: Int,
    hour :: Int,
    minute :: Int
  }
  deriving (Show)

data Action = WakeUp | FallAsleep | BeginShift Int

lexer = P.makeTokenParser haskellDef

natural = fromIntegral <$> P.natural lexer

parseDate = do
  year <- natural
  char '-'
  month <- natural
  char '-'
  day <- natural
  return (year, month, day)

parseTime = do
  hour <- natural
  char ':'
  minute <- natural
  return (hour, minute)

parseTimeStamp = do
  char '['
  (year, month, day) <- parseDate
  char ' '
  (hour, minute) <- parseTime
  return TimeStamp {year, month, day, hour, minute}

-- TODO: sort actions by timestamp. calculate largest diff

-- parseAction = do
--   wakeUp <- string "wakes up"
--   fallAsleep <- string "falls asleep"
--   beginShift <- string "Guard #" >> many digit << string " begins shift"

main = undefined
