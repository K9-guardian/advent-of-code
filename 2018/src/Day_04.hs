{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

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

-- TODO: sort actions by timestamp. calculate largest diff

-- parseAction = do
--   wakeUp <- string "wakes up"
--   fallAsleep <- string "falls asleep"
--   beginShift <- string "Guard #" >> many digit << string " begins shift"

-- main = undefined
