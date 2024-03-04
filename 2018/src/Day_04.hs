{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Day_04 where

import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (FormatExtension (ExtendedFormat), ISO8601, calendarFormat, hourMinuteFormat, iso8601ParseM, utcTimeFormat, parseFormatExtension, Format (formatReadP))
import Text.Parsec

parseTimestamp = do
  calendar <- formatReadP Day
  return calendar 

test :: Maybe UTCTime
test = iso8601ParseM "1518-09-19T00:42:00Z"

data Action = WakeUp | FallAsleep | BeginShift Int

-- TODO: sort actions by timestamp. calculate largest diff

-- parseAction = do
--   wakeUp <- string "wakes up"
--   fallAsleep <- string "falls asleep"
--   beginShift <- string "Guard #" >> many digit << string " begins shift"

-- main = undefined
