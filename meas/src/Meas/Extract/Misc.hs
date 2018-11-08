{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}


{-# LANGUAGE TemplateHaskell #-}

module Meas.Extract.Misc
where

-- import Debug.Trace (trace)

import            Data.Monoid
import qualified  Data.Text as T
import            Data.Time.Calendar
import            Data.Time.Clock
import            Data.Time.Clock.POSIX
import            Data.Time.Format

import            Options.Applicative

defUTCTime :: UTCTime
defUTCTime = UTCTime (ModifiedJulianDay 0) 0


toUTCTime :: Int -> UTCTime
toUTCTime n = posixSecondsToUTCTime (fromIntegral $ n `div` 1000)

previousSecond :: UTCTime -> UTCTime
previousSecond t = addUTCTime  (-1) t

intToDateText :: UTCTime -> T.Text
intToDateText t = T.pack $ formatTime defaultTimeLocale  "%Y%m%d" t


utcTimeToStdDateText :: UTCTime -> T.Text
utcTimeToStdDateText t = T.pack $ formatTime defaultTimeLocale  (iso8601DateFormat Nothing) t

dayToStdDateText :: Day -> T.Text
dayToStdDateText d = T.pack $ formatTime defaultTimeLocale  (iso8601DateFormat Nothing) d


data Options = MkOptions {
  optAuth :: String
  }
  deriving (Show)

optionParser :: Parser Options
optionParser =
  MkOptions
        <$> strOption (
              long "authorization"
              <> short 'a'
              <> (help "YouTrack authorization"))

-- | Parse command line options
parseCliArgs :: IO Options
parseCliArgs = customExecParser (prefs showHelpOnError) (info optionParser fullDesc)




