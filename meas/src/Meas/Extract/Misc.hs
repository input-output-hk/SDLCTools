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


toDay :: Int -> Day
toDay n = utctDay $ posixSecondsToUTCTime (fromIntegral $ n `div` 1000)



intToDateText :: Int -> T.Text
intToDateText n = T.pack $ formatTime defaultTimeLocale  "%Y%m%d" $ posixSecondsToUTCTime (fromIntegral $ n `div` 1000)

intToStdDateText :: Int -> T.Text
intToStdDateText n = T.pack $ formatTime defaultTimeLocale  (iso8601DateFormat Nothing) $ posixSecondsToUTCTime (fromIntegral $ n `div` 1000)

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




