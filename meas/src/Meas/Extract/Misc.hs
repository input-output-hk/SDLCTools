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

import qualified  Data.Text as T
import            Data.Time.Calendar
import            Data.Time.Clock
import            Data.Time.Clock.POSIX
import            Data.Time.Format



toDay :: Int -> Day
toDay n = utctDay $ posixSecondsToUTCTime (fromIntegral $ n `div` 1000)



intToDateText :: Int -> T.Text
intToDateText n = T.pack $ formatTime defaultTimeLocale  "%Y%m%d" $ posixSecondsToUTCTime (fromIntegral $ n `div` 1000)

