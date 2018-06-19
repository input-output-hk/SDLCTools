{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}


{-# LANGUAGE TemplateHaskell #-}

module Types where


--import Lib

import Data.List.Split (splitOn)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

import qualified Data.Text as T

import Data.Time.Clock.POSIX
import Data.Time.Format
import Data.Csv

import Data.Time.Clock

--d = print $ formatTime defaultTimeLocale  "%Y%m%d" $ posixSecondsToUTCTime 1525111273




data MeasRecord = MkMeasRecord
  { mrIssueId         :: T.Text
  , mrProject         :: T.Text
  , mrBacklog         :: Int
  , mrInProgress      :: Int
  , mrInProgressDone  :: Maybe Int
  , mrReview          :: Maybe Int
  , mrReviewDone      :: Maybe Int
  , mrDone            :: Maybe Int
  }

intToDateText :: Int -> T.Text
intToDateText n = T.pack $ formatTime defaultTimeLocale  "%Y%m%d" $ posixSecondsToUTCTime (fromIntegral $ n `div` 1000)

--1525111273

instance ToNamedRecord MeasRecord where
    toNamedRecord (MkMeasRecord{..}) = namedRecord
        [ "IssueId" .= mrIssueId
        , "Backlog" .= (intToDateText mrBacklog)
        , "InProgress" .= (intToDateText mrInProgress), "InProgressDone" .= maybe T.empty  intToDateText mrInProgressDone
        , "Review"  .= maybe T.empty intToDateText mrReview, "ReviewDone"  .= maybe T.empty intToDateText mrReviewDone
        , "Done" .= maybe T.empty intToDateText mrDone
        , "Project" .= mrProject
        ]

instance DefaultOrdered MeasRecord where
    headerOrder _ = header ["IssueId", "Backlog", "InProgress", "InProgressDone", "Review", "ReviewDone", "Done", "Project"]


