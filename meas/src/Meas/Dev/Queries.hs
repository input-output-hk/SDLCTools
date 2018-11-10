{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}


{-# LANGUAGE TemplateHaskell #-}

module Meas.Dev.Queries
where

-- import Debug.Trace (trace)


import            Data.Aeson
import qualified  Data.ByteString as BS
import qualified  Data.ByteString.Char8 as BS8
import qualified  Data.ByteString.Lazy as LBS
import qualified  Data.List as L
import qualified  Data.Text as T
import            Data.Time.Clock

import            Network.HTTP.Simple as HTTP

import            Text.XML.JSON.StreamingXmlToJson

import            Meas.Misc
import            Meas.Dev.Types
import            Meas.Dev.Parser  -- TODO should we need it?
import            Meas.YouTrack.Queries




singleIssue :: String -> String -> IO (Either String GenericIssue)
singleIssue authorization issueId = do
  jsonBs <- singleIssueJson authorization issueId
  return $ eitherDecode jsonBs


allIssues :: String -> String -> String -> IO [GenericIssue]
allIssues authorization projectId query = do
  jsonBs <- allIssuesForProjectJson authorization projectId query
  let (d :: Either String GenericIssues) = eitherDecode jsonBs
  case d of
    Left err -> error err
    Right (GenericIssues gIssues) -> do
      return $ gIssues

allChangesForIssue :: String -> T.Text -> IO [(UTCTime, [ValueChange])]
allChangesForIssue authorization issueId = do
  jsonBs <- changesForIssueJson authorization issueId
  let (d :: Either String Hist) = eitherDecode jsonBs
  case d of
    Left err -> error err
    Right (Hist _ changes) -> return $ groupChanges changes


{-
Group changes by update (change) time. Sort the resulting list by asceding times.
Precondition:
* each `[ValueChange]` contains one and only one `UpdateTime`.
-}
groupChanges :: [[ValueChange]] -> [(UTCTime, [ValueChange])]
groupChanges allChanges =
  L.sortOn fst $ L.map go allChanges
  where
  go l =
    case L.foldl proc (Nothing, []) l of
    (Just t, changes) -> (t, changes)
    _ -> error "Bad YouTrack changes"
  proc (_, changes) (UpdateTime t) = (Just (toUTCTime t), changes)
  proc (time, changes) change = (time, change:changes)


