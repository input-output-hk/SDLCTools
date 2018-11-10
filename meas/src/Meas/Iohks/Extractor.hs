{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}


{-# LANGUAGE TemplateHaskell #-}

module Meas.Iohks.Extractor

where

-- import Debug.Trace (trace)

import            Data.Aeson
import qualified  Data.Text as T

import            Meas.Issue
import            Meas.Iohks.Types
import            Meas.Iohks.Issue
import            Meas.YouTrack.Queries
import            Meas.Dev.Parser

nbTouchedDays :: Integer
nbTouchedDays = 0

getAll :: String  -> IO [YtIohksIssue]
getAll authorization = do
  issues <- allIssues authorization projectId query >>= (return . extractAllIohksIssues)
  issues' <- mapM (getDevIssue authorization) issues
  --let issues' = issues
  return issues'
  where
  projectId = "IOHKS"
  query = "sort by: {issue id}"

getDevIssue :: String -> YtIohksIssue -> IO YtIohksIssue
getDevIssue authorization issue@(MkYtIohksIssue{..}) = do
  case findDevIssue _ytIohksLinks of
    Nothing -> return issue
    Just devIssueId -> do
      jsonBs <- singleIssueJson authorization (T.unpack devIssueId)
      let (d :: Either String GenericIssue) = eitherDecode jsonBs
      case d of
        Left err -> error err
        Right genericIssue -> do
--          let devIssue = extractIssue genericIssue
          return $ issue {_ytIohksDevIssue = Just $ extractIssue genericIssue}

