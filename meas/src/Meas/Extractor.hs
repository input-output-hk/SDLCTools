{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}


{-# LANGUAGE TemplateHaskell #-}

module Meas.Extractor

where

-- import Debug.Trace (trace)

import qualified  Data.Text as T
import            Data.Time.Clock

import            Meas.Extract.Efficiency
import            Meas.Extract.Issue
import            Meas.Extract.State
import            Meas.Extract.Types
import            Meas.YouTrack.Queries

nbTouchedDays :: Integer
nbTouchedDays = 0

getSingleIssue :: String -> String -> IO ([YtTask], [YtIssue])
getSingleIssue authorization issueId = do
 res <- singleIssue authorization issueId
 case res of
    Right gissues -> do
      let (tasks, issues) = extractAllIssues [gissues]
      return (tasks, issues)
    Left err -> do
      putStrLn ("Error cannot parse: "++ issueId )
      return ([], [])

getAllNoHistory :: String -> [(String, String)] -> IO [(String, [YtTask], [YtIssue])]
getAllNoHistory authorization projects = do
  mapM getOneproject projects
  where
  getOneproject (projectId, query) = do
    print $ "Extracting project: "++ projectId
    gissues <- allIssues authorization projectId query
    print ("Found # issues: " ++ (show $ length gissues))
    let (tasks, issues) = extractAllIssues gissues
    return (projectId, tasks, issues)


getAll :: String -> [(String, String)] -> IO [(String, [YtTask], [YtIssue])]
getAll authorization projects = do
  mapM getOneproject projects
  where
  getOneproject (projectId, query) = do
    print $ "Extracting project: "++ projectId
    gissues <- allIssues authorization projectId query
    print ("Found # issues: " ++ (show $ length gissues))
    let (tasks, issues) = extractAllIssues gissues
    tasks'  <- mapM (getHistoryForTask  authorization) tasks
    issues' <- mapM (getHistoryForIssue authorization) issues
    return (projectId, tasks', issues')

getHistoryForTask :: String -> YtTask -> IO YtTask
getHistoryForTask authorization task = do
  putStrLn $ "History for Task: " ++ T.unpack (_yttTaskId task)
  changes <- allChangesForIssue authorization (_yttTaskId task)
  --let !evChanges = force changes
  let !stateTransitions = getStateTransitions (_yttCreated task) changes
  currentDay <- getCurrentTime >>= (return . utctDay)
  let blockedDays = getBlockedDays currentDay nbTouchedDays stateTransitions changes
  return $ task {_yttChanges = changes, _yttStateTransitions = stateTransitions, _yttBlockedDays = blockedDays}

getHistoryForIssue :: String -> YtIssue -> IO YtIssue
getHistoryForIssue authorization issue = do
  putStrLn $ "History for Issue: " ++ T.unpack (_ytiIssueId issue)
  changes <- allChangesForIssue authorization (_ytiIssueId issue)
  let !stateTransitions = getStateTransitions (_ytiCreated issue) changes
  currentDay <- getCurrentTime >>= (return . utctDay)
  let blockedDays = getBlockedDays currentDay nbTouchedDays stateTransitions changes
  return $ issue {_ytiChanges = changes, _ytiStateTransitions = stateTransitions, _ytiBlockedDays = blockedDays}

