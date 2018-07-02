{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}


{-# LANGUAGE TemplateHaskell #-}

--import Debug.Trace(trace)

import qualified  Data.ByteString.Lazy as LBS
import qualified  Data.Csv as CSV
import            Data.Time.Clock


import Meas.Extract.Types
import Meas.Extractor
import Meas.Extract.Misc
import Meas.Extract.Report


main :: IO ()
main = do
  (MkOptions {..}) <- parseCliArgs
  run optAuth


run :: String -> IO ()
run authorization = do
  res <- getAll authorization
          [
      --    ("CSL", "Type:Task #Bug sort by: {issue id} desc")
--         ("DDW", "issue id: DDW-10")
--         ("DDW", "Type:Task #{User Story} #Bug sort by: {issue id} asc")
          --   ("CDEC", "Type:Task #{User Story} #Bug sort by: {issue id} asc")
          -- ("CO", "Type:Task #{User Story} #Bug sort by: {issue id} desc")
         --            ("CO", "issue id: CO-14")

          ("DEVOPS", "Type:Task  #Bug sort by: {issue id} asc")
          , ("TSD", "Type:Task #{User Story} #Bug sort by: {issue id} asc")
          , ("PB", "Type:Task #{User Story} #Bug sort by: {issue id} asc")
          , ("DDW", "Type:Task #{User Story} #Bug sort by: {issue id} asc")
          , ("CDEC", "Type:Task #{User Story} #Bug sort by: {issue id} asc")
          , ("CBR", "Type:Task #{User Story} #Bug sort by: {issue id} asc")
          , ("CHW", "Type:Task #{User Story} #Bug sort by: {issue id} asc")
          , ("CO", "Type:Task #{User Story} #Bug sort by: {issue id} asc")
          ]

  let allTasks = do
        (_, tasks, _) <- res
        tasks

  -- only keep task without transition errors and without errors
  let goodTasks = filter (\t -> null (_yttErrors t) && (_yttStateTransitions t) /= STIllegalStateTransitions) allTasks

 -- mapM print goodTasks

  let csvTasksLBS = CSV.encodeByName defaultTaskHeader goodTasks
  LBS.writeFile "cfd-tasks.csv" LBS.empty
  LBS.appendFile "cfd-tasks.csv" csvTasksLBS


  -- saving issues
  let allIssues = do
        (_, _, issues) <- res
        issues

  -- only keep task without transition errors and without errors
  let goodIssues = filter (\t -> null (_ytiErrors t) && (_ytiStateTransitions t) /= STIllegalStateTransitions) allIssues

 -- mapM print goodTasks

  let csvIssuesLBS = CSV.encodeByName defaultIssueHeader goodIssues
  LBS.writeFile "cfd-issues.csv" LBS.empty
  LBS.appendFile "cfd-issues.csv" csvIssuesLBS

  -- create reports
  currentDay <- getCurrentTime >>= (return . utctDay)

  let csvTaskReportLBS = CSV.encodeByName defaultTaskReportHeader $ map (TaskReport currentDay) allTasks
  LBS.writeFile   "task-report.csv" LBS.empty
  LBS.appendFile  "task-report.csv" csvTaskReportLBS

  let csvIssueReportLBS = CSV.encodeByName defaultIssueReportHeader $ map (IssueReport currentDay) allIssues
  LBS.writeFile   "issue-report.csv" LBS.empty
  LBS.appendFile  "issue-report.csv" csvIssueReportLBS



