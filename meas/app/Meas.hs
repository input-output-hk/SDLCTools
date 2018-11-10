{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}


{-# LANGUAGE TemplateHaskell #-}

--import Debug.Trace(trace)

import            Control.Monad
import qualified  Data.ByteString.Lazy as LBS
import qualified  Data.ByteString as BS
import qualified  Data.Csv as CSV
import qualified  Data.List as L
import qualified  Data.Map.Strict as M
import            Data.Time.Calendar
import            Data.Time.Clock
import            Data.String
import            Database.PostgreSQL.Simple

import Meas.Dev.Report.Breakdown
import Meas.Dev.Types
import Meas.Dev.Extractor
import Meas.Dev.Database
import Meas.Misc
import Meas.Dev.Report.Report
import Meas.Config

import Meas.Test.Types
import Meas.Test.Database

main :: IO ()
main = do
  (MkOptions {..}) <- parseCliArgs
  cfg <- readConfig optConfigFile
  print cfg
  run cfg


run :: Config -> IO ()
run cfg@MkConfig{..} = do
  res <- getAll cfg_yt_key cfgDevQueries


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

  -- compute project breakdown

  let periods = let
        days = L.map (\(y, m) -> fromGregorian (fromIntegral y) m 1) [(y, m) | y <- [2018], m <- [1 .. 12]]
        in L.zip days (L.tail days)
  currentDay <- getCurrentTime >>= (return . utctDay)

  let (bds :: [Breakdown]) = L.concatMap (\p@(s, e) -> L.map (\(prj, r) -> MkBreakdown s e prj r) $ M.toList $ projectBreakDown goodTasks currentDay p) periods
  let bdsLBS = CSV.encodeByName defaultBreakdownHeader bds
  LBS.writeFile   "breakdown-by-tasks.csv" LBS.empty
  LBS.appendFile  "breakdown-by-tasks.csv" bdsLBS

  -- Save in database
  conn <- connectPostgreSQL (connectionString cfg) --"host=localhost port=5432 dbname=sdlc_db user=postgres"

  when cfgDevCleanupDB (deleteDevProjectData conn)

  -- save issues
  mapM_ (saveIssue conn) goodIssues

  -- save tasks
  mapM_ (saveTask conn) goodTasks

  -- get data from test projects

  testIssues <- getAllTestIssues cfg_yt_key cfgTestQueries

  -- save in database
  when cfgTestCleanupDB (deleteTestProjectData conn)

  mapM_ (\(_, issues) -> mapM_ (saveTestProjectIssue conn) issues) testIssues


connectionString :: Config -> BS.ByteString
connectionString MkConfig{..} =
  fromString ("host=" ++ cfg_db_host ++ " port=" ++ show cfg_db_port ++ " dbname="
  ++ cfg_db_name ++ " user=" ++ cfg_db_user ++ " password=" ++ cfg_db_pwd)




--          [
          --  ("CBR", "Type:Task sort by: {issue id} asc")
          --  ("CBR", "Type:{User Story} #Bug #Task sort by: {issue id} asc")
--    ("CE", "Type:Task #{User Story} sort by: {issue id} asc")
--      ("GMC", "Type: {User Story} #Bug #Task sort by: {issue id} asc")

          --  ("CDEC", "Type:Task #{User Story} #Bug sort by: {issue id} asc")
           -- ("EC", "Type:Task  #Bug sort by: {issue id} asc")

      --    ("CSL", "Type:Task #Bug sort by: {issue id} desc")
    --     ("DDW", "Type:Task #{User Story} #Bug sort by: {issue id} asc")
--         ("DDW", "Type:Task #{User Story} #Bug sort by: {issue id} asc")
          --   ("CDEC", "Type:Task #{User Story} #Bug sort by: {issue id} asc")
          -- ("CO", "Type:Task #{User Story} #Bug sort by: {issue id} desc")
         --            ("CO", "issue id: CO-14")

--            ("CO", "Type:Task #{User Story} #Bug sort by: {issue id} asc")

--          ("CSM", "Type:Task #{User Story} #Bug sort by: {issue id} asc")
--          ("TSD", "Type:Task #{User Story} #Bug sort by: {issue id} asc")
--          , ("PB", "Type:Task #{User Story} #Bug sort by: {issue id} asc")
--          , ("DEVOPS", "Type:Task  #Bug sort by: {issue id} asc")
--          , ("DDW", "Type:Task #{User Story} #Bug sort by: {issue id} asc")
--          , ("CDEC", "Type:Task #{User Story} #Bug sort by: {issue id} asc")
--          , ("CBR", "Type:Task #{User Story} #Bug sort by: {issue id} asc")
--          , ("CHW", "Type:Task #{User Story} #Bug sort by: {issue id} asc")
--          , ("CO", "Type:Task #{User Story} #Bug sort by: {issue id} asc")
--          , ("QA", "Type:Task sort by: {issue id} asc")

--          ]
