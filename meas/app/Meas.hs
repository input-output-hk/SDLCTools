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
import qualified  Data.ByteString as BS
import            Data.Time.Clock
import            Data.String
import            Database.PostgreSQL.Simple

--import            Meas.Imports

import            Meas.Dev.Types
import            Meas.Dev.Extractor
import            Meas.Dev.Database
import            Meas.Misc
import            Meas.Dev.Report.Analytics
import            Meas.Dev.Report.Report
import            Meas.Config

import            Meas.Test.Extractor
import            Meas.Test.Database

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

  generateAnalyticsForTasks "cfd-tasks.csv" goodTasks

  -- saving issues
  let allIssues = do
        (_, _, issues) <- res
        issues

  -- only keep task without transition errors and without errors
  let goodIssues = filter (\t -> null (_ytiErrors t) && (_ytiStateTransitions t) /= STIllegalStateTransitions) allIssues

  generateAnalyticsForIssues "cfd-issues.csv" goodIssues


  -- create reports
  currentDay <- getCurrentTime >>= (return . utctDay)

  generateReportForTasks "task-report.csv" currentDay allTasks
  generateReportForIssues "issue-report.csv" currentDay allIssues


  -- Save in database
  conn <- connectPostgreSQL (connectionString cfg) --"host=localhost port=5432 dbname=sdlc_db user=postgres"

  when (cfgDevCleanupDB && not (null goodIssues)) (deleteDevProjectData conn)

  -- save issues
  mapM_ (saveIssue conn) goodIssues

  -- save tasks
  mapM_ (saveTask conn) goodTasks

  -- get data from test projects

  testIssues <- getAllTestIssues cfg_yt_key cfgTestQueries

  -- save in database
  when (cfgTestCleanupDB && not (null testIssues)) (deleteTestProjectData conn)

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
