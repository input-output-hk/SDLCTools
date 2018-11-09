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


import Meas.Extract.Iohks.Extractor
import Meas.Extract.Iohks.Report
import Meas.Extract.Config
import Meas.Extract.Misc



main :: IO ()
main = do
  (MkOptions {..}) <- parseCliArgs
  (cfg::Config) <- readConfig optConfigFile
  run (cfg_yt_key cfg)


run :: String -> IO ()
run authorization = do
  issues <- getAll authorization

  -- create reports
  currentDay <- getCurrentTime >>= (return . utctDay)

  let csvIohksReportLBS = CSV.encodeByName defaultIohksReportHeader $ map (IohksReport currentDay) issues
  LBS.writeFile   "iohks-report.csv" LBS.empty
  LBS.appendFile  "iohks-report.csv" csvIohksReportLBS




