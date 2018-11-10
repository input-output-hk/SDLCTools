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


main = return ()
{-}
import Meas.Iohks.Extractor
import Meas.Iohks.Report
import Meas.Config
import Meas.Misc



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


-}

