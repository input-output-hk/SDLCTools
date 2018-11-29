{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}


{-# LANGUAGE TemplateHaskell #-}

module GH.Report.StateTransition
(
  generateStateTransitionReport
)
where

-- import            Debug.Trace (trace)

import qualified  Data.ByteString.Lazy as LBS
import            Data.Csv as CSV
import qualified  Data.List as L
import qualified  Data.Text as T
import            Data.Time.Calendar


import            Control.Monad
import            Control.Applicative
import qualified  Data.List as L
import qualified  Data.Map.Strict as M
import qualified  Data.Text as T
import qualified  Data.Set as S
import            Data.Vector      (toList)

import            Data.Time.Calendar
import            Data.Time.Clock
import            Data.Time.Clock.POSIX
import            Data.Time.Format

import            System.IO

import            GH.StateTransition
import            GH.Types

generateStateTransitionReport :: String -> [Issue] -> IO ()
generateStateTransitionReport filename issues = do
  let issues' =  L.filter hasIllegalStateTransitions issues
  writeFile filename "Illegal Transitions\n"
  mapM_ go issues'
  where
  go issue = do
    appendFile filename "\n"
    mapM (\s -> do
        appendFile filename s
        appendFile filename "\n"
        ) $ stateEventSummary issue
    appendFile filename "\n"
    appendFile filename "-------------------------------\n"
  hasIllegalStateTransitions MkIssue{..} = iStateTransitions == STIllegalStateTransitions




