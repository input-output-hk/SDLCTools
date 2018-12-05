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

import qualified  Data.List as L

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
    mapM_ (\s -> do
        appendFile filename s
        appendFile filename "\n"
        ) $ stateEventSummary issue
    appendFile filename "\n"
    appendFile filename "-------------------------------\n"
  hasIllegalStateTransitions MkIssue{..} = iStateTransitions == STIllegalStateTransitions




