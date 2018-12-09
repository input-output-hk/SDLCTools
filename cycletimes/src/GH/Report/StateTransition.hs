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

generateStateTransitionReport :: String -> String -> [Issue] -> IO ()
generateStateTransitionReport illegalTransitionsFilename legalTransitionsFilename issues = do
  let issues' =  L.filter hasIllegalStateTransitions issues
  writeFile illegalTransitionsFilename "Illegal Transitions\n"
  mapM_ (go illegalTransitionsFilename) issues'

  let issues'' =  L.filter (not . hasIllegalStateTransitions) issues
  writeFile legalTransitionsFilename "Legal Transitions\n"
  mapM_ (go legalTransitionsFilename) issues''

  where
  go filename issue = do
    appendFile filename "\n"
    mapM_ (\s -> do
        appendFile filename s
        appendFile filename "\n"
        ) $ stateEventSummary issue
    appendFile filename "\n"
    appendFile filename "-------------------------------\n"
  hasIllegalStateTransitions MkIssue{..} = iStateTransitions == STIllegalStateTransitions




