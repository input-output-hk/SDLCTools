{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}



module Meas.Dev.Report.Analytics
(
  generateAnalyticsForIssues
  , generateAnalyticsForTasks
  ,
)
where

-- import            Debug.Trace (trace)

import qualified  Data.ByteString.Lazy as LBS
import            Data.Csv as CSV
import qualified  Data.List as L
import qualified  Data.Text as T

import            Meas.Misc
import            Meas.Dev.Types

generateAnalyticsForIssues :: String -> [YtIssue] -> IO ()
generateAnalyticsForIssues filename issues = do
  let csvIssuesLBS = CSV.encodeByName defaultAnalyticsIssueHeader issues
  LBS.writeFile filename LBS.empty
  LBS.appendFile filename csvIssuesLBS



generateAnalyticsForTasks :: String -> [YtTask] -> IO ()
generateAnalyticsForTasks filename tasks = do
  let csvTasksLBS = CSV.encodeByName defaultAnalyticsTaskHeader tasks
  LBS.writeFile filename LBS.empty
  LBS.appendFile filename csvTasksLBS

defaultAnalyticsTaskHeader :: Header
defaultAnalyticsTaskHeader = header
  [ "IssueId", "Backlog", "Selected"
  , "InProgress", "Review", "Done"
  , "Blocked Days"
  , "Project"
  , "3D"
  , "Assignee-1", "Assignee-2", "Assignee-3"
  , "Parent"
  , "State"
  ]




instance ToNamedRecord YtTask where
    toNamedRecord (MkYtTask{..}) = namedRecord
        [ "IssueId"       .= _yttTaskId
        , "Backlog"       .= maybe T.empty intToDateText (getBacklogTime _yttStateTransitions)
        , "Selected"      .= maybe T.empty intToDateText (getSelectedTime _yttStateTransitions)
        , "InProgress"    .= maybe T.empty intToDateText (getInProgressTime _yttStateTransitions)
        , "Review"        .= maybe T.empty intToDateText (getReviewTime _yttStateTransitions)
        , "Done"          .= maybe T.empty intToDateText (getDoneTime _yttStateTransitions)
        , "Project"       .= _yttProject
        , "3D"            .= show _ytt3D
        , "Assignee-1"    .= a1
        , "Assignee-2"    .= a2
        , "Assignee-3"    .= a3
        , "Parent"        .= _yttParent
        , "State"         .= show _yttState
        , "Blocked Days"  .= _yttBlockedDays
        ]
        where
        (a1:a2:a3:_) = _yttAssignees ++ L.repeat ""

instance CSV.DefaultOrdered YtTask where
    headerOrder _ = defaultAnalyticsTaskHeader



defaultAnalyticsIssueHeader :: Header
defaultAnalyticsIssueHeader = header
  [ "IssueId", "Backlog", "Selected"
  , "InProgress", "Review", "Done"
  , "Blocked Days"
  , "Project"
  , "Squad", "Owner"
  , "Target Version"
  , "Type"
  , "State"
  ]

instance ToNamedRecord YtIssue where
    toNamedRecord (MkYtIssue{..}) = namedRecord
        [ "IssueId"         .= _ytiIssueId
        , "Backlog"         .= maybe T.empty intToDateText (getBacklogTime _ytiStateTransitions)
        , "Selected"        .= maybe T.empty intToDateText (getSelectedTime _ytiStateTransitions)
        , "InProgress"      .= maybe T.empty intToDateText (getInProgressTime _ytiStateTransitions)
        , "Review"          .= maybe T.empty intToDateText (getReviewTime _ytiStateTransitions)
        , "Done"            .= maybe T.empty intToDateText (getDoneTime _ytiStateTransitions)
        , "Type"            .= _ytiType
        , "Project"         .= _ytiProject
        , "Squad"           .= _ytiSquad
        , "Owner"           .= _ytiOwner
        , "Target Version"  .= tv
        , "State"           .= show _ytiState
        , "Blocked Days"    .= _ytiBlockedDays
        ]
        where
        (tv:_) = _ytiTargetVersions ++ L.repeat ""


instance CSV.DefaultOrdered YtIssue where
    headerOrder _ = defaultAnalyticsIssueHeader


