{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}


{-# LANGUAGE TemplateHaskell #-}

module Meas.Extract.Report
where

-- import            Debug.Trace (trace)

import            Data.Csv
import qualified  Data.List as L
import qualified  Data.Text as T
import            Data.Time.Calendar

import            Meas.Extract.Misc
import            Meas.Extract.State
import            Meas.Extract.Types

data TaskReport = TaskReport Day YtTask

defaultTaskReportHeader :: Header
defaultTaskReportHeader = header
  [ "IssueId"
  , "Project"
  , "Summary"
  , "State"
  , "Cycle Time"
  , "Age"
  , "Blocked Days"
  , "3D"
  , "Assignee-1", "Assignee-2", "Assignee-3"
  , "Parent"
  ]

instance ToNamedRecord TaskReport where
    toNamedRecord (TaskReport currentDay MkYtTask{..}) = namedRecord
        [ "IssueId"       .= _yttTaskId
        , "Project"       .= _yttProject
        , "Summary"       .= _yttSummary
        , "State"         .= show _yttState
        , "Cycle Time"    .= cycleTime currentDay _yttStateTransitions
        , "Age"           .= ageInCurrentState currentDay _yttCreated _yttChanges
        , "Blocked Days"  .= _yttBlockedDays
        , "3D"            .= show _ytt3D
        , "Assignee-1"    .= a1
        , "Assignee-2"    .= a2
        , "Assignee-3"    .= a3
        , "Parent"        .= _yttParent
        , "Last Updated"    .= (maybe T.empty utcTimeToStdDateText  _yttUpdated)
        ]
        where
        (a1:a2:a3:_) = _yttAssignees ++ L.repeat ""



instance DefaultOrdered TaskReport where
    headerOrder _ = defaultTaskReportHeader



data IssueReport = IssueReport Day YtIssue


defaultIssueReportHeader :: Header
defaultIssueReportHeader = header
  [ "IssueId"
  , "Project"
  , "Summary"
  , "Type"
  , "State"
  , "Resolution"
  , "Cycle Time"
  , "Age"
  , "Blocked Days"
  , "Squad"
  , "Owner"
  , "Target Version"
  , "Due Date"
  , "Last Updated"
  ]

instance ToNamedRecord IssueReport where
    toNamedRecord (IssueReport currentDay MkYtIssue{..}) = namedRecord
        [ "IssueId"         .= _ytiIssueId
        , "Project"         .= _ytiProject
        , "Summary"         .= _ytiSummary
        , "Type"            .= show _ytiType
        , "State"           .= show _ytiState
        , "Resolution"      .= show _ytiResolution
        , "Cycle Time"      .= cycleTime currentDay _ytiStateTransitions
        , "Age"             .= ageInCurrentState currentDay _ytiCreated _ytiChanges
        , "Blocked Days"    .= _ytiBlockedDays
        , "Squad"           .= _ytiSquad
        , "Owner"           .= _ytiOwner
        , "Target Version"  .= tv
        , "Due Date"        .= utcTimeToStdDateText _ytiDueDate
        , "Last Updated"    .= (maybe T.empty utcTimeToStdDateText  _ytiUpdated)
        ]
        where
        (tv:_) = _ytiTargetVersions ++ L.repeat ""


instance DefaultOrdered IssueReport where
    headerOrder _ = defaultIssueReportHeader



