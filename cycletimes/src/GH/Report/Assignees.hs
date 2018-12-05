{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}


{-# LANGUAGE TemplateHaskell #-}

module GH.Report.Assignees
(
  generateAssigneeIssueReport
  , generateIssueAssigneeReport
)
where

-- import            Debug.Trace (trace)

import qualified  Data.ByteString.Lazy as LBS
import            Data.Csv as CSV
import qualified  Data.List as L
import qualified  Data.Map.Strict as M
import qualified  Data.Set as S
import qualified  Data.Text as T


import            GH.Types
import            GH.DevNames

data AssigneeIssueReport = AssigneeIssueReport T.Text Int [T.Text]

generateAssigneeIssueReport :: String -> M.Map GHUser (S.Set GHIssue) -> IO ()
generateAssigneeIssueReport filename assignements = do
  let csvIssueReportLBS = CSV.encodeByName defaultAssigneeIssueReportHeader rows
  LBS.writeFile  filename LBS.empty
  LBS.appendFile filename csvIssueReportLBS

  where
  rows = L.map toRow $ M.toList assignements
  toRow (assignee, issues) = let
    urls = L.map ghiUrl (S.toList issues) ++ L.repeat ""
    in AssigneeIssueReport (toRealName $ ghuUser assignee) (S.size issues) urls




defaultAssigneeIssueReportHeader :: Header
defaultAssigneeIssueReportHeader = header
  [ "Assignee"
  , "Nb Issues"
  , "Issue-1"
  , "Issue-2"
  , "Issue-3"
  , "Issue-4"
  , "Issue-5"
  ]


instance ToNamedRecord AssigneeIssueReport where
    toNamedRecord (AssigneeIssueReport assignee nbIssues issues) = namedRecord
        [ "Assignee"       .= assignee
        , "Nb Issues"      .= nbIssues
        , "Issue-1"        .= i0
        , "Issue-2"        .= i1
        , "Issue-3"        .= i2
        , "Issue-4"        .= i3
        , "Issue-5"        .= i4
        ]
        where
        (i0:i1:i2:i3:i4:_) = issues



instance CSV.DefaultOrdered AssigneeIssueReport where
    headerOrder _ = defaultAssigneeIssueReportHeader



data IssueAssigneeReport = IssueAssigneeReport T.Text Int [T.Text]



generateIssueAssigneeReport :: String -> M.Map GHIssue (S.Set GHUser) -> IO ()
generateIssueAssigneeReport filename assignements = do
  let csvIssueReportLBS = CSV.encodeByName defaultIssueAssigneeReportHeader rows
  LBS.writeFile  filename LBS.empty
  LBS.appendFile filename csvIssueReportLBS

  where
  rows = L.map toRow $ M.toList assignements
  toRow (issue, assignees) = let
    assigneeNames = L.map ghuUser (S.toList assignees) ++ L.repeat ""
    in IssueAssigneeReport (ghiUrl issue) (S.size assignees) (toRealName <$> assigneeNames)



defaultIssueAssigneeReportHeader :: Header
defaultIssueAssigneeReportHeader = header
  [ "Issue"
  , "Nb Assignees"
  , "Assignee-1"
  , "Assignee-2"
  , "Assignee-3"
  , "Assignee-4"
  , "Assignee-5"
  ]

instance ToNamedRecord IssueAssigneeReport where
    toNamedRecord (IssueAssigneeReport issue nbAssignees assignees) = namedRecord
        [ "Issue"             .= issue
        , "Nb Assignees"      .= nbAssignees
        , "Assignee-1"        .= a0
        , "Assignee-2"        .= a1
        , "Assignee-3"        .= a2
        , "Assignee-4"        .= a3
        , "Assignee-5"        .= a4
        ]
        where
        (a0:a1:a2:a3:a4:_) = assignees



instance CSV.DefaultOrdered IssueAssigneeReport where
    headerOrder _ = defaultIssueAssigneeReportHeader
{-}

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


instance CSV.DefaultOrdered IssueReport where
    headerOrder _ = defaultIssueReportHeader

-}

