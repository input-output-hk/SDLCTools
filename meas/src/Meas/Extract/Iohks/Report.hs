{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}


{-# LANGUAGE TemplateHaskell #-}

module Meas.Extract.Iohks.Report
where

-- import            Debug.Trace (trace)

import            Data.Csv
import            Data.Monoid ((<>))
import qualified  Data.Text as T
import            Data.Time.Calendar

import            Meas.Extract.Misc
import            Meas.Extract.Report
import            Meas.Extract.Iohks.Types

data IohksReport = IohksReport Day YtIohksIssue

defaultIohksReportHeader :: Header
defaultIohksReportHeader = header
  [ "Iohks IssueId"
  , "Iohks Summary"
  , "Iohks Description"
  , "Iohks Created"
  , "Iohks State"
  , "Iohks Priority"
  , "Iohks Assignees"
  , "Iohks SubSystem"
  , "Iohks Fix Versions"
  , "Iohks Affected Versions"
  , "Iohks Exchange"
  , "Iohks Resolution"
  , "Iohks Platform"

  -- dev issue
  , "IssueId"
  , "Project"
  , "State"
  , "Age"
  , "Squad"
  , "Owner"
  , "Target Version"
  ]






instance ToNamedRecord IohksReport where
    toNamedRecord (IohksReport currentDay MkYtIohksIssue{..}) =
      iohksRecord <> devIssueRecord
      where
      iohksRecord = namedRecord
        [ "Iohks IssueId"             .= _ytIohksIssueId
        , "Iohks Created"             .= intToStdDateText _ytIohksCreated
        , "Iohks Summary"             .= _ytIohksSummary
        , "Iohks Description"         .= _ytIohksDescription
        , "Iohks State"               .= show _ytIohksState
        , "Iohks Priority"            .= show _ytIohksPriority
        , "Iohks Assignees"           .= show _ytIohksAssignees
        , "Iohks SubSystem"           .= _ytIohksSubSystem
        , "Iohks Fix Versions"        .= show _ytIohksFixVersions
        , "Iohks Affected Versions"   .= show _ytIohksAffectedVersions
        , "Iohks Exchange"            .= _ytIohksExchange
        , "Iohks Resolution"          .= _ytIohksResolution
        , "Iohks Platform"            .= _ytIohksPlatform
        ]
      devIssueRecord =
        case _ytIohksDevIssue of
          Nothing       -> namedRecord
                              [ "IssueId"         .= T.empty
                              , "Project"         .= T.empty
                              , "State"           .= T.empty
                              , "Cycle Time"      .= T.empty
                              , "Age"             .= T.empty
                              , "Blocked Days"    .= T.empty
                              , "Squad"           .= T.empty
                              , "Owner"           .= T.empty
                              , "Target Version"  .= T.empty
                              ]
          Just devIssue -> toNamedRecord $ IssueReport currentDay devIssue

instance DefaultOrdered IohksReport where
    headerOrder _ = defaultIohksReportHeader

{-

  "IssueId"       .= _yttTaskId
        , "Project"       .= _yttProject
        , "State"         .= show _yttState
        , "Cycle Time"    .= cycleTime currentDay _yttStateTransitions
        , "Age"           .= ageInCurrentState currentDay _yttCreated _yttChanges
        , "Blocked Days"  .= _yttBlockedDays
        , "3D"            .= show _ytt3D
        , "Assignee-1"    .= a1
        , "Assignee-2"    .= a2
        , "Assignee-3"    .= a3
        , "Parent"        .= _yttParent
        ]
        where
        (a1:a2:a3:_) = _yttAssignees ++ L.repeat ""



data YtIohksIssue = MkYtIohksIssue
  { _ytIohksIssueId           :: T.Text
  , _ytIohksCreated           :: Int
  , _ytIohksProject           :: T.Text
  , _ytIohksNumber            :: Int
  , _ytIohksState             :: IohksStateValue
  , _ytIohksPriority          :: PriorityValue
  , _ytIohksAssignees         :: [T.Text]
  , _ytIohksSubSystem         :: T.Text
  , _ytIohksFixVersions       :: [T.Text]
  , _ytIohksAffectedVersions  :: [T.Text]
  , _ytIohksExchange          :: T.Text
  , _ytIohksResolution        :: T.Text
  , _ytIohksPlatform          :: T.Text
  , _ytIohksErrors            :: [String]
  , _ytIohksLinks             :: [(LinkType, T.Text)]
  , _ytIohksDevIssue          :: Maybe YtIssue
  }
  deriving (Show, Generic, NFData)




data IssueReport = IssueReport Day YtIssue


defaultIssueReportHeader :: Header
defaultIssueReportHeader = header
  [ "IssueId"
  , "Project"
  , "State"
  , "Cycle Time"
  , "Age"
  , "Blocked Days"
  , "Squad"
  , "Owner"
  , "Target Version"
  ]

instance ToNamedRecord IssueReport where
    toNamedRecord (IssueReport currentDay MkYtIssue{..}) = namedRecord
        [ "IssueId"         .= _ytiIssueId
        , "Project"         .= _ytiProject
        , "State"           .= show _ytiState
        , "Cycle Time"      .= cycleTime currentDay _ytiStateTransitions
        , "Age"             .= ageInCurrentState currentDay _ytiCreated _ytiChanges
        , "Blocked Days"    .= _ytiBlockedDays
        , "Squad"           .= _ytiSquad
        , "Owner"           .= _ytiOwner
        , "Target Version"  .= tv
        ]
        where
        (tv:_) = _ytiTargetVersions ++ L.repeat ""


instance DefaultOrdered IssueReport where
    headerOrder _ = defaultIssueReportHeader

-}

