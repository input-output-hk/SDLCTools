{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}


{-# LANGUAGE TemplateHaskell #-}

module Meas.Extract.Types
where

-- import            Debug.Trace (trace)

import            Control.DeepSeq
import            Control.Lens hiding (element, (.=))

import            Data.Csv
import qualified  Data.List as L
import            Data.Text.Conversions
import qualified  Data.Text as T

import            Data.Time.Clock

import            GHC.Generics (Generic)

import Meas.Extract.Misc


data PriorityValue =
  ShowStopper
  |Critical
  |Major
  |Normal
  |Minor
  deriving (Eq, Show, Generic, NFData)

data IohksStateValue =
   IohksSubmitted
  |IohksReadyToSolve
  |IohksFixed
  |IohksDone
  deriving (Eq, Generic, NFData)


data StateValue =
  Backlog
  |Planning
  |Selected
  |InProgress
  |Review
  |Done
  |Neutral
  deriving (Eq, Show, Generic, NFData)

data WaitValue =
  Running
  |Waiting
  deriving (Eq, Show, Generic, NFData)

data TypeValue =
  TaskType
  |IssueType
  |OtherType
  deriving (Eq, Show, Generic, NFData)

data ThreeDValue =
  Design
  |Development
  |Documentation
  |Test
  deriving (Eq, Show, Generic, NFData)

data ROMMandaysValue =
  Days
  |Weeks
  |Months
  |Quarters
  deriving (Eq, Show, Generic, NFData)

data ResolutionValue =
    Successful
  | Aborted
  | Duplicate
  | Obsolete
  deriving (Eq, Show, Generic, NFData)

data LinkType =
  ParentFor
  |SubTaskOf
  |MustStartAfter
  |IsPreRequisiteFor
  |DependsOn
  |Duplicates
  |RelatesTo
  |IsDuplicatedBy
  |IsRequiredFor
  |TestOf
  |TestedBy
  deriving (Eq, Show, Generic, NFData)

data ValueChange =
  UpdateTime Int
  | Updater T.Text
  | StateChange StateValue StateValue
  | WaitChange WaitValue WaitValue
  deriving (Eq, Show, Generic, NFData)


{-
All possible histories of state transition for an issue.
-}
data StateTransitions =
    STBacklog UTCTime
  | STSelected UTCTime UTCTime
  | STInProgress UTCTime UTCTime UTCTime
  | STInReview UTCTime UTCTime UTCTime UTCTime
  | STDone UTCTime UTCTime UTCTime UTCTime UTCTime
  | STIllegalStateTransitions
  deriving (Eq, Show, Generic, NFData)

data YtIssue = MkYtIssue
  { _ytiIssueId           :: T.Text
  , _ytiType              :: T.Text
  , _ytiSummary           :: T.Text
  , _ytiDescription       :: T.Text
  , _ytiCreated           :: UTCTime
  , _ytiUpdated           :: Maybe UTCTime
  , _ytiProject           :: T.Text
  , _ytiNumber            :: Int
  , _ytiState             :: StateValue
  , _ytiWait              :: WaitValue
  , _ytiDueDate           :: UTCTime
  , _ytiROMManday         :: Maybe ROMMandaysValue
  , _ytiPPriorities       :: (Int, Int, Int)
  , _ytiSquad             :: Maybe T.Text
  , _ytiOwner             :: Maybe T.Text
  , _ytiPotentialSquad    :: [T.Text]
  , _ytiTargetVersions    :: [T.Text]
  , _ytiResolution        :: ResolutionValue
  , _ytiLinks             :: [(LinkType, T.Text)]
  , _ytiChanges           :: [(UTCTime, [ValueChange])]
  , _ytiStateTransitions  :: StateTransitions
  , _ytiBlockedDays       :: Integer
  , _ytiErrors            :: [String]
  }
  deriving (Show, Generic, NFData)

makeLenses ''YtIssue

defaultIssue :: YtIssue
defaultIssue = MkYtIssue
  T.empty T.empty T.empty T.empty defUTCTime Nothing T.empty 0 Backlog Running defUTCTime Nothing (0, 0, 0)
  Nothing Nothing [] [] Successful [] [] STIllegalStateTransitions 0 []


data YtTask = MkYtTask
  { _yttTaskId            :: T.Text
  , _yttSummary           :: T.Text
  , _yttDescription       :: T.Text
  , _yttCreated           :: UTCTime
  , _yttUpdated           :: Maybe UTCTime
  , _yttProject           :: T.Text
  , _yttNumber            :: Int
  , _yttState             :: StateValue
  , _yttWait              :: WaitValue
  , _ytt3D                :: ThreeDValue
  , _yttAssignees         :: [T.Text]
  , _yttLinks             :: [(LinkType, T.Text)]
  , _yttChanges           :: [(UTCTime, [ValueChange])]
  , _yttStateTransitions  :: StateTransitions
  , _yttBlockedDays       :: Integer
  , _yttParent            :: T.Text
  , _yttErrors            :: [String]
  }
  deriving (Show, Generic, NFData)

makeLenses ''YtTask

defaultTask :: YtTask
defaultTask = MkYtTask T.empty T.empty T.empty defUTCTime Nothing T.empty 0 Backlog Running Development [] [] [] STIllegalStateTransitions 0 T.empty []


instance FromText StateValue where
  fromText "Backlog"                            = Backlog
  fromText "Planning"                           = Planning
  fromText "Selected"                           = Selected
  fromText "In Progress"                        = InProgress
  fromText "Review"                             = Review
  fromText "Done"                               = Done
  fromText "Backlog (pool of Ideas)"            = Backlog
  fromText "To Verify"                          = Backlog

-- old stuff in YouTrack ...
  fromText "Closed"                             = Done
  fromText "No State"                           = Backlog
  fromText "No state"                           = Backlog
  fromText "Open"                               = Backlog
  fromText "Assigned"                           = Selected
  fromText "No value"                           = Backlog
  fromText "Waiting for review"                 = Review
  fromText "DONT-USE-1"                         = Backlog
  fromText "DONT-USE-3"                         = Backlog
  fromText "DONT-USE-5"                         = Backlog
  fromText "Aborted"                            = Done
  fromText "To be discussed"                    = Backlog
  fromText "Waiting for build"                  = Backlog
  fromText "79-532-1524688761506.Done"          = Done
  fromText "79-532-1524688761506.Backlog"       = Backlog
  fromText "79-532-1524688761506.In Progress"   = InProgress
  fromText "Duplicate"                          = Done
  fromText "Verified"                           = Done
  fromText "Pool of Ideas"                      = Backlog
  fromText "Obsolete"                           = Done
  fromText "Submitted"                          = Backlog
  fromText "Can't Reproduce"                    = Done
  fromText "Blocking"                           = Done
  fromText "Postponed"                          = Backlog
  fromText "Waiting for test"                   = InProgress
  fromText "Waiting to be merged into `master`" = InProgress
  fromText "To be deployed to staging"          = InProgress
  fromText "To be deployed to production"       = InProgress
  fromText "Ready to Solve"                     = Selected
  fromText "Ready"                              = Selected
  fromText "Blocked"                            = Neutral
  fromText "Merged"                             = Done
  fromText "Cancel"                             = Done
  fromText "Close"                              = Done

  fromText s                                    = error $ ("Unknow State: "++T.unpack s)



instance FromText ROMMandaysValue where
  fromText "Day"      = Days
  fromText "Week"     = Weeks
  fromText "Month"    = Months
  fromText "Quarter"  = Quarters
  fromText s          = error ("Unknown ROMMandays: " ++ T.unpack s)



instance FromText WaitValue where
  fromText "Running"        = Running
  fromText "Waiting"        = Waiting
  fromText "No wait"        = Running
  fromText "<lost change>"  = Running
  fromText s                = error $ ("Unknow Wait: "++ T.unpack s)



instance FromText TypeValue where
  fromText "User Story" = IssueType
  fromText "Bug"        = IssueType
  fromText "Task"       = TaskType
  fromText "Issue"      = IssueType
  fromText _            = OtherType



instance FromText ThreeDValue where
  fromText "Design"         = Design
  fromText "Development"    = Development
  fromText "Documentation"  = Documentation
  fromText "Test"           = Test
  fromText s                = error ("Unknown 3D: " ++ T.unpack s)



instance FromText LinkType where
  fromText "parent for"             = ParentFor
  fromText "subtask of"             = SubTaskOf
  fromText "must start after"       = MustStartAfter
  fromText "is a prerequisite for"  = IsPreRequisiteFor
  fromText "depends on"             = DependsOn
  fromText "duplicates"             = Duplicates
  fromText "relates to"             = RelatesTo
  fromText "is duplicated by"       = IsDuplicatedBy
  fromText "is required for"        = IsRequiredFor
  fromText "tested by"              = TestedBy
  fromText "test of"                = TestOf
  fromText s                        = error ("unknow link role: "++T.unpack s)


instance FromText PriorityValue where
  fromText "Show-stopper" = ShowStopper
  fromText "Critical"     = Critical
  fromText "Major"        = Major
  fromText "Normal"       = Normal
  fromText "Minor"        = Minor
  fromText s              = error ("unknow Priority :"++T.unpack s)

instance FromText IohksStateValue where
  fromText "Submitted"      = IohksSubmitted
  fromText "Ready to Solve" = IohksReadyToSolve
  fromText "Fixed"          = IohksFixed
  fromText "Done"           = IohksDone
  fromText s                = error ("unknow Iohks State: "++T.unpack s)


instance FromText ResolutionValue where
  fromText "Successful" = Successful
  fromText "Aborted"    = Aborted
  fromText "Duplicate"  = Duplicate
  fromText "Obsolete"   = Obsolete
  fromText s            = error ("unknow Resolution: "++T.unpack s)

instance Show IohksStateValue where
  show IohksSubmitted     = "Submitted"
  show IohksReadyToSolve  = "Ready to Solve"
  show IohksFixed         = "Fixed"
  show IohksDone          = "Done"


-- Misc functions



getBacklogTime :: StateTransitions -> Maybe UTCTime
getBacklogTime (STBacklog t)              = Just t
getBacklogTime (STSelected t _)           = Just t
getBacklogTime (STInProgress t _ _)       = Just t
getBacklogTime (STInReview t _ _ _)       = Just t
getBacklogTime (STDone t _ _ _ _)         = Just t
getBacklogTime STIllegalStateTransitions  = Nothing

getSelectedTime :: StateTransitions -> Maybe UTCTime
getSelectedTime (STBacklog _)             = Nothing
getSelectedTime (STSelected _ t)          = Just t
getSelectedTime (STInProgress _ t _)      = Just t
getSelectedTime (STInReview _ t _ _)      = Just t
getSelectedTime (STDone _ t _ _ _)        = Just t
getSelectedTime STIllegalStateTransitions = Nothing

getInProgressTime :: StateTransitions -> Maybe UTCTime
getInProgressTime (STBacklog _)             = Nothing
getInProgressTime (STSelected _ _)          = Nothing
getInProgressTime (STInProgress _ _ t)      = Just t
getInProgressTime (STInReview _ _ t _)      = Just t
getInProgressTime (STDone _ _ t _ _)        = Just t
getInProgressTime STIllegalStateTransitions = Nothing

getReviewTime :: StateTransitions -> Maybe UTCTime
getReviewTime (STBacklog _)             = Nothing
getReviewTime (STSelected _ _)          = Nothing
getReviewTime (STInProgress _ _ _)      = Nothing
getReviewTime (STInReview _ _ _ t)      = Just t
getReviewTime (STDone _ _ _ t _)        = Just t
getReviewTime STIllegalStateTransitions = Nothing

getDoneTime :: StateTransitions -> Maybe UTCTime
getDoneTime (STBacklog _)             = Nothing
getDoneTime (STSelected _ _)          = Nothing
getDoneTime (STInProgress _ _ _)      = Nothing
getDoneTime (STInReview _ _ _ _)      = Nothing
getDoneTime (STDone _ _ _ _ t)        = Just t
getDoneTime STIllegalStateTransitions = Nothing





-- CSV
defaultTaskHeader :: Header
defaultTaskHeader = header
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

instance DefaultOrdered YtTask where
    headerOrder _ = defaultTaskHeader



defaultIssueHeader :: Header
defaultIssueHeader = header
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


instance DefaultOrdered YtIssue where
    headerOrder _ = defaultIssueHeader

