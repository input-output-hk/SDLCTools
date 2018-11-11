{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}


{-# LANGUAGE TemplateHaskell #-}

module Meas.Dev.Types
(
  PriorityValue (..)
  , StateValue (..)
  , WaitValue (..)
  , TypeValue (..)
  , ThreeDValue (..)
  , ROMMandaysValue (..)
  , ResolutionValue (..)
  , LinkType (..)
  , StateTransitions (..)
  , YtIssue (..)
  , YtTask (..)
  , ValueChange (..)

  , Hist (..)
  , Issue (..)
  , GenericIssues (..)
  , GenericIssue (..)
  , GenericIssueField (..)

  , defaultIssue
  , defaultTask


  , getBacklogTime
  , getSelectedTime
  , getInProgressTime
  , getReviewTime
  , getDoneTime
)
where

-- import            Debug.Trace (trace)

import qualified  Data.Text as T
import            Data.Time.Clock

import            Meas.Misc

data PriorityValue =
  ShowStopper
  |Critical
  |Major
  |Normal
  |Minor
  deriving (Eq, Show)


data StateValue =
  Backlog
  |Planning
  |Selected
  |InProgress
  |Review
  |Done
  |Neutral
  deriving (Eq, Show)

data WaitValue =
  Running
  |Waiting
  deriving (Eq, Show)

data TypeValue =
  TaskType
  |IssueType
  |OtherType
  deriving (Eq, Show)

data ThreeDValue =
  Design
  |Development
  |Documentation
  |Test
  deriving (Eq, Show)

data ROMMandaysValue =
  Days
  |Weeks
  |Months
  |Quarters
  deriving (Eq, Show)

data ResolutionValue =
  Successful
  |Aborted
  |Duplicate
  |Obsolete
  deriving (Eq, Show)

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
  deriving (Eq, Show)

data ValueChange =
  UpdateTime Int
  | Updater T.Text
  | StateChange StateValue StateValue
  | WaitChange WaitValue WaitValue
  deriving (Eq, Show)


{-
All possible histories of state transition for an issue.
-}
data StateTransitions =
  STBacklog UTCTime
  |STSelected UTCTime UTCTime
  |STInProgress UTCTime UTCTime UTCTime
  |STInReview UTCTime UTCTime UTCTime UTCTime
  |STDone UTCTime UTCTime UTCTime UTCTime UTCTime
  |STIllegalStateTransitions
  deriving (Eq, Show)

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
  deriving (Show)


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
  deriving (Show)


defaultTask :: YtTask
defaultTask = MkYtTask T.empty T.empty T.empty defUTCTime Nothing T.empty 0 Backlog Running Development [] [] [] STIllegalStateTransitions 0 T.empty []


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

-- generic types used to parse json returned by YouTrack



data GenericIssues = GenericIssues [GenericIssue]
  deriving (Show)


data GenericIssue = MkGenericIssue
  { issueId :: T.Text
  , issueFields :: [GenericIssueField]
  }
  deriving (Show)


data GenericIssueField =
  GProjectField T.Text
  |GTypeField T.Text
  |GCreatedField T.Text
  |GUpdatedField T.Text
  |GNumberField T.Text
  |GSummaryField T.Text
  |GDescriptionField T.Text
  |GStateField T.Text
  |GWaitField T.Text
  |GThreeDField T.Text
  |GDueDateField T.Text
  |GRomManDaysField T.Text
  |GSquadField T.Text
  |GOwnerField T.Text
  |GPEasyField T.Text
  |GPBenefitsField T.Text
  |GPUrgencyField T.Text
  |GLinkField [(LinkType, T.Text)]
  |GAssigneeField T.Text
  |GPotentialSquadField T.Text
  |GPriorityField T.Text
  |GTargetVersionsField T.Text
  |GResolutionField T.Text

  deriving (Show)



data Hist =  Hist
  { issue :: Issue
  , changes :: [[ValueChange]]
  }
  deriving (Show)

data Issue = Issue T.Text
  deriving (Show)


