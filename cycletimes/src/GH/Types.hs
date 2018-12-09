{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE RecordWildCards   #-}

module GH.Types where

import            GHC.Generics

import qualified  Data.Csv as CSV
import qualified  Data.Text as T
import            Data.Time.Clock
import            Data.Time.Clock.POSIX


toUTCTime :: Int -> UTCTime
toUTCTime n = posixSecondsToUTCTime (fromIntegral $ n `div` 1000)


data Issue = MkIssue
  { iGHIssue            :: GHIssue
  , iZHIssue            :: ZHIssue
  , iGHIssueEvents      :: [GHIssueEvent]
  , iZHIssueEvents      :: [ZHIssueEvent]
  , iRepoName           :: T.Text
  , iStateTransitions   :: StateTransitions
  }
  deriving (Show, Eq, Ord)



data GHUser = MkGHUser
  { ghuUser     :: T.Text
  , ghuUserId   :: Int
  }
  deriving (Show, Eq, Ord)

data GHMilestone = MkGHMilestone
  {
    ghmNumber   :: Int
  , ghmDueDate  :: UTCTime
  }
  deriving (Show, Eq, Ord)

defGHUser :: GHUser
defGHUser = MkGHUser "" 0

data GHIssue = MkGHIssue
  { ghiId               :: Int
  , ghiNumber           :: Int
  , ghiTitle            :: T.Text
  , ghiUser             :: GHUser
  , ghiCreationTime     :: UTCTime
  , ghiMainAssignee     :: Maybe GHUser
  , ghiAssignees        :: [GHUser]
  , ghiUrl              :: T.Text
  , ghiIsPR             :: Bool
  , ghiMilestone        :: Maybe GHMilestone
  }
  deriving (Show, Eq, Ord)

data ZHIssue = MkZHIssue
  { zhiState            :: State
  , zhiIsEpic           :: Bool
  , zhiParentEpic       :: Maybe Int
  , zhiChildren         :: [Int]
  }
  deriving (Show, Eq, Ord)


newtype AllEpics = AllEpics { epics :: [Int] }
  deriving (Show, Eq, Ord)

newtype EpicChildren = EpicChildren { ecIssues :: [Int] }
  deriving (Show, Eq, Ord)

data GHIssueEvent =
  GHEvtCloseEvent TimeStamp
  |GHEvtReOpenEvent TimeStamp
  deriving (Show, Eq)

instance Ord GHIssueEvent where
  compare ge1 ge2 = compare (getTime ge1) (getTime ge2)
    where
      getTime (GHEvtCloseEvent t) = t
      getTime (GHEvtReOpenEvent t) = t



data ZHIssueEvent =
  ZHEvtTransferState State State TimeStamp
  deriving (Show, Eq)

instance Ord ZHIssueEvent where
  compare ze1 ze2 = compare (getTime ze1) (getTime ze2)
    where
      getTime (ZHEvtTransferState _ _ t) = t

data StateEvent = StateEvent State State TimeStamp
  deriving (Eq, Show, Generic)

instance Ord StateEvent where
  compare se1 se2 = compare (getTime se1) (getTime se2)
    where
      getTime (StateEvent _ _ t) = t


data State =
  Backlog
  |InProgress
  |InReview
  |Done
  |Neutral
  deriving (Eq, Show, Generic, Ord)

type UserId = Int
type TimeStamp = UTCTime


nameToState :: T.Text -> State
nameToState "New Issues"      = Backlog
nameToState "Backlog"         = Backlog
nameToState "In Progress"     = InProgress
nameToState "Review/QA"       = InReview
nameToState "Review"          = InReview
nameToState "Done"            = Done
nameToState "Closed"          = Done
nameToState "Retrospective"   = Done
nameToState "Epics"           = Backlog
nameToState "Proposed"        = Backlog
nameToState "Accepted"        = Backlog
nameToState "Waiting"         = Neutral
nameToState e = error (T.unpack e)



data StateTransitions =
  STBacklog UTCTime
  |STInProgress UTCTime UTCTime
  |STInReview UTCTime UTCTime UTCTime
  |STDone UTCTime UTCTime UTCTime UTCTime
  |STIllegalStateTransitions
  deriving (Eq, Show, Ord)




getBacklogTime :: StateTransitions -> Maybe UTCTime
getBacklogTime (STBacklog t)              = Just t
getBacklogTime (STInProgress t _)         = Just t
getBacklogTime (STInReview t _ _)         = Just t
getBacklogTime (STDone t _ _ _)           = Just t
getBacklogTime STIllegalStateTransitions  = Nothing


getInProgressTime :: StateTransitions -> Maybe UTCTime
getInProgressTime (STBacklog _)             = Nothing
getInProgressTime (STInProgress _ t)        = Just t
getInProgressTime (STInReview _ t _)        = Just t
getInProgressTime (STDone _ t _ _)          = Just t
getInProgressTime STIllegalStateTransitions = Nothing

getReviewTime :: StateTransitions -> Maybe UTCTime
getReviewTime (STBacklog _)             = Nothing
getReviewTime (STInProgress _ _)        = Nothing
getReviewTime (STInReview _ _ t)        = Just t
getReviewTime (STDone _ _ t _)          = Just t
getReviewTime STIllegalStateTransitions = Nothing

getDoneTime :: StateTransitions -> Maybe UTCTime
getDoneTime (STBacklog _)             = Nothing
getDoneTime (STInProgress _ _)        = Nothing
getDoneTime (STInReview _ _ _)        = Nothing
getDoneTime (STDone _ _ _ t)          = Just t
getDoneTime STIllegalStateTransitions = Nothing


data Developer = MkDeveloper {
    ghUserName  :: !T.Text
  , devRealName :: !T.Text
  }
  deriving (Show, Eq)

instance CSV.FromNamedRecord Developer where
  parseNamedRecord dev = MkDeveloper <$> dev CSV..: "ghUserName" <*> dev CSV..: "devRealName"

