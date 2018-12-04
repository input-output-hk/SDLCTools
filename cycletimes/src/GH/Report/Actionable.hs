{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}


{-# LANGUAGE TemplateHaskell #-}

module GH.Report.Actionable
(
  generateActionableForIssues
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

import            GH.Types

intToDateText :: UTCTime -> T.Text
intToDateText t = T.pack $ formatTime defaultTimeLocale  "%Y%m%d" t


generateActionableForIssues :: String -> [Issue] -> IO ()
generateActionableForIssues filename issues = do
  let csvIssuesLBS = CSV.encodeByName defaultActionableIssueHeader $ L.map ActionableIssueReport $ L.filter hasValidStateTransitions issues
  LBS.writeFile filename LBS.empty
  LBS.appendFile filename csvIssuesLBS
  where
  hasValidStateTransitions MkIssue{..} = iStateTransitions /= STIllegalStateTransitions


defaultActionableIssueHeader :: Header
defaultActionableIssueHeader = header
  [ "IssueId"
  , "Backlog", "InProgress", "Review", "Done"
  , "Repo"
  , "Is Epic"
  , "Parent Epic"
  , "Milestone"
  , "Assignee-1"
  , "Assignee-2"
  , "Assignee-3"

  ]

data ActionableIssueReport = ActionableIssueReport Issue

{-}

data Issue = MkIssue
  { iGHIssue            :: GHIssue
  , iZHIssue            :: ZHIssue
  , iGHIssueEvents      :: [GHIssueEvent]
  , iZHIssueEvents      :: [ZHIssueEvent]
  , iRepoName           :: T.Text
  , iStateTransitions   :: StateTransitions
  }
  deriving (Show, Eq, Ord)
-}

instance ToNamedRecord ActionableIssueReport where
    toNamedRecord (ActionableIssueReport (MkIssue MkGHIssue{..} MkZHIssue{..} _ _ repo transitions)) = namedRecord
        [ "IssueId"         .= (T.unpack repo ++ "-" ++ show ghiNumber)
        , "Backlog"         .= maybe T.empty intToDateText (getBacklogTime transitions)
        , "InProgress"      .= maybe T.empty intToDateText (getInProgressTime transitions)
        , "Review"          .= maybe T.empty intToDateText (getReviewTime transitions)
        , "Done"            .= maybe T.empty intToDateText (getDoneTime transitions)
        , "Repo"            .= repo
        , "Is Epic"         .= show zhiIsEpic
        , "Parent Epic"     .= maybe "" (\n -> T.unpack repo ++ "-" ++ show n) zhiParentEpic
        , "Milestone"       .= maybe "" (\MkGHMilestone{..} -> T.unpack repo ++ "-" ++ show ghmNumber ++ " (" ++ utcTimeToDateString ghmDueDate ++ ")") ghiMilestone
        , "Assignee-1"      .= a0
        , "Assignee-2"      .= a1
        , "Assignee-3"      .= a2
        ]
        where
        (a0:a1:a2:a3:a4:_) = (L.map ghuUser ghiAssignees ++ L.repeat "")
        utcTimeToDateString t = formatTime defaultTimeLocale  "%d-%m-%Y" t



instance CSV.DefaultOrdered ActionableIssueReport where
    headerOrder _ = defaultActionableIssueHeader

