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
  let csvIssuesLBS = CSV.encodeByName defaultActionableIssueHeader $ L.map ActionableIssueReport issues
  LBS.writeFile filename LBS.empty
  LBS.appendFile filename csvIssuesLBS



defaultActionableIssueHeader :: Header
defaultActionableIssueHeader = header
  [ "IssueId"
  , "Backlog", "InProgress", "Review", "Done"
  , "Repo"
  , "Is Epic"
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
        , "Backlog"         .= ("20180813" :: String) -- maybe T.empty intToDateText (getBacklogTime transitions)
        , "InProgress"      .= ("20180814" :: String) -- maybe T.empty intToDateText (getInProgressTime transitions)
        , "Review"          .= ("20180815" :: String) -- maybe T.empty intToDateText (getReviewTime transitions)
        , "Done"            .= ("20180816" :: String) -- maybe T.empty intToDateText (getDoneTime transitions)
        , "Repo"            .= repo
        , "Is Epic"         .= show zhiIsEpic
        , "Assignee-1"      .= a0
        , "Assignee-2"      .= a1
        , "Assignee-3"      .= a2
        ]
        where
        (a0:a1:a2:a3:a4:_) = (L.map ghuUser ghiAssignees ++ L.repeat "")


instance CSV.DefaultOrdered ActionableIssueReport where
    headerOrder _ = defaultActionableIssueHeader

