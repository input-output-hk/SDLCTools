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
import            Data.Time.Clock
import            Data.Time.Format

import            GH.Types
import            GH.DevNames

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
  , "Milestone"
  , "Release"
  , "Parent Epic"
  , "Assignee-1"
  , "Assignee-2"
  , "Assignee-3"

  ]

data ActionableIssueReport = ActionableIssueReport Issue



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
        , "Milestone"       .= maybe "" (\MkGHMilestone{..} -> T.unpack repo ++ "-" ++ show ghmNumber ++ " (" ++ maybe "n/a" utcTimeToDateString ghmDueDate ++ "):" ++ T.unpack ghmTitle) ghiMilestone
        , "Release"         .= maybe "" (\MkZHRelease{..} -> T.unpack zhrTitle ++ " (" ++ utcTimeToDateString zhrEndDate ++ "): " ++ T.unpack repo) releaseM
        , "Assignee-1"      .= a0
        , "Assignee-2"      .= a1
        , "Assignee-3"      .= a2
        ]
        where
        (a0:a1:a2:_) = (L.map (toRealName . ghuUser) ghiAssignees ++ L.repeat "")
        utcTimeToDateString t = formatTime defaultTimeLocale  "%d-%m-%Y" t
        releaseM =
          case (zhiRelease, zhiRelease) of
            (Just r, _)  -> Just r
            (_, Just r)  -> Just r
            _ -> Nothing




instance CSV.DefaultOrdered ActionableIssueReport where
    headerOrder _ = defaultActionableIssueHeader

