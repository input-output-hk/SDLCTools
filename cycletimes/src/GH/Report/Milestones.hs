{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}


{-# LANGUAGE TemplateHaskell #-}

module GH.Report.Milestones
(
  generateMilestoneReport
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

import            GHC.Exts

import            GH.Types

data MilestoneReport = MilestoneReport (Maybe Issue) (Maybe Issue)

getIssueMap :: [Issue] -> M.Map Int Issue
getIssueMap issues =
  M.fromList $ L.map proc issues
  where
  proc issue@(MkIssue MkGHIssue{..} _ _ _ _ _) = (ghiNumber, issue)

generateMilestoneReport :: String -> [Issue] -> IO ()
generateMilestoneReport filename issues = do
  let csvIssueReportLBS = CSV.encodeByName defaultMilestoneReportHeader rows
  LBS.writeFile  filename LBS.empty
  LBS.appendFile filename csvIssueReportLBS
  where
  rows = sortWith sortFun $ L.map proc issues
  proc issue@(MkIssue MkGHIssue{..} MkZHIssue{..} _ _ _ _) =
    if zhiIsEpic
    then MilestoneReport (Just issue) Nothing
    else  let epic = do
                e <- zhiParentEpic
                M.lookup e issueMap
          in MilestoneReport epic (Just issue)
  issueMap = getIssueMap issues
  sortFun (MilestoneReport Nothing (Just i)) = let
    (MkIssue MkGHIssue{..} MkZHIssue{..} _ _ _ _) = i
    in Just (zhiState, zhiIsEpic, ghiNumber)
  sortFun (MilestoneReport (Just e) _) = let
    (MkIssue MkGHIssue{..} MkZHIssue{..} _ _ _ _) = e
    in Just (zhiState, zhiIsEpic, ghiNumber)


defaultMilestoneReportHeader :: Header
defaultMilestoneReportHeader = header
  [ "Repo"
  , "Epic"
  , "Issue"
  , "State"
  , "Issue Milestone"
  , "Epic Milestone"
  ]


instance ToNamedRecord MilestoneReport where
    toNamedRecord (MilestoneReport Nothing (Just issue)) = namedRecord
        [ "Repo"            .= repo
        , "Epic"            .= (""::String)
        , "Issue"           .= ghiUrl
        , "State"           .= show zhiState
        , "Issue Milestone" .= (""::String)
        , "Epic Milestone"  .= maybe "" (show . ghmNumber) ghiMilestone
        ]
        where
        (MkIssue MkGHIssue{..} MkZHIssue{..} _ _ repo _) = issue

    toNamedRecord (MilestoneReport (Just epic) Nothing) = namedRecord
        [ "Repo"            .= repo
        , "Epic"            .= ghiUrl
        , "Issue"           .= (""::String)
        , "State"           .= show zhiState
        , "Issue Milestone" .= maybe "" (show . ghmNumber) ghiMilestone
        , "Epic Milestone"  .= (""::String)
        ]
        where
        (MkIssue MkGHIssue{..} MkZHIssue{..} _ _ repo _) = epic

    toNamedRecord (MilestoneReport (Just epic) (Just issue)) = namedRecord
        [ "Repo"            .= repo
        , "Epic"            .= epicUrl
        , "Issue"           .= issueUrl
        , "State"           .= show epicState
        , "Issue Milestone" .= issueMilestone
        , "Epic Milestone"  .= epicMilestone
        ]
        where
        repo = iRepoName epic
        epicState = (zhiState . iZHIssue) issue
        epicUrl = (ghiUrl . iGHIssue) epic
        issueUrl = (ghiUrl . iGHIssue) issue
        epicMilestone = maybe "" (show . ghmNumber) $ (ghiMilestone . iGHIssue) epic
        issueMilestone = maybe "" (show . ghmNumber) $ (ghiMilestone . iGHIssue) issue


instance CSV.DefaultOrdered MilestoneReport where
    headerOrder _ = defaultMilestoneReportHeader


