{-# Language  BangPatterns     #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Extract where

import qualified Data.Text as T

import Types


-- | given a pull request getPrData returns a triple containing the
-- development start date, review start time and pull request closing
-- time (if closed).
getPrData :: PullRequest -> PRCSVData
getPrData PullRequest{..} =
  let !prNum           = T.pack . show $ prNumber
      !devStartedAt    = formatTime $ cCommittedDate (head prCommits)
      !reviewStartedAt = formatTime prCreatedAt
      !prClosingDate   = formatTime . maybe "" id $
        if (prMergedAt == Nothing)
          then prClosedAt
        else prMergedAt
  in PRCSVData ( prNum, devStartedAt, reviewStartedAt, prClosingDate)


-- | given a dateTime in ISO 8601 format returns a Date in yyyymmdd format
formatTime :: Date -> T.Text
formatTime = T.concat . T.splitOn "-" . T.takeWhile (/= 'T')

-- | given a PullRequest returns the commitTime of head of the commitList
getCommitTime :: PullRequest -> T.Text
getCommitTime PullRequest{..} = formatTime . cCommittedDate . head $ prCommits

-- | given the latest commit time and a PR containing the first commit
-- date, mkPRAnalysis returns a PRAnalysis
mkPRAnalysis :: T.Text -> PullRequest -> PRAnalysis
mkPRAnalysis latestCommitTime PullRequest{..} =
  let !prNum           = prNumber
      !firstCommitTime = formatTime $ cCommittedDate (head prCommits)
      !reviewStartedAt = formatTime prCreatedAt
      !prClosingDate   = formatTime . maybe "" id $
        if (prMergedAt == Nothing)
          then prClosedAt
        else prMergedAt
  in PRAnalysis prNum firstCommitTime reviewStartedAt latestCommitTime prClosingDate
