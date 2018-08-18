{-# Language  BangPatterns     #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Extract where

import Types

-- | given a pull request getPrData returns a triple containing the
-- development start date, review start time and pull request closing
-- time (if closed).
getPrData :: PullRequest -> (Date, Date, Maybe Date)
getPrData PullRequest{..} =
  let !devStartedAt    = cCommittedDate (head prCommits)  -- JCM how do you make sure commits are correctly ordered?
      !reviewStartedAt = prCreatedAt
      !prClosingDate   = prClosedAt   -- JCM what if prClosedAt = Nothing and prMergedAt is Just date?
  in (devStartedAt, reviewStartedAt, prClosedAt)
