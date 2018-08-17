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
  let !devStartedAt    = cCommittedDate (head prCommits)
      !reviewStartedAt = prCreatedAt
      !prClosingDate   = prClosedAt
  in (devStartedAt, reviewStartedAt, prClosedAt)
