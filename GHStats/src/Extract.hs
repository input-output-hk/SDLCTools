{-# Language BangPatterns         #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}



module Extract where

import qualified Data.List as L
import qualified Data.Text as T

import Types


-- | given a PullRequest returns the authored date of the earliest commit.
getFirstCommitTime :: PullRequest -> Maybe Date
getFirstCommitTime PullRequest{..} =
  case prCommits of
  [] -> Nothing
  (h:_) -> Just $ cAuthoredDate h

-- | given a PullRequest returns the authored date of the latest commit.
getLastCommitTime :: PullRequest -> Maybe Date
getLastCommitTime PullRequest{..} =
  case reverse prCommits of
  [] -> Nothing
  (h:_) -> Just $ cAuthoredDate h



-- | Splits commits between those created before the PR creation and those created after the PR creation
splitCommits :: PullRequest -> ([Commit], [Commit])
splitCommits PullRequest{..} = L.partition (\c -> cAuthoredDate c <= prCreatedAt) prCommits

-- partition :: (a -> Bool) -> [a] -> ([a], [a])





-- | given the latest commit time and a PR containing the first commit
-- date, mkPRAnalysis returns a PRAnalysis.
mkPRAnalysis :: PullRequest -> Maybe PRAnalysis
mkPRAnalysis pr@PullRequest{..} =do
  let !prNum       = prNumber
  firstCommitTime  <- getFirstCommitTime pr
  latestCommitTime <- getLastCommitTime pr
  let !prClosingDate    =
        if (prMergedAt == Nothing)
          then prClosedAt
        else prMergedAt
  let devReviewCommits = splitCommits pr
  return $ PRAnalysis prNum firstCommitTime prCreatedAt latestCommitTime prClosingDate devReviewCommits prComments













