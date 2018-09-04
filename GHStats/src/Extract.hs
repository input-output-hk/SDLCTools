{-# Language BangPatterns         #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Extract where

import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.ByteString.Lazy            as LBS
import qualified Data.ByteString.Lazy.Char8      as LBSC
import           Types
import           Regex

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

-- | given the latest commit time and a PR containing the first commit
-- date, mkPRAnalysis returns a PRAnalysis.
mkPRAnalysis :: PullRequest -> Maybe PRAnalysis
mkPRAnalysis pr@PullRequest{..} =do
  let !prNum       = prNumber
  firstCommitTime  <- getFirstCommitTime pr
  latestCommitTime <- getLastCommitTime pr
  let (prClosingDate, wasMerged)    =
        if (prMergedAt == Nothing)
          then (prClosedAt, False)
        else (prMergedAt, True)
  let devReviewCommits = splitCommits pr
  return $ PRAnalysis prNum firstCommitTime prCreatedAt latestCommitTime prClosingDate wasMerged (auName prAuthor) devReviewCommits prComments


extractIssueId :: Commit -> Either T.Text YtIssueId
extractIssueId Commit{..} = do
  regex <- compileRegex issueIdRegExp
  matchWithCapture <- maybeToEither "IssueId not found!" $ matchOnce regex string
  pure . T.pack . LBSC.unpack $ getOneMatch string matchWithCapture
  where
    string = LBSC.pack . T.unpack $ cMessage

issueIdRegExp :: LBS.ByteString
issueIdRegExp = "\\[ *[A-Z]+\\-[0-9]+ *\\]"









