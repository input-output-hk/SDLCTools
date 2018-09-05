{-# Language BangPatterns         #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Extract where

import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.ByteString.Lazy            as LBS
import qualified Data.ByteString.Lazy.Char8      as LBSC
import           Control.Monad
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

mkPRAnalysis :: PullRequest -> Maybe PRAnalysis
mkPRAnalysis pr@PullRequest{..} = do
  firstCommitTime  <- getFirstCommitTime pr
  latestCommitTime <- getLastCommitTime pr
  let !prYTID      = either (const Nothing) pure $ extractIssueId prTitle
      (prClosingDate, wasMerged) =
        if (prMergedAt == Nothing)
          then (prClosedAt, False)
        else (prMergedAt, True)
      devReviewCommits = splitCommits pr
      ytIssuseIdPresence = chkAllEithers $ extractIssueId <$> ( prTitle : (cMessage <$> prCommits))
  return $ PRAnalysis prNumber prYTID firstCommitTime prCreatedAt latestCommitTime prClosingDate wasMerged (auName prAuthor) devReviewCommits prComments prSourceBranch prTargetBranch ytIssuseIdPresence

mkPRCDetails :: PullRequest -> Maybe [PRCDetails]
mkPRCDetails PullRequest{..} = do
  let !prYTID = either (const Nothing) pure $ extractIssueId prTitle
  forM prCommits $ \ Commit{..} -> do
    let !prcYTID = either (const Nothing) pure $ extractIssueId cMessage
    pure $ PRCDetails prNumber (auName prAuthor) cAuthoredDate cAuthor cAuthorEmail prYTID prcYTID cId

extractIssueId :: T.Text -> Either T.Text YtIssueId
extractIssueId text = do
  regex <- compileRegex issueIdRegExp
  matchWithCapture <- maybeToEither "IssueId not found!" $ matchOnce regex string
  pure .T.dropEnd 1 . T.drop 1 . T.pack . LBSC.unpack $ getOneMatch string matchWithCapture
  where
    string = LBSC.pack . T.unpack $ text

issueIdRegExp :: LBS.ByteString
issueIdRegExp = "\\[ *[A-Z]+\\-[0-9]+ *\\]"


-- | auxilliary function to merge all the eithers and return False if any of
-- them contain Left something.
chkAllEithers :: [Either a b] -> Bool
chkAllEithers []       = True
chkAllEithers (x : xs) = case x of
  Left _ -> False
  _      -> chkAllEithers xs



