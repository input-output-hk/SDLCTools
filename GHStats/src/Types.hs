{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE RecordWildCards    #-}
module Types where

import qualified Data.ByteString.Char8 as B8
import           Control.Monad
import           GHC.Generics
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text


newtype PullRequestList = PullRequestList [PullRequest]
  deriving ( Show )

instance FromJSON PullRequestList where
  parseJSON = parseResponse

data PullRequest = PullRequest {
                   prId        :: Id
                 , prNumber    :: Int
                 , prTitle     :: Title
                 , prCreatedAt :: Date
                 , prState     :: PRState
                 , prClosed    :: Bool
                 , prClosedAt  :: Maybe Date
                 , prMerged    :: Bool
                 , prMergedAt  :: Maybe Date
                 , prCommits   :: [Commit]
                 , prComments  :: [Comment]
                 } deriving ( Show, Eq, Generic)

instance FromJSON PullRequest where
  parseJSON = parsePullRequest

data Commit = Commit {
              cId            :: Id
            , cCommittedDate :: Date
            , cMessage       :: Message
            } deriving ( Show, Eq, Generic)

instance FromJSON Commit where
  parseJSON = parseCommit

data Comment = Comment {
               coId        :: Id
             , coBodyText  :: BodyText
             , coCreatedAt :: Date
             , coAuthor    :: Author
             } deriving ( Show, Eq, Generic)

instance FromJSON Comment where
  parseJSON = parseComment

data Author = Author {
              auName :: Maybe Name
            } deriving ( Show, Eq, Generic)

instance FromJSON Author where
  parseJSON = withObject "Author" $ \o -> do
    auName <- o .: "name"
    return Author{..}

data PRState = OPEN | CLOSED | MERGED
  deriving ( Show, Eq, Generic, FromJSON)

type Title           = Text
type Id              = Text
type Date            = Text
type Name            = Text
type Message         = Text
type BodyText        = Text
type YoutrackIssueId = Text

newtype PRCSVData = PRCSVData ( Date, Date, Date)


parseResponse :: Value -> Parser PullRequestList
parseResponse = withObject "All PullRequests" $ \o -> do
  data_           <- o               .: "data"
  organisation    <- data_           .: "organization"
  repository      <- organisation    .: "repository"
  allPullRequests <- repository      .: "pullRequests"
  prNodes         <- allPullRequests .: "nodes"
  pullRequests    <- forM prNodes parsePullRequest
  return $ PullRequestList pullRequests


parsePullRequest :: Value -> Parser PullRequest
parsePullRequest = withObject "PullRequest" $ \pullRequest -> do
  prId         <- pullRequest  .: "id"
  prNumber     <- pullRequest  .: "number"
  prTitle      <- pullRequest  .: "title"
  prCreatedAt  <- pullRequest  .: "createdAt"
  prState      <- pullRequest  .: "state"
  prClosed     <- pullRequest  .: "closed"
  prClosedAt   <- pullRequest  .: "closedAt"
  prMerged     <- pullRequest  .: "merged"
  prMergedAt   <- pullRequest  .: "mergedAt"
  allCommits   <- pullRequest  .: "commits"
  commitNodes  <- allCommits   .: "nodes"
  prCommits    <- forM commitNodes parseCommit
  allComments  <- pullRequest  .: "comments"
  commentNodes <- allComments  .: "nodes"
  prComments   <- forM commentNodes parseComment
  return PullRequest{..}

parseCommit :: Value -> Parser Commit
parseCommit = withObject "Commit" $ \commitNode -> do
  commit         <- commitNode .: "commit"
  cId            <- commit     .: "id"
  cCommittedDate <- commit     .: "committedDate"
  cMessage       <- commit     .: "message"
  return Commit{..}

parseComment :: Value -> Parser Comment
parseComment = withObject "Comment" $ \comment -> do
  coId        <- comment .: "id"
  coBodyText  <- comment .: "bodyText"
  coCreatedAt <- comment .: "createdAt"
  coAuthor    <- comment .: "author"
  return Comment{..}

