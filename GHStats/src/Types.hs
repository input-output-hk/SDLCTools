{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE RecordWildCards    #-}
module Types where

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Control.Monad
import           GHC.Generics
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text
import qualified Data.HashMap.Strict as HM

data Commit = Commit {
              cId            :: Id
            , cCommittedDate :: Date
            , cMessage       :: Message
            } deriving ( Show, Eq, Generic)

data Comment = Comment {
               coId        :: Id
             , coBodyText  :: BodyText
             , coCreatedAt :: Date
             , coAuthor    :: Author
             } deriving ( Show, Eq, Generic)

data Author = Author {
              auName :: Name
            } deriving ( Show, Eq, Generic)

data PullRequest = PullRequest {
                   prTitle       :: Title
                 } deriving ( Show, Eq, Generic)

type Title    = Text
type Id       = Text
type Date     = Text
type Name     = Text
type Message  = Text
type BodyText = Text

newtype CommitList = CommitList [Commit]
  deriving ( Show, Eq, Generic)

instance FromJSON CommitList where
  parseJSON v = CommitList <$> parseAllCommit v

parseAllCommit :: Value -> Parser [Commit]
parseAllCommit = withObject "Commits" $ \o -> do
  data_        <- o            .: "data"
  organisation <- data_        .: "organization"
  repository   <- organisation .: "repository"
  pullRequest  <- repository   .: "pullRequest"
  commits      <- pullRequest  .: "commits"
  commitNodes  <- commits      .: "nodes"
  forM commitNodes parseCommit

parseCommit :: Value -> Parser Commit
parseCommit = withObject "Commit" $ \commitNode -> do
  commit         <- commitNode .: "commit"
  cId            <- commit     .: "id"
  cCommittedDate <- commit     .: "committedDate"
  cMessage       <- commit     .: "message"
  return Commit{..}

instance FromJSON Author where
  parseJSON = withObject "Author" $ \o -> do
    auName <- o .: "name"
    return Author{..}

newtype CommentList = CommentList [Comment]
  deriving ( Show, Eq, Generic)

instance FromJSON CommentList where
  parseJSON v = CommentList <$> parseAllComment v

parseAllComment :: Value -> Parser [Comment]
parseAllComment = withObject "Comments" $ \o -> do
  data_        <- o            .: "data"
  organisation <- data_        .: "organization"
  repository   <- organisation .: "repository"
  pullRequest  <- repository   .: "pullRequest"
  comments     <- pullRequest  .: "comments"
  commentNodes <- comments     .: "nodes"
  forM commentNodes parseComment

parseComment :: Value -> Parser Comment
parseComment = withObject "Comment" $ \comment -> do
  coId        <- comment .: "id"
  coBodyText  <- comment .: "bodyText"
  coCreatedAt <- comment .: "createdAt"
  coAuthor    <- comment .: "author"
  return Comment{..}

instance FromJSON PullRequest where
  parseJSON = parseResponse

parseResponse :: Value -> Parser PullRequest
parseResponse = withObject "PullRequest" $ \o -> do
  data_        <- o            .: "data"
  organisation <- data_        .: "organization"
  repository   <- organisation .: "repository"
  pullRequest  <- repository   .: "pullRequest"
  prTitle      <- pullRequest  .: "title"
  return $ PullRequest{..}



