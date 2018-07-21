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

data PullRequest = PullRequest {
                   prTitle       :: Title
                 } deriving ( Show, Eq, Generic)

type Title   = Text
type Id      = Text
type Date    = Text
type Message = Text

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

