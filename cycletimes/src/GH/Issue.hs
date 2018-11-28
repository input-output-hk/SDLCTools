{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE BangPatterns         #-}

module GH.Issue where


import Debug.Trace (trace)

import            Control.Monad
import            Control.Applicative
import            GHC.Generics
import            Data.Aeson
import            Data.Aeson.Types
import qualified  Data.List as L
import            Data.Maybe (catMaybes)
import qualified  Data.Text as T
import            Data.Vector      (toList)

import            Data.Time.Calendar
import            Data.Time.Clock
import            Data.Time.Clock.POSIX
import            Data.Time.Format

import            GH.Config
import            GH.Parser
import            GH.Queries
import            GH.Types


getIssues :: Config -> IO [Issue]
getIssues MkConfig{..} = do
  issues <- mapM (\(u, r, rid) -> getGHIssuesForRepo u r rid) cfgRepos >>= (return . L.concat)
  return $ L.map (\(r, ghi, ghEvts, zhi, zhEvts) -> MkIssue ghi zhi ghEvts zhEvts (T.pack r) STIllegalStateTransitions) issues

  where
  getGHIssuesForRepo user repo repoId = do
    jsons <- getAllIssuesFromGHRepo cfg_gh_key user repo
    let ghIssues = L.concat $ L.map (\json ->
                    case eitherDecode json of
                      Right issues -> issues
                      Left e -> error e) jsons
    mapM proc ghIssues
    where
    proc ghIssue = do
      print ("getting data for: ", ghiNumber ghIssue)
      zhIssue <- getZHIssueForRepo repo repoId ghIssue
      zhIssueEvents <- getZHIssueEventsForRepo repo repoId ghIssue
      ghIssueEvents <- getGHIssueEventsForRepo user repo ghIssue

      return (repo, ghIssue, ghIssueEvents, zhIssue, zhIssueEvents)

  getGHIssueEventsForRepo user repo ghIssue@MkGHIssue{..} = do
    json <- getIssueEventsFromGHRepo cfg_gh_key user repo ghiNumber
    let (ghEvtsE :: Either String [Maybe GHIssueEvent]) = eitherDecode json
    case ghEvtsE of
      Right ghEvts -> return $ catMaybes ghEvts
      Left e -> fail e

  getZHIssueForRepo repo repoId ghIssue@MkGHIssue{..} = do
    json <- getSingleIssueFromZHRepo cfg_zh_key repoId ghiNumber
--    print "====================="
--    print json

    let (zhIssueE :: Either String ZHIssue) = eitherDecode json
    case zhIssueE of
      Right zhIssue -> return zhIssue
      Left e -> fail e

  getZHIssueEventsForRepo repo repoId ghIssue@MkGHIssue{..} = do
    json <- getSingleIssueEventsFromZHRepo cfg_zh_key repoId ghiNumber
    let (zhEvtsE :: Either String [Maybe ZHIssueEvent]) = eitherDecode json
    case zhEvtsE of
      Right zhEvts -> return $ catMaybes zhEvts
      Left e -> fail e

{-}

data GHUser = MkGHUser
  { ghuUser     :: T.Text
  , ghuUserId   :: Int
  }
  deriving (Show, Eq, Ord)

defGHUser :: GHUser
defGHUser = MkGHUser "" 0

data GHIssue = MkGHIssue
  { ghiId               :: Int
  , ghiNumber           :: Int
  , ghiTitle            :: T.Text
  , ghiUser             :: GHUser
  , ghiCreationTime     :: UTCTime
  , ghiMainAssignee     :: Maybe GHUser
  , ghiAssignees        :: [GHUser]
  , ghiUrl              :: T.Text
  , ghiRepoName         :: T.Text
  }
  deriving (Show, Eq, Ord)

data GHIssueEvent =
  GHEvtCloseEvent UTCTime
  |GHEvtReOpenEvent UTCTime
  |GHEvtOther
  deriving Show



data Event =
  TransferState State State TimeStamp UserId
  | SetState State TimeStamp UserId
  | NotImportant
  deriving (Eq, Show, Generic)

instance FromJSON Event where
  parseJSON = withObject "Event" $ \o -> do
    eventType <- o .: "type" :: Parser T.Text
    case eventType of
      "transferIssue" -> do
        timeStamp <- toUTCTime <$> (o .: "created_at" :: Parser Int)

        let transferState  = do
              fromPipeLine <-  o .: "from_pipeline"
              fromState <- nameToState <$> (fromPipeLine .: "name" :: Parser T.Text)
              toPipeLine <-  o .: "to_pipeline"
              toState <- nameToState <$> (toPipeLine .: "name" :: Parser T.Text)
              pure $ TransferState fromState toState (toUTCTime 1) 1

        let setState  = do
              toPipeLine <-  o .: "to_pipeline"
              toState <- nameToState <$> (toPipeLine .: "name" :: Parser T.Text)
              pure $ SetState toState (toUTCTime 1) 1

        transferState <|> setState

      _ -> pure NotImportant

data State =
  Backlog
  |InProgress
  |InReview
  |Done
  deriving (Eq, Show, Generic)

type UserId = Int
type TimeStamp = UTCTime

parseEvents :: Value -> Parser [Event]
parseEvents (Array arr) = do
  forM (toList arr) parseJSON :: Parser [Event]

nameToState :: T.Text -> State
nameToState "New Issues"  = Backlog
nameToState "In Progress" = InProgress
nameToState "Review/QA"   = InReview
nameToState "Closed"      = Done


-}

