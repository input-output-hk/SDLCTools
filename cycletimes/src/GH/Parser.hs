{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module GH.Parser
where

--import Debug.Trace (trace)

import            Control.Monad (forM)

import            Data.Aeson
import            Data.Aeson.Types (Parser)
import qualified  Data.HashMap.Strict as M
import qualified  Data.Text as T
import            Data.Time.Format


import            GH.Types

parseUTCTime :: ParseTime t => String -> t
parseUTCTime t = parseTimeOrError  True defaultTimeLocale  "%Y-%m-%dT%H:%M:%S%QZ" t


instance {-# OVERLAPS #-} FromJSON (Maybe ZHIssueEvent) where
  parseJSON = withObject "ZHEvent" $ \o -> do
    eventType <- o .: "type" :: Parser T.Text
    case eventType of
      "transferIssue" -> do
        timeStampText <- o .: "created_at" :: Parser T.Text
        let timeStamp = parseUTCTime (T.unpack timeStampText)

        let transferState  = do
              fromPipeLine <-  o .: "from_pipeline"
              fromState <- nameToState <$> (fromPipeLine .: "name" :: Parser T.Text)
              toPipeLine <-  o .: "to_pipeline"
              toState <- nameToState <$> (toPipeLine .: "name" :: Parser T.Text)
              return $ Just $ ZHEvtTransferState fromState toState timeStamp

        transferState

      _ -> return Nothing

instance FromJSON ZHIssue where
  parseJSON = withObject "ZH Issue" $ \o -> do
    zhiIsEpic       <- o .: "is_epic"
    pipeline        <- o .: "pipeline"
    zhiState        <- fmap nameToState $ pipeline .: "name"
    estimateM       <- o .:? "estimate" -- :: Parser (Maybe Object)
    zhiEstimate     <- case estimateM of
                        Just estimate -> do
                          e <- (estimate .: "value")
                          return e
                        Nothing -> return Nothing
    let zhiParentEpic = Nothing
    let zhiChildren = []
    let zhiRelease = Nothing
    let zhiInheritedRelease = Nothing
    return $ MkZHIssue {..}

instance {-# OVERLAPS #-} FromJSON (Maybe GHIssueEvent) where
  parseJSON = withObject "event" $ \o -> do
    evt  <- o .: "event" :: Parser T.Text
    timeTxt <- o .: "created_at" :: Parser T.Text
    let time = parseUTCTime (T.unpack timeTxt)

    case evt of
      "closed" -> return $ Just $ GHEvtCloseEvent time
      "reopened" -> return $ Just $ GHEvtReOpenEvent time
      _ -> return Nothing

instance FromJSON GHUser where
  parseJSON = withObject "User" $ \o -> do
    ghuUser  <- o .: "login" :: Parser T.Text
    ghuUserId    <- o .: "id" :: Parser Int
    return $ MkGHUser {..}

instance FromJSON GHMilestone where
  parseJSON = withObject "User" $ \o -> do
    ghmNumber       <- o .: "number" :: Parser Int
    ghmTitle        <- o .: "title"  :: Parser T.Text
    dueDate         <- o .:? "due_on" :: Parser (Maybe T.Text)
    ghmOpenIssues   <- o .: "open_issues"  :: Parser Int
    ghmClosedIssues <- o .: "closed_issues"  :: Parser Int
    closeAt         <- o .:? "closed_at" :: Parser (Maybe T.Text)
    let ghmRepoName = T.empty
    let ghmDueDate = fmap (\d -> parseTimeOrError  True defaultTimeLocale "%Y-%m-%dT%TZ" (T.unpack d)) dueDate
    let ghmCloseAt = fmap (\d -> parseTimeOrError  True defaultTimeLocale "%Y-%m-%dT%TZ" (T.unpack d)) closeAt
    return $ MkGHMilestone {..}

instance FromJSON GHIssue where
  parseJSON = withObject "Issue" $ \o -> do
    ghiId           <- o .: "id" :: Parser Int
    ghiNumber       <- o .: "number" :: Parser Int
    ghiTitle        <- o .: "title" :: Parser T.Text
    ghiUser         <- o .: "user" :: Parser GHUser
    creationTime    <- o .: "created_at" :: Parser T.Text
    ghiMainAssignee <- o .: "assignee" :: Parser (Maybe GHUser)
    ghiAssignees    <- o .: "assignees" :: Parser [GHUser]
    ghiUrl          <- o .: "html_url" :: Parser T.Text
    let ghiCreationTime = parseTimeOrError  True defaultTimeLocale "%Y-%m-%dT%TZ" (T.unpack creationTime)
    let ghiIsPR = M.member "pull_request" o
    ghiMilestone    <- o .:? "milestone" :: Parser (Maybe GHMilestone)
    return $ MkGHIssue {..}

instance FromJSON EpicChildren where
  parseJSON = withObject "EpicChildren" $ \o -> do
    issues  <- o .: "issues" :: Parser [Object]
    childIssues <- forM issues $ \i -> do
      issueNum <- i .: "issue_number" :: Parser Int
      return issueNum
    return $ EpicChildren childIssues


instance FromJSON AllEpics where
  parseJSON = withObject "Epics" $ \o -> do
    issues  <- o .: "epic_issues" :: Parser [Object]
    epicIssues <- forM issues $ \i -> do
      issueNum <- i .: "issue_number" :: Parser Int
      return issueNum
    return $ AllEpics epicIssues


{-
[{"release_id":"5be5bf0de8e9d45fbb5b5e64",
"title":"Wallet Evolution","description":"All work required to decouple the wallet with BIP-44 and offline transaction signing."
,"start_date":"2018-10-29T12:00:00.000Z"
,"desired_end_date":"2018-12-31T12:00:00.000Z"
,"created_at":"2018-11-09T17:08:29.997Z"
,"closed_at":null,"state":"open"}]
-}


instance FromJSON ZHRelease where
  parseJSON = withObject "Release" $ \o -> do
    zhrId       <- o .: "release_id" :: Parser T.Text
    zhrTitle    <- o .: "title" :: Parser T.Text
    endDate     <- o .: "desired_end_date" :: Parser T.Text
    let zhrEndDate = parseUTCTime (T.unpack endDate)
    return $ MkZHRelease{..}


-- {"repo_id":154148239,"issue_number":1}


instance FromJSON ZHIssueInRelease where
  parseJSON = withObject "Issue in release" $ \o -> do
    zhirIssueId  <- o .: "issue_number" :: Parser Int
    return $ MkZHIssueInRelease{..}

