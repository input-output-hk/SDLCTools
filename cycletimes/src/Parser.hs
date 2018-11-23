{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Parser
where

import Debug.Trace (trace)
import            Data.Aeson
import            Data.Aeson.Types (Parser)
import qualified  Data.Text as T

import            Data.Time.Calendar
import            Data.Time.Clock
import            Data.Time.Clock.POSIX
import            Data.Time.Format


import            Types


{-

data GHUser = MkGHUser
  { ghiUser     :: T.Text
    ghiUserId   :: T.Text
  }


data GHIssue = MkGHIssue
  { ghiId               :: Int
  , ghiNumber           :: Int
  , ghiTitle            :: T.Text
  , ghiUser             :: GHUser
  , ghiCreationTime     :: UTCTime
  , ghiMainAssignee     :: GHUser
  , ghiAssignees        :: [GHUser]
  }

-}


instance FromJSON GHUser where
  parseJSON = withObject "User" $ \o -> do
    ghuUser  <- o .: "login" :: Parser T.Text
    ghuUserId    <- o .: "id" :: Parser Int
    return $ MkGHUser {..}


instance FromJSON GHIssue where
  parseJSON = withObject "Issue" $ \o -> do
    ghiId           <- o .: "id" :: Parser Int
    ghiNumber       <- o .: "number" :: Parser Int
    ghiTitle        <- o .: "title" :: Parser T.Text
    ghiUser         <- o .: "user" :: Parser GHUser
    creationTime    <- o .: "created_at" :: Parser T.Text
    ghiMainAssignee <- o .: "assignee" :: Parser GHUser
    ghiAssignees    <- o .: "assignees" :: Parser [GHUser]
    let ghiCreationTime = trace (show creationTime) $ parseTimeOrError  True defaultTimeLocale  "%Y-%m-%dT%TZ"
                                                                        (T.unpack creationTime)
    return $ MkGHIssue {..}













