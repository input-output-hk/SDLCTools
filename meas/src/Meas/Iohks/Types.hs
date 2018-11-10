{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}


{-# LANGUAGE TemplateHaskell #-}

module Meas.Iohks.Types
where

-- import            Debug.Trace (trace)

import            Control.DeepSeq
import            Control.Lens hiding (element, (.=))

import qualified  Data.Text as T
import            Data.Time.Clock


import            GHC.Generics (Generic)

import Meas.Dev.Types
import Meas.Misc



data YtIohksIssue = MkYtIohksIssue
  { _ytIohksIssueId          :: T.Text
  , _ytIohksSummary          :: T.Text
  , _ytIohksDescription      :: T.Text
  , _ytIohksCreated          :: UTCTime
  , _ytIohksProject          :: T.Text
  , _ytIohksNumber           :: Int
  , _ytIohksState            :: IohksStateValue
  , _ytIohksPriority         :: PriorityValue
  , _ytIohksAssignees        :: [T.Text]
  , _ytIohksSubSystem        :: T.Text
  , _ytIohksFixVersions      :: [T.Text]
  , _ytIohksAffectedVersions :: [T.Text]
  , _ytIohksExchange         :: T.Text
  , _ytIohksResolution       :: T.Text
  , _ytIohksPlatform         :: T.Text
  , _ytIohksTargetVersions   :: [T.Text]
  , _ytIohksErrors           :: [String]
  , _ytIohksLinks            :: [(LinkType, T.Text)]
  , _ytIohksDevIssue         :: Maybe YtIssue
  }
  deriving (Show, Generic, NFData)

makeLenses ''YtIohksIssue

defaultIohksIssue :: YtIohksIssue
defaultIohksIssue = MkYtIohksIssue
  T.empty T.empty T.empty defUTCTime T.empty 0 IohksSubmitted Normal [] T.empty [] [] T.empty T.empty T.empty [] [] [] Nothing



