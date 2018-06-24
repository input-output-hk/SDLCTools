{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}


{-# LANGUAGE TemplateHaskell #-}

module Meas.Extract.Iohks.Types
where

-- import            Debug.Trace (trace)

import            Control.DeepSeq
import            Control.Lens hiding (element, (.=))

import qualified  Data.Text as T


import            GHC.Generics (Generic)

import Meas.Extract.Types



data YtIohksIssue = MkYtIohksIssue
  { _ytIohksIssueId           :: T.Text
  , _ytIohksCreated           :: Int
  , _ytIohksProject           :: T.Text
  , _ytIohksNumber            :: Int
  , _ytIohksState             :: IohksStateValue
  , _ytIohksPriority          :: PriorityValue
  , _ytIohksAssignees         :: [T.Text]
  , _ytIohksSubSystem         :: T.Text
  , _ytIohksFixVersions       :: [T.Text]
  , _ytIohksAffectedVersions  :: [T.Text]
  , _ytIohksExchange          :: T.Text
  , _ytIohksResolution        :: T.Text
  , _ytIohksPlatform          :: T.Text
  , _ytIohksErrors            :: [String]
  , _ytIohksLinks             :: [(LinkType, T.Text)]
  , _ytIohksDevIssue          :: Maybe YtIssue
  }
  deriving (Show, Generic, NFData)

makeLenses ''YtIohksIssue

defaultIohksIssue :: YtIohksIssue
defaultIohksIssue = MkYtIohksIssue
  "" 0 T.empty 0 IohksSubmitted Normal [] T.empty [] [] T.empty T.empty T.empty [] [] Nothing



