{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}


{-# LANGUAGE TemplateHaskell #-}

module Meas.Extract.Iohks.Issue
where

-- import Debug.Trace (trace)

import            Control.Lens hiding (element, (.=))

import qualified  Data.List as L
import qualified  Data.Text as T
import qualified  Data.Text.Conversions as T

import            Meas.Extract.Issue
import            Meas.Extract.Misc
import            Meas.Extract.Types
import            Meas.Extract.Iohks.Types
import            Meas.YouTrack.Parser



extractAllIohksIssues :: [GenericIssue] -> [YtIohksIssue]
extractAllIohksIssues genericIssues =
  L.foldl go [] genericIssues
  where
  go issues gIssue = extractIohksIssue gIssue:issues




extractIohksIssue :: GenericIssue -> YtIohksIssue
extractIohksIssue genericIssue =
  --trace (show genericIssue) $
  issue1
  where
  issue1 = foldr updater issue0 (issueFields genericIssue)
  issue0 = defaultIohksIssue {_ytIohksIssueId = issueId genericIssue}
  updater (GCreatedField s)           = set ytIohksCreated (toUTCTime $ read $ T.unpack s)
  updater (GSummaryField s)           = set ytIohksSummary s
  updater (GDescriptionField s)       = set ytIohksDescription s
  updater (GProjectField s)           = set ytIohksProject s
  updater (GNumberField s)            = set ytIohksNumber (read $ T.unpack s)
  updater (GStateField s)             = set ytIohksState (T.fromText s)
  updater (GPriorityField s)          = set ytIohksPriority (T.fromText s)
  updater (GAssigneeField s)          = set ytIohksAssignees (T.splitOn "," s)
  updater (GSubSystemField s)         = set ytIohksSubSystem s
  updater (GFixVersionsField s)       = set ytIohksFixVersions (T.splitOn "," s)
  updater (GAffectedVersionsField s)  = set ytIohksAffectedVersions (T.splitOn "," s)
  updater (GExchangeField s)          = set ytIohksExchange s
  updater (GTargetVersionsField s)    = set ytIohksTargetVersions (T.splitOn "," s)
  updater (GResolutionField s)        = set ytIohksResolution s
  updater (GPlatformField s)          = set ytIohksPlatform s
  updater (GLinkField links)          = \issue ->
    L.foldl' go issue links
    where
    go issue (linkType,  iId) = issue {_ytIohksLinks = (linkType, iId):(_ytIohksLinks issue)}  -- use lens

  updater _ = id



findDevIssue :: [(LinkType, T.Text)] -> Maybe T.Text
findDevIssue links =
  case findLinkedIssues DependsOn links of
    [] -> Nothing
    (i:_) -> Just i




{-}
data YtIohksIssue = MkYtIohksIssue
  { _ytIohksIssueId           :: T.Text
  , _ytiCreated           :: Int
  , _ytiProject           :: T.Text
  , _ytiNumber            :: Int
  , _ytiState             :: IokhsStateValue
  , _ytIohksPriority      :: PriorityValue
  , _yttIohksAssignees         :: [T.Text]
  , _ytIohksSubSystem      :: T.Text
  , _ytIohksFixVersions      :: [T.Text]
  , _ytIohksAffectedVersions      :: [T.Text]
  , _ytIohksExchange      :: T.Text
  , _ytIohksResolution      :: T.Text
  , _ytIohksPlatform      :: T.Text
  , _ytiErrors            :: [String]
  }
-}




