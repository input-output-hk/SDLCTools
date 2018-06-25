{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}


{-# LANGUAGE TemplateHaskell #-}

module Meas.Extract.Issue
where

-- import Debug.Trace (trace)

import            Control.Lens hiding (element, (.=))

import qualified  Data.List as L
import            Data.Maybe (mapMaybe)
import qualified  Data.Text as T
import qualified  Data.Text.Conversions as T

import            Meas.Extract.Types
import            Meas.YouTrack.Parser



extractAllIssues :: [GenericIssue] -> ([YtTask], [YtIssue])
extractAllIssues genericIssues =
  L.foldl go ([], []) genericIssues
  where
  go (tasks, issues) gIssue =
    case getType gIssue of
      (TaskType, Just parent) -> (extractTask parent gIssue:tasks, issues)
      (IssueType, _) -> (tasks, extractIssue gIssue:issues)
      _ -> (tasks, issues)



extractTask :: T.Text -> GenericIssue -> YtTask
extractTask parent genericIssue =
  case L.foldl checkTask [] (issueFields genericIssue) of
  [] -> task1
  errors -> set yttErrors errors task0
  where
  task1 = foldr updater task0 (issueFields genericIssue)
  task0 = defaultTask {_yttTaskId = issueId genericIssue, _yttParent = parent}
  updater (GCreatedField s)   = set yttCreated (read $ T.unpack s)
  updater (GProjectField s)   = set yttProject s
  updater (GNumberField s)    = set yttNumber (read $ T.unpack s)
  updater (GStateField s)     = set yttState (T.fromText s)
  updater (GWaitField s)      = set yttWait (T.fromText s)
  updater (GThreeDField s)    = set ytt3D (T.fromText s)
  updater (GAssigneeField s)  = set yttAssignees (T.splitOn "," s)
  updater _ = id



extractIssue :: GenericIssue -> YtIssue
extractIssue genericIssue =
  case L.foldl checkIssue [] (issueFields genericIssue) of
  [] -> issue1
  errors -> set ytiErrors errors issue0
  where
  issue1 = foldr updater issue0 (issueFields genericIssue)
  issue0 = defaultIssue {_ytiIssueId = issueId genericIssue}
  updater (GCreatedField s)     = set ytiCreated (read $ T.unpack s)
  updater (GDueDateField s)     = set ytiDueDate (read $ T.unpack s)
  updater (GProjectField s)     = set ytiProject s
  updater (GNumberField s)      = set ytiNumber (read $ T.unpack s)
  updater (GStateField s)       = set ytiState (T.fromText s)
  updater (GWaitField s)        = set ytiWait (T.fromText s)
  updater (GRomManDaysField s)  =
    case s of
      "No rom mandays" -> id
      _ -> set ytiROMManday (Just $ T.fromText s)
  updater (GSquadField s)           = set ytiSquad (Just s)
  updater (GOwnerField s)           = set ytiOwner (Just s)
  updater (GPotentialSquadField s)  = set ytiPotentialSquad (T.splitOn "," s)
  updater (GTargetVersions s)       = set ytiTargetVersions (T.splitOn "," s)
  updater (GLinkField links)        = \issue ->
    L.foldl' go issue links
    where
    go issue (linkType,  iId) = issue {_ytiLinks = (linkType, iId):(_ytiLinks issue)}  -- use lens

  updater _ = id


{-
Determine the type of a Generic Issue.

* User Stories, Bug are considered as IssueType
* A Task with a parent is considered as IssueType (subtask of some other issue)
* A Task without any parent is considered as a TaskType.
-}

getType :: GenericIssue -> (TypeValue, Maybe T.Text)
getType issue = L.head $ mapMaybe (\i ->
  case i of
  GTypeField t  -> Just $ proc (T.fromText t)
  _ -> Nothing) (issueFields issue)
  where
  proc TaskType =
    case findParent issue of
    Just p  -> (TaskType, Just p)
    Nothing -> (IssueType , Nothing)
  proc t = (t, Nothing)

findParent :: GenericIssue -> Maybe T.Text
findParent issue = do
  links <- findLinkField issue
  let elems = findLinkedIssues SubTaskOf links
  case elems of
    [] -> Nothing
    (p:_) -> Just p


findLinkField :: GenericIssue -> Maybe [(LinkType, T.Text)]
findLinkField issue =
  go $ issueFields issue
  where
  go [] = Nothing
  go (h:t) =
    case h of
    GLinkField links  -> Just links
    _ -> go t


findLinkedIssues :: LinkType -> [(LinkType, T.Text)] -> [T.Text]
findLinkedIssues linkType links =
  L.foldl (\acc (lt, t) -> if lt == linkType then t:acc else acc) [] links



-- GLinkField [(T.Text, T.Text)]

checkTask :: [String] -> GenericIssueField -> [String]
checkTask acc (GThreeDField s) =
  if s == "No value" then ("No value for 3D":acc) else acc
checkTask acc _ = acc

--  NOTE: check (GPPotentialSquadField T.Text
checkIssue :: a -> b -> a
checkIssue acc _ = acc

