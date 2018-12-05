{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE RecordWildCards   #-}

module GH.Assignee where

import qualified  Data.List as L
import qualified  Data.Map.Strict as M
import qualified  Data.Set as S

import            GH.Types

assigneeMap :: [GHIssue] -> M.Map GHUser (S.Set GHIssue)
assigneeMap issues =
  L.foldl fproc M.empty issues
  where
  fproc acc issue@MkGHIssue{..} =
    let acc' =  case ghiMainAssignee of
                Just user -> M.insertWith S.union user (S.singleton issue) acc
                Nothing -> acc
    in L.foldl (fproc' issue) acc' ghiAssignees
  fproc' issue acc user = M.insertWith S.union user (S.singleton issue) acc


issueMap :: [GHIssue] -> M.Map GHIssue (S.Set GHUser)
issueMap issues =
  L.foldl fproc M.empty issues
  where
  fproc acc issue@MkGHIssue{..} =
    let acc' =  case ghiMainAssignee of
                Just user -> M.insertWith S.union issue (S.singleton user) acc
                Nothing -> M.insertWith S.union issue S.empty acc
    in L.foldl (fproc' issue) acc' ghiAssignees
  fproc' issue acc user = M.insertWith S.union issue (S.singleton user) acc







