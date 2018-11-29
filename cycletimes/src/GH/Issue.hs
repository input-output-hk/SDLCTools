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
import            GH.StateTransition
import            GH.Types




getIssues :: Config -> IO [Issue]
getIssues MkConfig{..} = do
  issues <- mapM (\(u, r, rid) -> getGHIssuesForRepo u r rid) cfgRepos >>= (return . L.concat)
  let issues1 = L.map (\(r, ghi, ghEvts, zhi, zhEvts) -> MkIssue ghi zhi ghEvts zhEvts (T.pack r) STIllegalStateTransitions) issues
  let issues2 = L.map computeStateTransitions issues1
  return issues2
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



computeStateTransitions :: Issue -> Issue
computeStateTransitions issue@MkIssue{..} =
  issue {iStateTransitions = st}
  where
  MkGHIssue {..} = iGHIssue
  st = getStateTransitions ghiCreationTime $ getStateEvents iGHIssueEvents iZHIssueEvents


