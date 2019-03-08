{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}


{-# LANGUAGE TemplateHaskell #-}

module GH.Report.Confluence.Milestones
(
  generateMilestoneConfluenceReport
  , generateMilestoneWeeklyReport
)
where

import            Debug.Trace (trace)

import qualified  Data.List as L
import qualified  Data.Text as T
import            Data.Time.Clock
import            Data.Time.Format

import            GHC.Exts

import            GH.Types
import            GH.Milestones


utctimeToString :: UTCTime -> String
utctimeToString t = formatTime defaultTimeLocale  "%d/%m/%Y" t





generateMilestoneConfluenceReport :: String -> [Issue] -> IO ()
generateMilestoneConfluenceReport filename issues = do
  writeFile filename ""
  mapM_ (appendFile filename) markupLines
  where
  milestones = sortWith (\MkMilestone{..} -> (mlRepo, mlDueTime)) $ extractMilestones issues
  markupLines = headerConfluence ++ L.map rowConfluence milestones


headerConfluence :: [String]
headerConfluence =
  [ "||Repo||MileStone||Nb Issues ||Backlog||WIP ||Done|| % Completed ||Date Started|| Planned Due Date|| Actual Date Completed||\n"
  ]

rowConfluence :: Milestone -> String
rowConfluence MkMilestone{..} =
  L.concat $ L.intersperse "|" l
  where
  l = [ ""
      , T.unpack mlRepo, T.unpack mlName, show mlNbIssues
      , show mlNbInBacklog, show mlNbInWip, show mlNbDone, show (ceiling (fromIntegral mlNbDone / fromIntegral mlNbIssues * 100.0))
      , maybe "No Started" utctimeToString mlStartTime
      , maybe "No Due Date" utctimeToString mlDueTime
      , maybe " " utctimeToString mlDoneTime
      , "\n" ]




generateMilestoneWeeklyReport :: String -> [GHMilestone] -> IO ()
generateMilestoneWeeklyReport filename milestones = do
  writeFile filename ""
  mapM_ (appendFile filename) markupLines
  where
  sortedMilestones = sortWith (\MkGHMilestone{..} -> (ghmRepoName, ghmDueDate)) $ milestones
  markupLines = headerWeekly ++ L.map rowWeekly sortedMilestones



toWorkStream :: String -> String
toWorkStream "cardano-chain"              = "Ledger Handover"
toWorkStream "cardano-wallet"             = "Wallet Backend"
toWorkStream "fm-ledger-rules"            = "Ledger Delegation"
toWorkStream "ouroboros-network"          = "Network & Consensus"
toWorkStream "iohk-monitoring-framework"  = "Monitoring & Performance"
toWorkStream "cardano-shell"              = "Integration Shell"



headerWeekly :: [String]
headerWeekly =
  [ "||Milestone|| % Completed || Planned Due Date|| Actual Date Completed||Status Update||\n"
  ]

rowWeekly :: GHMilestone -> String
rowWeekly MkGHMilestone{..} =
  L.concat $ L.intersperse "|" l
  where
  l = [ " "
      , T.unpack ghmTitle
      , if nb == 0 then " " else (show (ceiling (fromIntegral ghmClosedIssues / fromIntegral nb * 100.0)) ++ "%")
      , maybe "No Due Date" utctimeToString ghmDueDate
      , maybe " " utctimeToString ghmCloseAt
      , " "
      , "\n" ]
  nb = ghmOpenIssues + ghmClosedIssues


