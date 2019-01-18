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
)
where

-- import            Debug.Trace (trace)

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
  markupLines = header ++ L.map row milestones




header :: [String]
header =
  [ "h2. Milestones Dates\n"
  , "||Repo||MileStone||Nb Issues ||Backlog||WIP ||Done|| Date Started|| Due Date|| Date Completed||\n"
  ]

row :: Milestone -> String
row MkMilestone{..} =
  L.concat $ L.intersperse "|" l
  where
  l = [ ""
      , "**"++T.unpack mlRepo++"**", T.unpack mlName, show mlNbIssues
      , show mlNbInBacklog, show mlNbInWip, show mlNbDone
      , maybe "No Started" utctimeToString mlStartTime
      , maybe "No Due Date" utctimeToString mlDueTime
      , maybe " " utctimeToString mlDoneTime
      , "\n" ]



