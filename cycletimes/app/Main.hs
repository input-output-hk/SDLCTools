{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ScopedTypeVariables   #-}


module Main where



import            Control.Monad

import qualified  Data.List as L
import            Data.Maybe (catMaybes)


import            PR.Extract
import            PR.Pr
import            PR.Report
import            PR.Types

import            GH.Assignee
import            GH.Config
import            GH.Issue
import            GH.Report.Assignees
import            GH.Report.Actionable
import            GH.Report.Confluence.Milestones
import            GH.Report.Estimate
import            GH.Report.Milestones
import            GH.Report.Release
import            GH.Report.StateTransition
import            GH.Types

import            Data.Time.Clock
import            Data.Time.Format

d::UTCTime
d = parseTimeOrError  True defaultTimeLocale "%Y-%m-%dT%TZ" "2018-12-31T12:00:00Z"


main :: IO ()
main = do
  issuesPerRepos <- getIssues config
  mapM_ (\(repo, issues) -> do
    goIssuesOneRepo repo issues
    ) issuesPerRepos

 -- consolidated view
  let allIssues = do
       (_, issues) <- issuesPerRepos
       issue <- issues
       return issue

  goIssuesOneRepo "global" allIssues


  -- Pull Request
  let MkConfig{..} = config
  when cfg_pr $ do
    pullRequestsPerRepo <- getAllPRs cfg_gh_key 100 $ map (\(_, r, _) -> r) cfg_Repos
    mapM_ (\(repo, pullRequests) -> do
      goPrsOneRepo repo pullRequests
      ) pullRequestsPerRepo

    -- consolidated view
    let allPullRequests = do
          (_, pullRequests) <- pullRequestsPerRepo
          pullRequest <- pullRequests
          return pullRequest

    goPrsOneRepo "global" allPullRequests

  where

  goIssuesOneRepo repo issues = do
      generateAssigneeIssueReport  ("files/" ++ repo ++ "/assignements.csv") $ (assigneeMap $ map iGHIssue (onlyInProgressIssues issues))
      generateIssueAssigneeReport  ("files/" ++ repo ++ "/assignees.csv") $ (issueMap $ map iGHIssue (onlyInProgressIssues issues))
      generateActionableForIssues ("files/" ++ repo ++ "/actionable.csv") issues
      generateReleaseReport ("files/" ++ repo ++ "/releases.txt") issues

  -- generate invalid state transition report
      generateStateTransitionReport ("files/" ++ repo ++ "/invalid state transitions.txt") ("files/" ++ repo ++ "/valid state transitions.txt") issues

      -- milestone report
      generateMilestoneReport ("files/" ++ repo ++ "/milestones.csv") issues

      -- Estimate report
      generateEstimateForIssues ("files/" ++ repo ++ "/estimates.csv") issues


      -- Generate Confluence Milestone report

      generateMilestoneConfluenceReport ("files/" ++ repo ++ "/confluence-milestone.md") issues

  onlyInProgressIssues issues = L.filter (\i -> let
                  s = (zhiState $ iZHIssue i)
                  isPR = ghiIsPR $ iGHIssue i
                  in (s == InProgress || s == InReview) && not isPR) issues

  config = MkConfig [("input-output-hk", "cardano-wallet", 154148239)
                    , ("input-output-hk", "ouroboros-network", 149481615)
                    , ("input-output-hk", "cardano-chain", 149791280)
                    , ("input-output-hk", "fm-ledger-rules", 150113380)
                    , ("input-output-hk", "cardano-shell", 154114906)
                    , ("input-output-hk", "iohk-monitoring-framework", 159627884)
                    ]
                    "key"
                    "key"
                    True

goPrsOneRepo :: [Char] -> [PullRequest] -> IO ()
goPrsOneRepo repo pullRequests = do
  makeReport ("files/" ++ repo ++"/PRActionable.csv") $ (catMaybes $ map (\pr -> mkPRAnalysis pr Nothing) pullRequests)
  makeReport ("files/" ++ repo ++"/PRCDetails.csv") $ (concat . catMaybes $ mkPRCDetails <$> pullRequests)



