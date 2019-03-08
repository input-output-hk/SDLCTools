{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE BangPatterns         #-}

module GH.Milestones
(
  extractMilestones
  , Milestone(..)
  , getMileStonesForOneRepo
  , getMileStones
)
where



-- import Debug.Trace (trace)

import            Data.Aeson
import qualified  Data.List as L
import            Data.Maybe (mapMaybe)
import qualified  Data.Set as S
import qualified  Data.Text as T

import            Data.Time.Clock

import            GH.Config
import            GH.Parser
import            GH.Queries
import            GH.Types


getMileStones :: Config -> IO [(String, [GHMilestone])]
getMileStones MkConfig{..} = do
  milestones <- mapM (\repo -> getMileStonesForOneRepo cfg_gh_key repo) cfg_Repos
  return milestones


getMileStonesForOneRepo :: String -> (String, String, Int) -> IO (String, [GHMilestone])
getMileStonesForOneRepo ghKey (user, repo, repoId) = do
  jsons <- getAllMileStonesFromGHRepo ghKey user repo
  let milestones = L.concat $ L.map (\jsonBS ->
          case eitherDecode jsonBS of
            Right ms -> ms
            Left e -> error e) jsons
  return (repo, milestones)

data Milestone = MkMilestone
  { mlRepo      :: T.Text
  , mlName      :: T.Text
  , mlNbIssues  :: Int
  , mlNbInBacklog :: Int
  , mlNbInWip       :: Int
  , mlNbDone        :: Int
  , mlStartTime     :: Maybe UTCTime
  , mlDueTime       :: Maybe UTCTime
  , mlDoneTime      :: Maybe UTCTime
  }


extractMilestones :: [Issue] -> [Milestone]
extractMilestones issues =
  L.map goOneMileStone milestones
  where
  milestones = S.toList $ S.fromList $ mapMaybe (\issue -> ghiMilestone $ iGHIssue issue) issues
  goOneMileStone ml@MkGHMilestone{..} = let
    -- issues for this milestone
    issues' = L.filter (hasMilestone ml) issues
    (t, b, w, d) = getNbIssueInState issues'
    (st, et) = getStartEndDate issues'
    et' = if (t==d) then et else Nothing
    in MkMilestone ghmRepoName ghmTitle t b w d st ghmDueDate et'
  hasMilestone ml issue = (ghiMilestone $ iGHIssue issue) == Just ml



getStartEndDate :: [Issue] -> (Maybe UTCTime, Maybe UTCTime)
getStartEndDate issues =
  L.foldl' proc (Nothing, Nothing) issues
  where
  proc (st, et) MkIssue{..} = let
    st' = comp (<) st (getInProgressTime iStateTransitions)
    et' = comp (>) et (getDoneTime iStateTransitions)
    in (st', et')

comp :: (a -> a -> Bool) -> Maybe a -> Maybe a -> Maybe a
comp _ Nothing tM = tM
comp _ tM Nothing = tM
comp op (Just t1) (Just t2) = if op t1 t2 then Just t1 else Just t2

getNbIssueInState :: [Issue] -> (Int, Int, Int, Int)
getNbIssueInState issues =
  L.foldl' proc (0, 0, 0, 0) issues
  where
  proc (t, b, w, d) MkIssue{..} =
    case iStateTransitions of
    STBacklog _               -> (t+1, b+1, w, d)
    STInProgress _ _          -> (t+1, b, w+1, d)
    STInReview _ _ _          -> (t+1, b, w+1, d)
    STDone _ _ _ _            -> (t+1, b, w, d+1)
    STIllegalStateTransitions -> (t, b, w, d)




