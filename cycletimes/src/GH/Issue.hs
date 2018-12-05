{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE BangPatterns         #-}

module GH.Issue where



--import Debug.Trace (trace)

import            Control.Concurrent

import            Data.Aeson
import qualified  Data.List as L
import qualified  Data.Map.Strict as M
import            Data.Maybe (catMaybes, fromJust)
import qualified  Data.Text as T

import            Data.Time.Clock

import            GH.Config
import            GH.Epic
import            GH.Queries
import            GH.StateTransition
import            GH.Types


getIssues :: Config -> IO [(String, [Issue])]
getIssues MkConfig{..} = do
  issues <- mapM (\repo -> getIssuesForOneRepo cfg_gh_key cfg_zh_key repo) cfg_Repos
  return issues


getIssuesForOneRepo :: String -> String -> (String, String, Int) -> IO (String, [Issue])
getIssuesForOneRepo ghKey zhKey (user, repo, repoId) = do
  issues <- getGHIssuesForRepo
  let issues1 = L.map (\(r, ghi, ghEvts, zhi, zhEvts) -> MkIssue ghi zhi ghEvts zhEvts (T.pack r) STIllegalStateTransitions) issues
  -- get rid of PR's
  let issues2 = L.filter (not . ghiIsPR . iGHIssue) issues1
  let issues3 = L.map computeStateTransitions issues2

  -- get Issue -> Epic Map
  epicMap <- makeEpicMap zhKey repoId

  -- update issues with Epic parent.
  let issues4 = L.map (\issue@MkIssue{..} -> let
                          p = M.lookup (ghiNumber iGHIssue) epicMap
                          in issue { iZHIssue = iZHIssue {zhiParentEpic = p}})
                      issues3

  -- update issues with children
  let invertedEpicMap = invertMap epicMap
  let issues5 = L.map (\issue@MkIssue{..} ->
                          case M.lookup (ghiNumber iGHIssue) invertedEpicMap of
                          Just children -> do
                            let issue' = issue { iZHIssue  = iZHIssue {zhiChildren = children}}
                                childIssues   = filter ( \MkIssue{..} -> elem (ghiNumber iGHIssue) children) issues4
                                stTransitions = (\ MkIssue{..} -> iStateTransitions) <$> childIssues
                                epicCreationTime  = ghiCreationTime  iGHIssue
                                maybeEpicProgress = getMinInProgressTimeForEpic stTransitions
                                maybeEpicDone     = getMaxDoneTimeForEpic stTransitions
                                maybeBacklogTime  = getMinBacklogTimeForEpic stTransitions
                                backlogTime =
                                    case maybeBacklogTime of
                                    Just t | t < epicCreationTime -> t
                                    Just _                        -> epicCreationTime
                                    Nothing                       -> epicCreationTime
                            case (maybeEpicProgress, maybeEpicDone) of
                              (Nothing,_) -> issue'
                              (Just ip, Nothing) -> issue' { iStateTransitions = STInProgress backlogTime ip }
                              (Just ip, Just dt) -> issue' { iStateTransitions = STDone backlogTime ip dt dt }
                          Nothing -> issue
                      )
                      issues4
  return (repo, issues5)
  where
  getGHIssuesForRepo = do
    jsons <- getAllIssuesFromGHRepo ghKey user repo
    let ghIssues = L.concat $ L.map (\jsonBS ->
                    case eitherDecode jsonBS of
                      Right issues -> issues
                      Left e -> error e) jsons
    mapM proc ghIssues
    where
    proc ghIssue = do
      print ("getting data for: "::String, ghiNumber ghIssue)
      -- poor man rate control
      threadDelay 150000

      zhIssue <- getZHIssueForRepo zhKey repoId ghIssue
      zhIssueEvents <- getZHIssueEventsForRepo zhKey repoId ghIssue
      ghIssueEvents <- getGHIssueEventsForRepo ghKey user repo ghIssue

      return (repo, ghIssue, ghIssueEvents, zhIssue, zhIssueEvents)

getGHIssueEventsForRepo :: String -> String -> String -> GHIssue -> IO [GHIssueEvent]
getGHIssueEventsForRepo ghKey user repo MkGHIssue{..} = do
  jsonBS <- getIssueEventsFromGHRepo ghKey user repo ghiNumber
  let (ghEvtsE :: Either String [Maybe GHIssueEvent]) = eitherDecode jsonBS
  case ghEvtsE of
    Right ghEvts -> return $ catMaybes ghEvts
    Left e -> fail e

getZHIssueForRepo :: String -> Int -> GHIssue -> IO ZHIssue
getZHIssueForRepo zhKey repoId MkGHIssue{..} = do
  jsonBS <- getSingleIssueFromZHRepo zhKey repoId ghiNumber
  let (zhIssueE :: Either String ZHIssue) = eitherDecode jsonBS
  case zhIssueE of
    Right zhIssue -> return zhIssue
    Left e -> fail e

getZHIssueEventsForRepo :: String -> Int -> GHIssue -> IO [ZHIssueEvent]
getZHIssueEventsForRepo zhKey repoId MkGHIssue{..} = do
  jsonBS <- getSingleIssueEventsFromZHRepo zhKey repoId ghiNumber
  let (zhEvtsE :: Either String [Maybe ZHIssueEvent]) = eitherDecode jsonBS
  case zhEvtsE of
    Right zhEvts -> return $ catMaybes zhEvts
    Left e -> fail e



computeStateTransitions :: Issue -> Issue
computeStateTransitions issue@MkIssue{..} =
  issue {iStateTransitions = st}
  where
  MkGHIssue {..} = iGHIssue
  st = getStateTransitions ghiCreationTime $ getStateEvents iGHIssueEvents iZHIssueEvents


invertMap :: M.Map Int Int -> M.Map Int [Int]
invertMap m =
  M.foldrWithKey (\k a acc -> M.insertWith (++) a [k] acc) M.empty m


getMinInProgressTimeForEpic :: [StateTransitions] -> Maybe UTCTime
getMinInProgressTimeForEpic sts = case filter (/= Nothing) progressTimes of
  [] -> Nothing
  ipTimes -> pure . minimum $ fromJust <$> ipTimes
  where
    progressTimes = getInProgressTime <$> sts

getMaxDoneTimeForEpic :: [StateTransitions] -> Maybe UTCTime
getMaxDoneTimeForEpic sts = case elem Nothing doneTimes of
  True -> Nothing
  False -> pure . maximum $ fromJust <$> doneTimes
  where
    -- take care to filter out Illegal State Transitions otherwise
    -- this function will return Nothing whenever State Transitions are illegal
    doneTimes = getDoneTime <$> (L.filter (/= STIllegalStateTransitions) sts)

getMinBacklogTimeForEpic :: [StateTransitions] -> Maybe UTCTime
getMinBacklogTimeForEpic sts = case filter (/= Nothing) backlogTimes of
  [] -> Nothing
  ipTimes -> pure . minimum $ fromJust <$> ipTimes
  where
    backlogTimes = getBacklogTime <$> sts

