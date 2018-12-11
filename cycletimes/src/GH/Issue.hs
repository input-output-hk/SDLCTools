{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE BangPatterns         #-}

module GH.Issue where



-- import Debug.Trace (trace)

import            Control.Concurrent
import            Control.Monad (foldM)

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
  -- reconcile issue and release
  issueInReleaseMap <- getZHReleaseForIssues zhKey repoId
  let issues6 = updateIssuesWithReleaseForRepo issueInReleaseMap issues5

  return (repo, issues6)
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

-- getZHReleaseForIssues :: String -> Int -> IO (M.Map Int ZHRelease)

updateIssuesWithReleaseForRepo :: M.Map Int ZHRelease -> [Issue] -> [Issue]
updateIssuesWithReleaseForRepo issueInReleaseMap issues =
  L.map proc issues
  where
  proc issue@MkIssue{..} = let
    MkGHIssue{..} = iGHIssue
    issue1 = updateRelease (M.lookup ghiNumber issueInReleaseMap) issue
    issue2 = updateInheritedRelease ((M.lookup ghiNumber issueMap) >>= (\p -> M.lookup p issueInReleaseMap)) issue1
    in
    issue2

  updateRelease releaseM issue@MkIssue{..} = let
    MkZHIssue{..} = iZHIssue
    in
    issue {iZHIssue = iZHIssue {zhiRelease = releaseM}}

  updateInheritedRelease releaseM issue@MkIssue{..} = let
    MkZHIssue{..} = iZHIssue
    in
    issue {iZHIssue = iZHIssue {zhiInheritedRelease = releaseM}}

  issueMap =
    L.foldl' foldProc M.empty issues
    where
    foldProc acc MkIssue{..} = let
      MkZHIssue{..} = iZHIssue
      MkGHIssue{..} = iGHIssue
      in  case zhiParentEpic of
          Just epicNumber -> M.insert ghiNumber epicNumber acc
          Nothing -> acc

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




getZHReleaseForIssues :: String -> Int -> IO (M.Map Int ZHRelease)
getZHReleaseForIssues zhKey repoId = do
  relJsonBS <- getReleaseFromZHRepo zhKey repoId
  let (zhReleasesE :: Either String [ZHRelease]) = eitherDecode relJsonBS
  case zhReleasesE of
    Right zhReleases -> do
      foldM foldProc M.empty zhReleases
    Left e -> fail e
  where
  foldProc acc release@MkZHRelease{..} = do
    jsonBS <- getIssuesInReleaseFromZH zhKey zhrId
    let (zhIssuesE :: Either String [ZHIssueInRelease]) = eitherDecode jsonBS
    case zhIssuesE of
      Right zhIssues ->
        return $ L.foldl' (\acc' (MkZHIssueInRelease i) -> M.insert i release acc') acc zhIssues
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

