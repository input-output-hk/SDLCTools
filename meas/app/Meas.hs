{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}


{-# LANGUAGE TemplateHaskell #-}



--import Lib

import Data.Decimal
import Data.Maybe (mapMaybe)
import Debug.Trace(trace)
import Control.Monad.State
import Control.Lens hiding (element, (.=))

import Test.QuickCheck (forAll, elements, quickCheck)
import Test.QuickCheck.Gen (generate, Gen, shuffle, sublistOf, choose, vectorOf)

import Data.Text.Conversions
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.List as L
import Text.Read (readMaybe)
import Control.Applicative

--import qualified Network.HTTP.Types.URI as HTTP
import Control.Applicative
import Control.Monad (when)
import Text.Read (readMaybe)
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified  Data.ByteString.Lazy as LBS

import qualified  Data.Text as T
import            Test.HUnit
import            Test.Framework
import            Test.Framework.Providers.HUnit
import            Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Either
import qualified Data.Vector as V
import Text.XML.JSON.StreamingXmlToJson
import Data.Maybe (catMaybes)
import Data.String
import           Network.HTTP.Simple as HTTP

import Data.List.Split (splitOn)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8



import GHC.Generics (Generic)
import Control.DeepSeq
import Data.Time.Clock.POSIX
import Data.Time.Format
--import System.Locale
--import Data.Csv.Builder
--import Data.Csv.Incremental
import qualified Data.Csv as CSV


import Types

d = print $ formatTime defaultTimeLocale  "%Y%m%d" $ posixSecondsToUTCTime 1525111273



data StateValue =
  Backlog
  |Planning
  |Selected
  |InProgress
  |Review
  |Done
  deriving (Eq, Show, Generic, NFData)

instance IsString StateValue where
  fromString "Backlog"      = Backlog
  fromString "Planning"     = Planning
  fromString "Selected"     = Selected
  fromString "In Progress"  = InProgress
  fromString "Review"       = Review
  fromString "Done"         = Done

-- old stuff in YouTrack ...
  fromString "No State"             = Backlog
  fromString "No state"             = Backlog
  fromString "Open"                 = Backlog
  fromString "Assigned"             = Backlog
  fromString "No value"             = Backlog
  fromString "Waiting for review"   = Review
  fromString "DONT-USE-1"           = Backlog
  fromString "DONT-USE-3"           = Backlog
  fromString "DONT-USE-5"           = Backlog
  fromString "Aborted"              = Done
  fromString "To be discussed"      = Backlog
  fromString "Waiting for build"    = Backlog
  fromString "79-532-1524688761506.Done"    = Done
  fromString "79-532-1524688761506.Backlog"    = Backlog
  fromString "79-532-1524688761506.In Progress"    = InProgress
  fromString "Duplicate"    = Done
  fromString "Verified"    = Done
  fromString "Pool of Ideas"    = Backlog
  fromString "Obsolete"    = Done
  fromString "Submitted"    = Backlog
  fromString "Can't Reproduce"    = Done
  fromString "Blocking"    = Done
  fromString "Postponed"    = Done
  fromString "Waiting for test" = InProgress
  fromString "Waiting to be merged into `master`" = InProgress
  fromString "To be deployed to staging" = InProgress
  fromString "To be deployed to production" = InProgress



  fromString   s         = error $ ("Unknow State: "++ s)




instance FromText StateValue where
  fromText "Backlog"          = Backlog
  fromText "Planning"         = Planning
  fromText "Selected"         = Selected
  fromText "In Progress"      = InProgress
  fromText "Review"           = Review
  fromText "Done"             = Done

-- old stuff in YouTrack ...
  fromText "No State"             = Backlog
  fromText "No state"             = Backlog
  fromText "Open"                 = Backlog
  fromText "Assigned"             = Backlog
  fromText "No value"             = Backlog
  fromText "Waiting for review"   = Review
  fromText "DONT-USE-1"           = Backlog
  fromText "DONT-USE-3"           = Backlog
  fromText "DONT-USE-5"           = Backlog
  fromText "Aborted"              = Done
  fromText "To be discussed"      = Backlog
  fromText "Waiting for build"    = Backlog
  fromText "79-532-1524688761506.Done"    = Done
  fromText "79-532-1524688761506.Backlog"    = Backlog
  fromText "79-532-1524688761506.In Progress"    = InProgress
  fromText "Duplicate"    = Done
  fromText "Verified"    = Done
  fromText "Pool of Ideas"    = Backlog
  fromText "Obsolete"    = Done
  fromText "Submitted"    = Backlog
  fromText "Can't Reproduce"    = Done
  fromText "Blocking"    = Done
  fromText "Postponed"    = Backlog
  fromText "Waiting for test" = InProgress
  fromText "Waiting to be merged into `master`" = InProgress
  fromText "To be deployed to staging" = InProgress
  fromText "To be deployed to production" = InProgress

  fromText s                  = error $ ("Unknow State: "++T.unpack s)


data WaitValue =
  Running
  |Waiting
  deriving (Eq, Show, Generic, NFData)

instance IsString WaitValue where
  fromString "Running"  = Running
  fromString "Waiting"  = Waiting
  fromString "<lost change>"  = Running


  fromString "No wait"  = Running
  fromString   s         = error $ ("Unknow Wait: "++ s)

instance FromText WaitValue where
  fromText "Running"  = Running
  fromText "Waiting"  = Waiting
  fromText "No wait"  = Running
  fromText "<lost change>"  = Running
  fromText s          = error $ ("Unknow Wait: "++ T.unpack s)


data TypeValue =
  TaskType
  |IssueType
  |OtherType
  deriving (Show, Generic, NFData)

instance IsString TypeValue where
  fromString "User Story" = IssueType
  fromString "Bug"        = IssueType
  fromString "Task"       = TaskType
  fromString _            = OtherType

instance FromText TypeValue where
  fromText "User Story" = IssueType
  fromText "Bug"        = IssueType
  fromText "Task"       = TaskType
  fromText _            = OtherType

data ThreeDValue =
  Design
  |Development
  |Documentation
  |Test
  deriving (Show, Generic, NFData)

instance IsString ThreeDValue where
  fromString "Design"         = Design
  fromString "Development"    = Development
  fromString "Documentation"  = Documentation
  fromString "Test"           = Test

instance FromText ThreeDValue where
  fromText "Design"         = Design
  fromText "Development"    = Development
  fromText "Documentation"  = Documentation
  fromText "Test"           = Test


data ROMMandaysValue =
  Days
  |Weeks
  |Months
  |Quarters
  deriving (Show, Generic, NFData)

instance IsString ROMMandaysValue where
  fromString "Day"     = Days
  fromString "Week"    = Weeks
  fromString "Month"   = Months
  fromString "Quarter" = Quarters

instance FromText ROMMandaysValue where
  fromText "Day"     = Days
  fromText "Week"    = Weeks
  fromText "Month"   = Months
  fromText "Quarter" = Quarters


data ValueChange =
  UpdateTime Int
  | Updater T.Text
  | StateChange StateValue StateValue
  | WaitChange WaitValue WaitValue
  deriving (Show, Generic, NFData)

data GenericIssues = GenericIssues [GenericIssue]
  deriving (Show, Generic, NFData)


allIssuesForProjectJson :: String -> String -> IO LBS.ByteString
allIssuesForProjectJson authorization projectName = do
  req <- HTTP.parseRequest ("https://iohk.myjetbrains.com/youtrack/rest/issue/byproject/" ++ projectName)
  let req' =  ((HTTP.setRequestHeaders
                [("Authorization", BS8.pack authorization)])
              . (HTTP.setRequestQueryString
                  [ ("max", Just "4000")
--                  , ("filter", Just "issue id: DEVOPS-82")
                  , ("filter", Just "Type:Task or Bug or {User Story} sort by: {issue id}  desc")
                  ])
              ) req
  resp <- httpBS req'
--  print req
--  print resp
  let xmlBs = getResponseBody resp
  BS.writeFile "tall" xmlBs
  let st = L.concat $ xmlStreamToJSON (BS8.unpack xmlBs)
  let jsonBs = LBS.fromStrict $ BS8.pack $ st
  LBS.writeFile "t.json" jsonBs
  return jsonBs


changesForIssueJson :: String -> T.Text -> IO LBS.ByteString
changesForIssueJson authorization issueId = do
  req <- HTTP.parseRequest ("https://iohk.myjetbrains.com/youtrack/rest/issue/"++T.unpack issueId++"/changes")
  let req' = HTTP.setRequestHeaders
                [("Authorization", BS8.pack authorization)]
                req
  resp <- httpBS req'
  let xmlBs = getResponseBody resp
  let st = L.concat $ xmlStreamToJSON (BS8.unpack xmlBs)
--  putStrLn st
  let jsonBs = LBS.fromStrict $ BS8.pack $ st
  LBS.writeFile "hist.json" jsonBs

  return jsonBs



--data GenericIssue = GenericIssue [GenericIssue]
--  deriving Show

data GenericIssue = MkGenericIssue
  { issueId :: T.Text
  , issueFields :: [GenericIssueField]
  }
  deriving (Show, Generic, NFData)




data GenericIssueField =
  GProjectField T.Text
  |GTypeField T.Text
  |GCreatedField T.Text
  |GNumberField T.Text
  |GStateField T.Text
  |GWaitField T.Text
  |GThreeDField T.Text
  |GDescriptionField T.Text
  |GRomManDaysField T.Text
  |GSquadField T.Text
  |GOwnerField T.Text
  |GPEasyField T.Text
  |GPBenefitsField T.Text
  |GPUrgencyField T.Text
  |GLinkField [(T.Text, T.Text)]
  |GAssigneeField T.Text
  |GPotentialSquadField T.Text
  deriving (Show, Generic, NFData)

data LinkType =
  ParentFor
  |SubTaskOf
  |MustStartAfter
  |IsPreRequisiteFor
  |DependsOn
  |Duplicates
  |RelatesTo
  |IsDuplicatedBy
  |IsRequiredFor
  deriving (Show, Generic, NFData)


instance FromText LinkType where
  fromText "parent for"           = ParentFor
  fromText "subtask of"           = SubTaskOf
  fromText "must start after"     = MustStartAfter
  fromText "is a prerequisite for"  = IsPreRequisiteFor
  fromText "depends on"           = DependsOn
  fromText "duplicates"           = Duplicates
  fromText "relates to"           = RelatesTo
  fromText "is duplicated by"     = IsDuplicatedBy
  fromText "is required for"      = IsRequiredFor
  fromText s      = error ("unknow link role:"++T.unpack s)

data YtIssue = MkYtIssue
  { _ytiIssueId         :: T.Text
  , _ytiCreated         :: Int
  , _ytiProject         :: T.Text
  , _ytiNumber          :: Int
  , _ytiState           :: StateValue
  , _ytiWait            :: WaitValue
  , _ytiROMManday       :: Maybe ROMMandaysValue
  , _ytiPPriorities     :: (Int, Int, Int)
  , _ytiSquad           :: Maybe T.Text
  , _ytiOwner           :: Maybe T.Text
  , _ytiPotentialSquad  :: [T.Text]
  , _ytiLinks           :: [(LinkType, T.Text)]
  , _ytiChanges         :: [(Int, [ValueChange])]
  }
  deriving (Show, Generic, NFData)

makeLenses ''YtIssue

data YtTask = MkYtTask
  { _yttTaskId          :: T.Text
  , _yttCreated         :: Int
  , _yttProject         :: T.Text
  , _yttNumber          :: Int
  , _yttState           :: StateValue
  , _yttWait            :: WaitValue
  , _ytt3D              :: ThreeDValue
  , _yttAssignees       :: [T.Text]
  , _yttLinks           :: [(LinkType, T.Text)]
  , _yttChanges         :: [(Int, [ValueChange])]
  }
  deriving (Show, Generic, NFData)

makeLenses ''YtTask



data Hist =  Hist
  { issue :: Issue
  , changes :: [[ValueChange]]
  }
  deriving (Show, Generic, NFData)

data Issue = Issue T.Text
  deriving (Show, Generic, NFData)


isTask issue = L.any (\i ->
  case i of
  GTypeField "Task"  -> True
  _ -> False
  ) $ issueFields issue
isIssue issue = L.any (\i ->
  case i of
  GTypeField "User Story"  -> True
  GTypeField "Bug"  -> True
  _ -> False
  ) $ issueFields issue

getType issue = L.head $ mapMaybe (\i ->
  case i of
  GTypeField s  -> Just (fromText s)
  _ -> Nothing) (issueFields issue)

checkTask acc (GThreeDField s) =
  if s == "No value" then ("No value for 3D":acc) else acc
checkTask acc _ = acc
checkIssue acc _ = acc

--  check (GPPotentialSquadField T.Text

extractAllIssues :: [GenericIssue] -> ([YtTask], [YtIssue], [(T.Text, [String])])
extractAllIssues issues =
  L.foldl go ([], [], []) issues
  where
  go (tasks, issues, errors) gIssue =
    case getType gIssue of
      TaskType ->
        case  L.foldl checkTask [] (issueFields gIssue) of
          [] -> (extractTask gIssue:tasks, issues, errors)
          errs -> (tasks, issues, (issueId gIssue, errs):errors)
      IssueType ->
        case  L.foldl checkIssue [] (issueFields gIssue) of
          [] -> (tasks, extractIssue gIssue:issues, errors)
          errs -> (tasks, issues, (issueId gIssue, errs):errors)
      _ -> (tasks, issues, errors)



extractTask :: GenericIssue -> YtTask
extractTask issue =
  foldr updater t0 (issueFields issue)
  where
  t0 = MkYtTask (issueId issue) 0 T.empty 0 Backlog Running Development [] [] []
  updater (GCreatedField s)  = set yttCreated (read $ T.unpack s)
  updater (GProjectField s)  = set yttProject s
  updater (GNumberField s)  = set yttNumber (read $ T.unpack s)
  updater (GStateField s)  = set yttState (fromText s)
  updater (GWaitField s)  = set yttWait (fromText s)
  updater (GThreeDField s)  = set ytt3D (fromText s)
  updater (GAssigneeField s)  = set yttAssignees (T.splitOn "," s)
  updater _ = id



extractIssue :: GenericIssue -> YtIssue
extractIssue issue =
  foldr updater t0 (issueFields issue)
  where
  t0 = MkYtIssue (issueId issue) 0 T.empty 0 Backlog Running Nothing (0, 0, 0) Nothing Nothing [] [] []
  updater (GCreatedField s)  = set ytiCreated (read $ T.unpack s)
  updater (GProjectField s)  = set ytiProject s
  updater (GNumberField s)  = set ytiNumber (read $ T.unpack s)
  updater (GStateField s)  = set ytiState (fromText s)
  updater (GWaitField s)  = set ytiWait (fromText s)
  updater (GRomManDaysField s)  =
    case s of
      "No rom mandays" -> id
      _ -> set ytiROMManday (Just $ fromText s)
  updater (GSquadField s)  = set ytiSquad (Just s)
  updater (GOwnerField s)  = set ytiOwner (Just s)
  updater (GPotentialSquadField s)  = set ytiPotentialSquad (T.splitOn "," s)
  updater (GLinkField links)  = \issue ->
    L.foldl' go issue links
    where
    go issue (linkType,  iId) = issue {_ytiLinks = (fromText linkType, iId):(_ytiLinks issue)}  -- use lens

  updater _ = id

--extractSubTasks [GenericIssue] =



data StateTransitions =
  STCanonical [(Int, StateValue)] Int [String]   -- ^ set of InProgress/Review + time done + warning/errors
  |STInProgress Int [String]
  |STInReview Int Int [String]
  |STDoneAndNoWip Int  [String] -- ^last transition before done, time of done transition
  |STUnfinished
  deriving Show

data CFDStateTransition = MkCFDStateTransition
  { cfdCreated    :: Int -- ^ when the issue is created
  , cfdInProgress :: Int
  , cfdReview     :: Maybe Int
  , cfdDone       :: Maybe Int
  }
  deriving Show

mergeWipStates :: Int -> StateTransitions -> Maybe CFDStateTransition
mergeWipStates tCreated (STCanonical trs tDone _) =
  Just $ MkCFDStateTransition tCreated inProgressTime (Just (inProgressTime + sumInProgress)) (Just tDone)
  where
  go :: [(Int, StateValue)] -> [(Int, Int, StateValue)]
  go [] = []
  go ((t1, v1) : []) =  [(t1, tDone, v1)]
  go ((t1, v1) : (t2, v2) : rest) =  (t1, t2, v1) : go ((t2, v2) : rest)

  inProgressTime = fst $ head trs

  valuesLifeSpans = go trs
  sumInProgress = sumPeriods valuesLifeSpans InProgress
--  sumReview = sumPeriods valuesLifeSpans Review

  sumPeriods :: [(Int, Int, StateValue)] -> StateValue -> Int
  sumPeriods valuesLifeSpans val =
    L.foldl' (\acc (tb, te, v) -> acc + te - tb) 0 l
    where
    l = L.filter (\(_,_,v) -> v == val) valuesLifeSpans

mergeWipStates tCreated (STInProgress tip _) = Just $ MkCFDStateTransition tCreated tip Nothing Nothing
mergeWipStates tCreated (STInReview tip tr _) = Just $ MkCFDStateTransition tCreated tip (Just tr) Nothing
mergeWipStates _ _ = Nothing


computeWaits waits record =
  goInReview $ goInProgress record
  where
  goInProgress r@(MkMeasRecord _ _ _ ip _ (Just rv) _ _) =
    r {mrInProgressDone = Just $ computeWaitRatio waits ip rv}
  goInProgress r = r

  goInReview r@(MkMeasRecord _ _ _ _ _ (Just rv) _ (Just done)) =
    r {mrReviewDone = Just $ computeWaitRatio waits rv done}
  goInReview r = r

{-

data MeasRecord = MkMeasRecord
  { mrIssueId         :: T.Text
  , mrProject         :: T.Text
  , mrBacklog         :: Int
  , mrInProgress      :: Int
  , mrInProgressDone  :: Maybe Int
  , mrReview          :: Maybe Int
  , mrReviewDone      :: Maybe Int
  , mrDone            :: Maybe Int
  }

    -}


computeWaitRatio :: [(Int, WaitValue)] -> Int -> Int -> Int
computeWaitRatio waits ti te =
  let waits0 = L.sortOn fst $ (0, Running):waits
      !g10 = trace (show ("==== waits0", ti, te, waits0))  ()
      waits1 = L.takeWhile (\(t,_) -> t < te) waits0 -- we know (te, _) is not included
     -- !g11 = trace (show ("==== waits1", ti, te, waits1))  ()
      waits2 = waits1 ++ [(te, Running)]
      periods1 =  L.zipWith (\(t0, s) (t1, _) -> (t0, t1, s)) waits2 (L.tail waits2)
    --  !g1 = trace (show ("====periods1 ", ti, te, periods1))  ()

      -- ensure there is a wait value at t = ti
      periods2 = L.map (\a@(t0, t1, s) -> if t0 < ti && ti < t1 then (ti, t1, s) else a) periods1
      -- ensure it starts at t = ti
      periods3  = L.filter (\(t0, _, _) -> ti <= t0) periods2
    --  !g = trace (show ("==== periods3", ti, te, periods3))  ()
      timeRunning = L.foldl' (go Running) 0 periods3
--        timeWaiting = L.foldl' (go Waiting) 0 periods3
  in ti + timeRunning
  where
  go ew acc (t0, t1, w) | w == ew = acc + (t1-t0)
  go ew acc (t0, t1, w) = acc



extractStateTransitions :: Int -> [(Int, [ValueChange])] -> StateTransitions
extractStateTransitions created allChanges =
  computeTransitions stateChanges
  where
  stateChanges :: [(Int, StateValue)]
  stateChanges = createStateChanges created allChanges


computeTransitions :: [(Int, StateValue)] -> StateTransitions
computeTransitions changes =
  goWaitEnterWIP changes
  where

  goWaitEnterWIP [] = STUnfinished
  goWaitEnterWIP ((t, s):rest) | s == InProgress || s == Review = goFoundWip [] [(t, s)] rest
  goWaitEnterWIP ((t, s):[]) | s == Done = STDoneAndNoWip t []
  goWaitEnterWIP ((t, s):rest) | s == Done = STDoneAndNoWip t ["State transition(s) after Done state"]
  goWaitEnterWIP ((t, s):rest) = goWaitEnterWIP rest


  goFoundWip errs trs [] = kindWip trs errs -- STCanonical (L.reverse trs) errs
  goFoundWip errs trs ((t, s):rest) | s == InProgress || s == Review = goFoundWip errs ((t, s):trs) rest
  goFoundWip errs trs ((t, s):[]) | s == Done = STCanonical (L.reverse trs) t errs
  goFoundWip errs trs ((t, s):rest)  | s == Done = STCanonical (L.reverse $ trs) t ("State transition(s) after Done state":errs)
  goFoundWip errs trs ((t, s):rest) = goFoundWip ("Spurious State Transition":errs) trs rest

  kindWip :: [(Int, StateValue)] -> [String] -> StateTransitions
  kindWip trs errors =
    case (inProgress, inReview) of
      (((tip, _):_), ((tr, _):_)) | tip <= tr -> STInReview tip tr errors
      (((tip, _):_), ((tr, _):_)) | tr < tip -> STInReview tr tr errors

      (((tip, _):_), []) -> STInProgress tip errors
      ([], ((tr, _):_)) -> STInReview tr tr errors

    where
    inProgress = L.filter isInProgress trs
    inReview  = L.filter isInReview trs

    isInProgress (t, s) = s == InProgress
    isInReview (t, s) = s == Review



createStateChanges :: Int -> [(Int, [ValueChange])] -> [(Int, StateValue)]
createStateChanges created allChanges =
  case stateChanges of
  [] ->[]
  (t, ov, nv):rest -> L.map (\(t, _, nv) -> (t, nv)) $ (created, ov, ov) : stateChanges
  where
  getStateChange :: ValueChange -> Maybe (StateValue, StateValue)
  getStateChange (StateChange ov nv) = Just (ov, nv)
  getStateChange _ = Nothing

  selectStateChange :: (Int, [ValueChange]) -> Maybe (Int, StateValue, StateValue)
  selectStateChange (t, changes) =
    case mapMaybe getStateChange changes of
    [] -> Nothing
    [(ov, nv)] -> Just (t, ov, nv)
    _ -> error "wrong YouTrack changes response"

  stateChanges :: [(Int, StateValue, StateValue)]
  stateChanges = mapMaybe selectStateChange allChanges



main = run authorization

e = changesForIssueJson authorization "CHW-110"

run authorization = do
--  hist <- allChangesForIssue "CDEC-10"
 -- res <- getAll authorization ["DEVOPS", "TSD", "PB", "DDW", "CDEC", "CBR", "CHW", "CO"]
--  res <- getAll authorization ["CSL"]
--  res <- getAll authorization ["CDEC"]
--  res <- getAll authorization ["DEVOPS"]
--  res <- getAll authorization ["DDW"]
--  res <- getAll authorization ["CHW"]
  res <- getAll authorization ["CBR"]
  LBS.writeFile "cfd.csv" LBS.empty
  print "empty file"
  mapM showIt res
  where
  showIt (projectId, tasks, issues, errors) = do
    print ("projectId: ", projectId)
    print tasks
--    print issues

    mapM print errors

    mapM go tasks

    mapM (\task -> do
      let cs = _yttChanges task
      let css = map (\(t, l) -> (t, length l)) cs
      print (_yttTaskId task, css))
      tasks

    let records = mapMaybe createRecords tasks -- $ L.take 40 tasks

    let header = CSV.header ["IssueId", "Backlog", "InProgress", "InProgressDone", "Review", "ReviewDone", "Done", "Project"]
    let csvLBS = CSV.encodeByName header records
    print "append file"
    LBS.appendFile "cfd.csv" csvLBS
    where

    createRecords task = do
      record <- createTaskCFDdata task
      let waitChanges = catMaybes $ findWaitTransitions (_yttChanges task)
      let !j = trace (show (_yttTaskId task)) ()
      let record' = computeWaits waitChanges record
      return record'
      where
      findWaitTransitions allChanges = do
        (t, changes) <- allChanges
        let changes' = do
              change <- changes
              case change of
                WaitChange _ nv -> return $ Just nv
                _ -> return Nothing
        case catMaybes changes' of
          [] -> return Nothing
          (h:_) -> return $ Just (t, h)


    go task = do
      let (trs) = extractStateTransitions (_yttCreated task) (_yttChanges task)
      print (_yttTaskId task, _yttCreated task)
      print trs

      let cfdStateTransitions = mergeWipStates (_yttCreated task) trs
      print cfdStateTransitions

      print $ createStateChanges (_yttCreated task) (_yttChanges task)
      putStrLn ""


createTaskCFDdata :: YtTask -> Maybe MeasRecord
createTaskCFDdata MkYtTask{..} = do
  let trs = extractStateTransitions _yttCreated _yttChanges
  MkCFDStateTransition{..} <- mergeWipStates _yttCreated trs
  return $ MkMeasRecord _yttTaskId _yttProject cfdCreated cfdInProgress Nothing cfdReview Nothing cfdDone



allIssues authorization projectId = do
  jsonBs <- allIssuesForProjectJson authorization projectId
  let (d :: Either String GenericIssues) = eitherDecode jsonBs
  case d of
    Left err -> error err
    Right (GenericIssues gIssues) -> do
      return $ extractAllIssues gIssues


allChangesForIssue authorization issueId = do
  jsonBs <- changesForIssueJson authorization issueId
  let (d :: Either String Hist) = eitherDecode jsonBs
  case d of
    Left err -> error err
    Right hist -> return hist

getAll authorization projectIds = do
  mapM getOneproject projectIds
  where
  getOneproject projectId = do
    print $ "Extracting project: "++ projectId
    (tasks, issues, errors) <- allIssues authorization projectId
    tasks' <- mapM getHistoryForTask tasks
    issues' <- mapM getHistoryForIssue issues
    return (projectId, tasks', issues', errors)

  getHistoryForTask task = do
    putStrLn $ "History for Task: " ++ T.unpack (_yttTaskId task)

    (Hist _ changes) <- allChangesForIssue authorization (_yttTaskId task)
    let !evChanges = force changes
    return $ task {_yttChanges = mergeChanges evChanges}

  getHistoryForIssue issue = do
    putStrLn $ "History for Issue: " ++ T.unpack (_ytiIssueId issue)
    (Hist _ changes) <- allChangesForIssue authorization (_ytiIssueId issue)
    return $ issue {_ytiChanges = mergeChanges changes}

mergeChanges :: [[ValueChange]] -> [(Int, [ValueChange])]
mergeChanges allChanges =
  L.sortOn fst $ L.map go allChanges
  where
  go l =
    case L.foldl proc (Nothing, []) l of
    (Just t, changes) -> (t, changes)
    _ -> error "Bad YouTrack changes"
  proc (_, changes) (UpdateTime t) = (Just t, changes)
  proc (time, changes) change = (time, change:changes)



issueParser =
  withObject "Issues" $ \o -> do
    issueListVal <- o .: "items"
    issues <- withArray "Issue List" (mapM p . V.toList) issueListVal
    return $ GenericIssues issues
  where
  p = withObject "Issue" $ \o -> do
        attrs <- o .: "attrs"
        issueId <- attrs .: "id"
        fieldsVal <- o .: "items"
        fields <- issueFieldsParser fieldsVal
        let !evFields = force fields
        return $ MkGenericIssue issueId evFields

issueFieldsParser value = do
  res <-  withArray "field items" (mapM issueFieldParser . V.toList) value
  return $ catMaybes res

--issueLinkParser :: Parser IssueField
issueLinkParser o = do
  itemsVal <- o .: "items"
  withArray "link elements" (mapM p . V.toList) itemsVal >>= (return . Just . GLinkField)
  where
  p = withObject "link" $ \o -> do
        attrs <- o .: "attrs"
        role <- attrs .: "role"
        issueId <- (o .: "items") >>= parseSingletonArray (withText "field value" pure)
        return (role, issueId)

issueFieldParser =
  withObject "Field" $ \o -> do
    kind <- o .: "name"
    case T.unpack kind of
      "field" -> do
        attrs <- (o .: "attrs")
        name <- attrs .: "name"
        let !r = trace (show (name)) ()
--        let !r = trace (show (name, o)) ()
        case T.unpack name of
          "projectShortName" -> issueSimpleFieldParser GProjectField o
          "numberInProject" -> issueSimpleFieldParser GNumberField o
          "created" -> issueSimpleFieldParser GCreatedField o
          "Type" -> trace (show ("GTypeField", o)) $ issueSimpleFieldParser GTypeField o
          "State" -> issueEnumFieldParser GStateField o
          "Wait" -> issueEnumFieldParser GWaitField o
         -- "description" -> issueSimpleFieldParser DescriptionField o
          "3D" -> issueEnumFieldParser GThreeDField o
          "ROM Mandays" -> issueEnumFieldParser GRomManDaysField o
          "Squad" -> issueEnumFieldParser GSquadField o
          "Owner" -> issueSimpleFieldParser GOwnerField o
          "p_easy" -> issueEnumFieldParser GPEasyField o
          "p_benefits" -> issueEnumFieldParser GPBenefitsField o
          "p_urgency" -> issueEnumFieldParser GPUrgencyField o
          "links" -> issueLinkParser o
          _ -> return Nothing
      _ -> return Nothing

{-

parseSingletonArray :: (Value -> Parser a) -> Value -> Parser a
parseSingletonArray p value =
  withArray "Singleton Array" (\a -> mapM p $ V.toList a) value >>= (return . L.head)
  -}

issueSimpleFieldParser ctor o = do
  itemsVal <- o .: "items"
  value <- parseNamedSingletonArray p itemsVal
  return $ Just $ ctor value
  where
  p = withObject "" $ \o -> do
        (name::T.Text) <- o .: "name"
        case name of
          "value" -> do
            itemsVal <- o .: "items"
            let !r = trace (show ("issueSimpleFieldParser.item", itemsVal)) ()
            parseSingletonArray (withText "field value" pure) itemsVal >>= (return . Just)
          _ -> return Nothing

issueEnumFieldParser ctor o = do
  itemsVal <- o .: "items"
  [value] <- withArray "" (mapM p . V.toList) itemsVal >>= (return . catMaybes)
  return $ Just $ ctor value
  where
  p = withObject "" $ \o -> do
        name <- o .: "name"
        case T.unpack name of
          "value" -> do
            itemsVal <- o .: "items"
            parseSingletonArray (withText "field value" pure) itemsVal >>= (return . Just)
          _ -> return Nothing

instance FromJSON GenericIssues where
  parseJSON = issueParser





-- improve
parseSingletonArray :: (Value -> Parser a) -> Value -> Parser a
parseSingletonArray p value =
  withArray "Singleton Array" (\a -> mapM p $ V.toList a) value >>= (return . L.head)


parseNamedSingletonArray :: (Value -> Parser (Maybe a)) -> Value -> Parser a
parseNamedSingletonArray p value =
  withArray "Singleton Array" (\a -> mapM p $ V.toList a) value >>= (return . L.head . catMaybes)


parseUpdateTime :: Value -> Parser ValueChange
parseUpdateTime = do
  withObject "updated object" $ \o -> do
    itemsVal <- o .: "items"
    res <- parseSingletonArray p itemsVal
    return $ UpdateTime res
  where
  p = withText "update time" $ \s ->
        case readMaybe (T.unpack s) of
          Just n -> return n
          Nothing -> fail "not a number"

parseFieldChange :: (T.Text -> T.Text -> ValueChange) -> Value -> Parser ValueChange
parseFieldChange ctor value = do
    [Left oldVal, Right newVal] <- withArray "changes" (mapM p . V.toList) value
    return $ ctor oldVal newVal
  where
  p = withObject "kind of value" $ \o -> do
    name <- o .: "name"
    case T.unpack name of
      "oldValue" -> (o .: "items") >>= parseSingletonArray (withText "old value" pure) >>= (return . Left)
      "newValue" -> (o .: "items") >>= parseSingletonArray (withText "new value" pure) >>= (return . Right)
     -- "value" -> (o .: "items") >>= parseSingletonArray (withText "updater name" pure) >>= (return . Just)
--      _ -> return Nothing

parseUpdaterName :: Value -> Parser ValueChange
parseUpdaterName  = do
  withObject "updater name object" $ \o -> do
    itemsVal <- o .: "items"
    res <- parseSingletonArray  p itemsVal
    return $ Updater res
  where
  p v = withText "updater name" pure v

parseChangeGroup :: Value -> Parser [ValueChange]
parseChangeGroup value = do
  changes <- withArray "items: change group" (mapM singleChangeParser . V.toList) value
  return $ catMaybes changes
  where
  singleChangeParser =
    withObject "Single Change" $ \o -> do
--      let !l = trace (show "here") $ ()
      (name::T.Text) <- o .: "name"
      case name of
        "field" -> do
          attrsVal <- o .: "attrs"
          name <- withObject "attrs" (\attrs -> attrs .: "name") attrsVal
          (itemsVal :: Value) <- o .: "items"
          case T.unpack name of
            "updaterName" -> parseSingletonArray parseUpdaterName itemsVal >>= (return . Just)
            "updated" -> parseSingletonArray parseUpdateTime itemsVal >>= (return . Just)
            "State" -> (parseFieldChange (\o n -> StateChange (fromText o) (fromText n)) itemsVal) >>= (return . Just)
            "Wait" -> (parseFieldChange (\o n -> WaitChange (fromText o) (fromText n)) itemsVal) >>= (return . Just)
            _ -> return Nothing
        _ -> return Nothing

--trace (show v) $

--itemParser :: Value -> Parser [()]
topItemsParser = withArray "items" (mapM topItemParser . V.toList)

parseIssue o = do
  issueId <- (o .: "attrs") >>= (
                withObject "Issue Attrs" $ \o' -> do
                   o' .: "id"
                )
  return $ Issue issueId

topItemParser =
  withObject "Top item" $ \o -> do
--    let !l = trace (show ("here", o)) $  ()

    name <- o .: "name"
  --  let !l = trace (show ("Passed here")) $  ()
    case T.unpack name of
      "issue" -> parseIssue o >>= (return . Right)
      "change" -> do
        itemsVal <- o .: "items"
        parseChangeGroup itemsVal >>= (return . Left)

instance FromJSON Hist where
  parseJSON = withObject "hist" $ \o -> do
    topItems <-  (o .: "items") >>= withArray "top items" (mapM topItemParser . V.toList)
    let [issue] = rights topItems
    let valueChanges = lefts topItems
    return $ Hist issue valueChanges





{-
    age  <- asum [
      -- The simple “number” case.
      o .: "age",
      -- The more complicated “string” case.
      do s <- o .: "age"
         case readMaybe s of
           Nothing -> fail "not a number"
           Just x  -> return x,
      -- The “tuple” case.
      fst <$> o .: "AGE",
      -- The “John” case.
      do guard (name == "John")
         return 24 ]
    return Person{..}

-}