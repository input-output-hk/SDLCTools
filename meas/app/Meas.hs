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
import Control.Lens hiding (element)

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




data StateValue =
  Backlog
  |Planning
  |Selected
  |InProgress
  |Review
  |Done
  deriving (Show, Generic, NFData)

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



  fromText s                  = error $ ("Unknow State: "++T.unpack s)


data WaitValue =
  Running
  |Waiting
  deriving (Show, Generic, NFData)

instance IsString WaitValue where
  fromString "Running"  = Running
  fromString "Waiting"  = Waiting

  fromString "No wait"  = Running
  fromString   s         = error $ ("Unknow Wait: "++ s)

instance FromText WaitValue where
  fromText "Running"    = Running
  fromText "Waiting"   = Waiting
  fromText   s         = error $ ("Unknow Wait: "++ T.unpack s)


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
  | StateChange StateValue
  | WaitChange WaitValue
  deriving (Show, Generic, NFData)

data GenericIssues = GenericIssues [GenericIssue]
  deriving (Show, Generic, NFData)


allIssuesForProjectJson :: String -> String -> IO LBS.ByteString
allIssuesForProjectJson authorization projectName = do
  req <- HTTP.parseRequest ("https://iohk.myjetbrains.com/youtrack/rest/issue/byproject/" ++ projectName)
  let req' =  ((HTTP.setRequestHeaders
                [("Authorization", BS8.pack authorization)])
              . (HTTP.setRequestQueryString
                  [ ("max", Just "2000")
                  , ("filter", Just "Type: Task #{User Story} #Bug")
                  ])
              ) req
  resp <- httpBS req'
  let xmlBs = getResponseBody resp
--  BS.writeFile "tall" xmlBs
  let st = L.concat $ xmlStreamToJSON (BS8.unpack xmlBs)
  let jsonBs = LBS.fromStrict $ BS8.pack $ st
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
  , _ytiChanges         :: [ValueChange]
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
  , _yttChanges         :: [ValueChange]
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


--checkGenericIssue :: GenericIssue -> [String]
--checkGenericIssue issue =
--  if isTask issue
--  then L.foldl checkTask [] (issueFields issue)
--  else
--    if isIssue issue
--    then L.foldl checkIssue [] (issueFields issue)
--    else []
--  where

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
{-}
data StateTransition =
  |STCanonical [(Int, StateValue)] [String]   -- ^ set of InProgress/Review/Done + warning/errors
  |DoneAndNoWip Int Int  [String] -- ^last transition before done, time of done transition
  |

computeTransitions l =

  where
  goWaitEnterWIP tp [] = STCanonical [] []
  goWaitEnterWIP tp ((t, s):rest) | s == InProgress || s == Review = goFoundWip [(t, s)] rest
  goWaitEnterWIP tp ((t, s):[]) | s == Done = DoneAndNoWip tp t []
  goWaitEnterWIP tp ((t, s):rest) | s == Done = DoneAndNoWip tp t ["State transition(s) after Done state"]
  goWaitEnterWIP tp ((t, s):rest) = goWaitEnterWIP t rest


  goFoundWip trs [] = STCanonical trs []
  goFoundWip trs ((t, s):rest) = | s == InProgress || s == Review = goFoundWip ((t, s):trs) rest


  goWaitDone

  goFoundDoneNoWip
-}

main = return ()

run authorization = do
--  hist <- allChangesForIssue "CDEC-10"
  res <- getAll authorization ["PB"]
  mapM showIt res
  where
  showIt (projectId, tasks, issues, errors) = do
    print ("projectId: ", projectId)
    print tasks
    print issues
    mapM print errors

--  print hist

--main2 = do
--  res <- allIssues "CDEC"
--  where
--  showIt (projectId, tasks, issues, errors) = do
--    print $ L.length tasks
--    print $ L.length issues
--    print errors


allIssues authorization projectId = do
  jsonBs <- allIssuesForProjectJson authorization projectId
  let (d :: Either String GenericIssues) = eitherDecode jsonBs
  case d of
    Left err -> error err
    Right (GenericIssues gIssues) -> return $ extractAllIssues gIssues




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
    return $ task {_yttChanges = L.concat evChanges}

  getHistoryForIssue issue = do
    putStrLn $ "History for Issue: " ++ T.unpack (_ytiIssueId issue)
    (Hist _ changes) <- allChangesForIssue authorization (_ytiIssueId issue)
    return $ issue {_ytiChanges = L.concat changes}

{-}
mai
main2 = do
  let (tasks, issues, errors) = allIssues
  print $ L.length tasks
  print $ L.length issues
  print errors

allChangesForIssue issueId = do
  jsonBs <- allChangesForIssue issueId
  let (d :: Either String Hist) = eitherDecode jsonBs
  case d of
    Left err -> error err
    Right hist -> hist
-}
--      let (tasks, issues, errors)  = extractAllIssues gIssues
--      print $ L.length tasks
--      print $ L.length issues
--      print errors
--  return ()


{-
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
  , _ytiChanges         :: [ValueChange]
  }

data GenericIssue = MkGenericIssue
  { issueId :: T.Text
  , issueFields :: [GenericIssueField]
  }


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
  |GPLinkField [(T.Text, T.Text)]
  |GPPotentialSquadField T.Text
--  updater (GProjectField s)  = set yttProject
-}


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
        case T.unpack name of
          "projectShortName" -> issueSimpleFieldParser GProjectField o
          "numberInProject" -> issueSimpleFieldParser GNumberField o
          "created" -> issueSimpleFieldParser GCreatedField o
          "Type" -> issueSimpleFieldParser GTypeField o
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

issueSimpleFieldParser ctor o = do
  itemsVal <- o .: "items"
  value <- parseSingletonArray p itemsVal
  return $ Just $ ctor value
  where
  p = withObject "" $ \o -> do
        itemsVal <- o .: "items"
        parseSingletonArray (withText "field value" pure) itemsVal

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






parseSingletonArray :: (Value -> Parser a) -> Value -> Parser a
parseSingletonArray p value =
  withArray "Singleton Array" (\a -> mapM p $ V.toList a) value >>= (return . L.head)


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

parseFieldChange :: (T.Text -> ValueChange) -> Value -> Parser ValueChange
parseFieldChange ctor value = do
    res <- withArray "changes" (mapM p . V.toList) value
    let [res'] = catMaybes res
    return $ ctor res'
  where
  p = withObject "kind of value" $ \o -> do
    name <- o .: "name"
    case T.unpack name of
      "newValue" -> (o .: "items") >>= parseSingletonArray (withText "updater name" pure) >>= (return . Just)
      "value" -> (o .: "items") >>= parseSingletonArray (withText "updater name" pure) >>= (return . Just)
      _ -> return Nothing

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
            "State" -> (parseFieldChange (StateChange . fromString . T.unpack)) itemsVal >>= (return . Just)
            "Wait" -> (parseFieldChange (WaitChange . fromString . T.unpack)) itemsVal >>= (return . Just)
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