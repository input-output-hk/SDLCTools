{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

module Meas.Test.Types
where
import Debug.Trace (trace)

import            Control.Monad
import            GHC.Generics
import            Data.Aeson
import            Data.Aeson.Types
import            Data.Text (Text)
import qualified  Data.Text as T
import qualified  Data.ByteString.Lazy as BL
import            Data.Vector (toList)
import qualified  Data.Char as C
import            Control.Lens
import            Control.Lens.TH

import            Data.Time.Calendar
import            Data.Time.Clock
import            Data.Time.Clock.POSIX
import            Data.Time.Format

import qualified  Data.ByteString.Char8 as BS8
import            Network.HTTP.Simple as HTTP

import           Meas.Extract.Misc


-- TODO : move them in a misc module or wait until everything is moved in the "meas" package
-- toDay :: Int -> Day
-- toDay n = utctDay $ posixSecondsToUTCTime (fromIntegral $ n `div` 1000)
--
-- toUTCTime :: Int -> UTCTime
-- toUTCTime n = posixSecondsToUTCTime (fromIntegral $ n `div` 1000)



getAllTestIssues :: String -> [(String, String)] -> IO [(String, [YttpIssue])]
getAllTestIssues authorization projects = do
  mapM getOneproject projects
  where
  getOneproject (projectId, query) = do
    print $ "Extracting Test project: "++ projectId
    issues <- allTestIssuesForProjectJson authorization projectId query
    print ("Found # issues: " ++ (show $ length issues))
    return (projectId, issues)

allTestIssuesForProjectJson :: String -> String -> String -> IO [YttpIssue]
allTestIssuesForProjectJson authorization projectName query = do
  req <- HTTP.parseRequest ("https://iohk.myjetbrains.com/youtrack/rest/issue/byproject/" ++ projectName)
  let req' =  (HTTP.setRequestHeaders
                [ ("Authorization", (BS8.pack authorization))
                , ("Accept", "application/json")
                ])

              . (HTTP.setRequestQueryString
                  [ ("max", Just "4000")
                  , ("filter", Just $ BS8.pack query)
                  ])

              $ req
  resp <- httpBS req'
  let !jsonBs = getResponseBody resp
  let resE = eitherDecodeStrict jsonBs :: Either String [YttpIssue]
  case resE of
    Right issues -> return issues
    Left e -> error e





data TypeVal =
  Review
  |DocumentRequest
  |Risk
  |Action
  |Issue
  |TestCase
  |Decision
  deriving (Show, Eq, Generic)

data State =
  Open
  |Obsolete
  |Verified
  |Blocking
  |Done
  |Selected
  deriving (Show, Eq, Generic)

data Priority =
  ShowStopper
  |High
  |Major
  |Medium
  |Low
  |Normal
  |Critical
  |Minor
  deriving (Show, Eq, Generic)

data ReviewStatus =
  Wip
  |ToBeReviewed
  |ReviewDone
  |NeedsUpdate
  |ToBeDeleted
  deriving (Show, Eq, Generic)

data AutomationStatus =
  Manual
  |Automated
  |ToBeAutomated
  deriving (Show, Eq, Generic)

data TestResult =
  Passed
  |Failed
  deriving (Show, Eq, Generic)

data TargetOS =
  Linux
  |Windows
  |AllPlatforms
  |MacOS
  |AnySingle
  deriving (Show, Eq, Generic)

data TestingType =
  IntegrationTest
  |UITest
  |E2ETest
  |APITest
  |ComponentTest
  |UXTest
  |NFT
  |UnitTest
  deriving (Show, Eq, Generic)

data BrowserVersion =
  GoogleChrome68
  |Opera55
  |GoogleChrome69
  deriving (Show, Eq, Generic)

data Link = MkLink
  { lValue :: Text
  , lType  :: Text
  , lRole  :: Text
  }
  deriving (Show, Eq, Generic)


data YttpIssue = MkYttpIssue
  { _yttpiIssueId           :: Text
  , _yttpiType              :: TypeVal
  , _yttpiSummary           :: Summary
  , _yttpiDescription       :: Description
  , _yttpiCreated           :: Created
  , _yttpiUpdatedAt         :: Maybe UpdatedAt
  , _yttpiProject           :: Project
  , _yttpiNumber            :: Number
  , _yttpiState             :: State
  , _yttpiPriority          :: Priority
  , _yttpiReviewStatus      :: ReviewStatus
  , _yttpiAutomationStatus  :: AutomationStatus
  , _yttpiInRegressionSuite :: Bool
  , _yttpiInSmokeTest       :: Bool
  , _yttpiExecutionTime     :: Maybe ExecutionTime
  , _yttpiPassedInVersions  :: [Version]
  , _yttpiFailedInVersions  :: [Version]
  , _yttpiBlockedInVersions :: [Version]
  , _yttpiCoveredComponents :: [Component]
  , _yttpiTestResult        :: Maybe TestResult
  , _yttpiTargetOS          :: [TargetOS]
  , _yttpiTestingType       :: [TestingType]
  , _yttpBrowserVersion     :: [BrowserVersion]
  , _yttpLinks              :: [Link]
  }
  deriving (Show, Eq, Generic)

type Summary           = Text
type Description       = Text
type Created           = UTCTime
type UpdatedAt         = UTCTime
type Project           = Text
type Number            = Int
type InRegressionSuite = Bool
type InSmokeTest       = Bool
type ExecutionTime     = Int
type Version           = Text
type Component         = Text

makeLenses ''YttpIssue


data YTField =
  F1 TypeVal
  | F2  Summary
  | F3  Description
  | F4  Created
  | F5  (Maybe UpdatedAt)
  | F6  Project
  | F7  Number
  | F8  State
  | F9  Priority
  | F10 ReviewStatus
  | F11 AutomationStatus
  | F12 InRegressionSuite
  | F13 InSmokeTest
  | F14 (Maybe ExecutionTime)
  | F15 [Version]              -- passed
  | F16 [Version]              -- failed
  | F17 [Version]              -- blocked
  | F18 [Component]
  | F19 (Maybe TestResult)
  | F20 [TargetOS]
  | F21 [TestingType]
  | F22 [BrowserVersion]
  | F23 [Link]
  | NotRequired
  deriving (Eq, Show, Generic)


pYTField :: Value -> Parser (Text, YTField)
pYTField o@(Object oo) = do
    name <- oo .: "name" :: Parser Text
    (name,) <$> case name of
        "Type"        -> pType o
        "summary"     -> pSummary o
        "description" -> pDescription o
        "created"     -> pCreated o
        "updated"     -> pUpdated o
        "projectShortName"  -> pProject o
        "numberInProject"   -> pNumber o
        "State"             -> pState o
        "Priority"          -> pPriority o
        "Review status"     -> pReviewStatus o

        "Automation status" -> pAutomationStatus o
        "In regression suite"    -> pInRegressionSuite o
        "In Smoke Test"          -> pInSmokeTest o

        "Execution time (hours)" -> pExecutionTime o
        "Passed in Versions"     -> pPassedVersions o
        "Failed  in Versions"    -> pFailedVersions o

        "Blocked in Versions"    -> pBlockedVersions o
        "Covered Components"     -> pCoveredComponents o
        "Test Result"            -> pTestResult o
        "Target OS"              -> pTargetOS o
        "Testing Type"           -> pTestingType o
        "Browser and Version"    -> pBrowserAndVersion o
        "links"                  -> pLinks o
        _ -> pure NotRequired


pType :: Value -> Parser YTField
pType = withObject "Object" $ \o -> do
  [value] <- o .: "value" :: Parser [Text]
  pure . F1 $ case value of
    "Review"           -> Review
    "Document Request" -> DocumentRequest
    "Risk"             -> Risk
    "Action"           -> Action
    "Issue"            -> Issue
    "Test Case"        -> TestCase
    "Decision"         -> Decision


pSummary :: Value -> Parser YTField
pSummary = withObject "Object" $ \o -> do
  value <- o .: "value"
  pure $ F2 value


pDescription :: Value -> Parser YTField
pDescription = withObject "Object" $ \o -> do
  value <- o .: "value"
  pure $ F3 value

pCreated :: Value -> Parser YTField
pCreated = withObject "Object" $ \o -> do
  value <- o .: "value"
  pure $ F4 $ toUTCTime (read value)


pUpdated :: Value -> Parser YTField
pUpdated = withObject "Object" $ \o -> do
  value <- o .: "value" :: Parser String
  pure . F5 $ case value of
    num | all C.isDigit num -> pure $ toUTCTime (read num :: Int)
    _ -> Nothing

pProject :: Value -> Parser YTField
pProject = withObject "Object" $ \o -> do
  value <- o .: "value"
  pure $ F6 value

pNumber :: Value -> Parser YTField
pNumber = withObject "Object" $ \o -> do
  value <- o .: "value"
  pure $ F7 (read value)

pState :: Value -> Parser YTField
pState = withObject "Object" $ \o -> do
  [value] <- o .: "value" :: Parser [Text]
  pure . F8 $ case value of
    "Open"     -> Open
    "Obsolete" -> Obsolete
    "Verified" -> Verified
    "Blocking" -> Blocking
    "Done"     -> Done
    "Selected" -> Selected

pPriority :: Value -> Parser YTField
pPriority = withObject "Object" $ \o -> do
  [value] <- o .: "value" :: Parser [Text]
  pure . F9 $ case value of
    "ShowStopper" -> ShowStopper
    "High"        -> High
    "Major"       -> Major
    "Medium"      -> Medium
    "Low"         -> Low
    "Normal"      -> Normal
    "Critical"    -> Critical
    "Minor"       -> Minor

pReviewStatus :: Value -> Parser YTField
pReviewStatus = withObject "Object" $ \o -> do
  [value] <- o .: "value" :: Parser [Text]
  pure . F10 $ case value of
    "WIP"            -> Wip
    "To be reviewed" -> ToBeReviewed
    "Review done"    -> ReviewDone
    "Needs update"   -> NeedsUpdate
    "To be deleted"  -> ToBeDeleted


pAutomationStatus :: Value -> Parser YTField
pAutomationStatus = withObject "Object" $ \o -> do
  [value] <- o .: "value" :: Parser [Text]
  pure . F11 $ case value of
    "Manual" -> Manual
    "Automated" -> Automated
    "To be automated" -> ToBeAutomated

pInRegressionSuite :: Value -> Parser YTField
pInRegressionSuite = withObject "Object" $ \o -> do
  [value] <- o .: "value" :: Parser [String]
  pure . F12 $ case (C.toLower <$> value) of
    "yes" -> True
    "no"  -> False


pInSmokeTest :: Value -> Parser YTField
pInSmokeTest = withObject "Object" $ \o -> do
  [value] <- o .: "value" :: Parser [String]
  pure . F13 $ case (C.toLower <$> value) of
    "yes" -> True
    "no"  -> False


pExecutionTime :: Value -> Parser YTField
pExecutionTime = withObject "Object" $ \o -> do
  [value] <- o .: "value" :: Parser [String]
  pure . F14 $ case value of
    num | all C.isDigit num -> pure (read num :: Int)
    _ -> Nothing


pPassedVersions :: Value -> Parser YTField
pPassedVersions = withObject "Passed Versions"
  $ \o -> fmap F15 $ o .: "value"


pFailedVersions :: Value -> Parser YTField
pFailedVersions = withObject "Failed Versions"
  $ \o -> fmap F16 $ o .: "value"


pBlockedVersions :: Value -> Parser YTField
pBlockedVersions = withObject "Blocked Versions"
  $ \o -> fmap F17 $ o .: "value"

pCoveredComponents :: Value -> Parser YTField
pCoveredComponents = withObject "Covered Components"
  $ \o -> fmap F18 $ o .: "value"


pTestResult :: Value -> Parser YTField
pTestResult = withObject "Test Result" $ \o -> do
  [value] <- o .: "value" :: Parser [String]
  pure . F19 . pure $ case value of
    "Passed" -> Passed
    "Failed"  -> Failed


pTargetOS :: Value -> Parser YTField
pTargetOS = withObject "Target OS" $ \o -> do
  values <- o .: "value" :: Parser [String]
  return $ F20 $ map decodeOS values
  where
  decodeOS value = case value of
    "Linux"         -> Linux
    "Windows"       -> Windows
    "All Platforms" -> AllPlatforms
    "MacOS"         -> MacOS
    "AnySingle"     -> AnySingle

pTestingType :: Value -> Parser YTField
pTestingType = withObject "Testing Type" $ \o -> do
  values <- o .: "value" :: Parser [String]
  return $ F21 $ map decode values
  where
  decode value = case value of
    "Integration Test" -> IntegrationTest
    "UI Test"          -> UITest
    "E2E Test"         -> E2ETest
    "API Test"         -> APITest
    "Component Test"   -> ComponentTest
    "UX Test"          -> UXTest
    "NFT"              -> NFT
    "Unit Test"        -> UnitTest


pBrowserAndVersion :: Value -> Parser YTField
pBrowserAndVersion  = withObject "Browser and Version" $ \o -> do
  values <- o .: "value"
  return $ F22 $ map toBwv values
  where
    toBwv :: Text -> BrowserVersion
    toBwv val = case val of
      "Google Chrome 68" -> GoogleChrome68
      "Opera 55"         -> Opera55
      "Google Chrome 69" -> GoogleChrome69

pLinks :: Value -> Parser YTField
pLinks = withObject "links" $ \o -> do
  values <- o .: "value" :: Parser [Value]
  links <- sequence $  toLink <$> values
  return $ F23 links
  where
    toLink :: Value -> Parser Link
    toLink = withObject "link" $ \obj -> do
      lValue <- obj .: "value"
      lType  <- obj .: "type"
      lRole  <- obj .: "role"
      return MkLink{..}


instance FromJSON YttpIssue where
  parseJSON = withObject "sdsdsd" $ \yttpIssue -> do
    issueId <- yttpIssue .: "id" :: Parser Text
    fields <- yttpIssue .: "field"  :: Parser [Value]
    fieldsetters <- fmap modifyYttpIssue <$> forM fields pYTField
    return $ (yttpiIssueId .~ issueId) $ foldr ($) dummyYttpIssue fieldsetters


modifyYttpIssue :: (Text, YTField) -> (YttpIssue -> YttpIssue)
modifyYttpIssue (name,field) = case (name, field) of
  ("Type"                  , F1  val )  -> yttpiType              .~ val
  ("summary"               , F2  val )  -> yttpiSummary           .~ val
  ("description"           , F3  val )  -> yttpiDescription       .~ val
  ("created"               , F4  val )  -> yttpiCreated           .~ val
  ("updated"               , F5  val )  -> yttpiUpdatedAt         .~ val
  ("projectShortName"      , F6  val )  -> yttpiProject           .~ val
  ("numberInProject"       , F7  val )  -> yttpiNumber            .~ val
  ("State"                 , F8  val )  -> yttpiState             .~ val
  ("Priority"              , F9  val )  -> yttpiPriority          .~ val
  ("Review status"         , F10 val )  -> yttpiReviewStatus      .~ val
  ("Automation status"     , F11 val )  -> yttpiAutomationStatus  .~ val
  ("In regression suite"   , F12 val )  -> yttpiInRegressionSuite .~ val
  ("In Smoke Test"         , F13 val )  -> yttpiInSmokeTest       .~ val
  ("Execution time (hours)", F14 val )  -> yttpiExecutionTime     .~ val
  ("Passed in Versions"    , F15 val )  -> yttpiPassedInVersions  .~ val
  ("Failed  in Versions"   , F16 val )  -> yttpiFailedInVersions  .~ val
  ("Blocked in Versions"   , F17 val )  -> yttpiBlockedInVersions .~ val
  ("Covered Components"    , F18 val )  -> yttpiCoveredComponents .~ val
  ("Test Result"           , F19 val )  -> yttpiTestResult        .~ val
  ("Target OS"             , F20 val )  -> yttpiTargetOS          .~ val
  ("Testing Type"          , F21 val )  -> yttpiTestingType       .~ val
  ("Browser and Version"   , F22 val )  -> yttpBrowserVersion     .~ val
  ("links"                 , F23 val )  -> yttpLinks              .~ val
  _                                     -> id

dummyYttpIssue :: YttpIssue
dummyYttpIssue = MkYttpIssue
  ""
  TestCase
  "Dummy ISSUE "
  ""
  (UTCTime (ModifiedJulianDay 0) 0)
  Nothing
  ""
  1
  Open
  High
  ReviewDone
  Automated
  False
  False
  Nothing
  []
  []
  []
  []
  Nothing
  []
  []
  []
  []

