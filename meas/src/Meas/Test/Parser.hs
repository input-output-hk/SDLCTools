{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Meas.Test.Parser
(
)
where

--import Debug.Trace (trace)

import            Control.Monad
import            Control.Lens
import            Data.Aeson
import            Data.Aeson.Types
import qualified  Data.Char as C
import            Data.Text (Text, unpack)

import            Meas.Misc
import            Meas.Test.Lens
import            Meas.Test.Types



pYTField :: Value -> Parser (Text, YTField)
pYTField o@(Object oo) = do
    name <- oo .: "name" :: Parser Text
    (name,) <$> case name of
        "Type"                    -> pType o
        "summary"                 -> pSummary o
        "description"             -> pDescription o
        "created"                 -> pCreated o
        "updated"                 -> pUpdated o
        "projectShortName"        -> pProject o
        "numberInProject"         -> pNumber o
        "State"                   -> pState o
        "Priority"                -> pPriority o
        "Review status"           -> pReviewStatus o

        "Automation status"       -> pAutomationStatus o
        "In regression suite"     -> pInRegressionSuite o
        "In Smoke Test"           -> pInSmokeTest o

        "Execution time (hours)"  -> pExecutionTime o
        "Passed in Versions"      -> pPassedVersions o
        "Failed  in Versions"     -> pFailedVersions o

        "Blocked in Versions"     -> pBlockedVersions o
        "Covered Components"      -> pCoveredComponents o
        "Test Result"             -> pTestResult o
        "Target OS"               -> pTargetOS o
        "Testing Type"            -> pTestingType o
        "Browser and Version"     -> pBrowserAndVersion o
        "links"                   -> pLinks o
        _                         -> pure NotRequired

pYTField _ = error "Cannot parse, expecting object"

pType :: Value -> Parser YTField
pType = withObject "Object" $ \o -> do
  [value] <- o .: "value" :: Parser [String]
  pure . F1 $ case value of
    "Review"            -> Review
    "Document Request"  -> DocumentRequest
    "Risk"              -> Risk
    "Action"            -> Action
    "Issue"             -> Issue
    "Test Case"         -> TestCase
    "Decision"          -> Decision
    s                   -> error ("Cannot Parse, unknown Type field: "++ s)


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
  [value] <- o .: "value" :: Parser [String]
  pure . F8 $ case value of
    "Open"      -> Open
    "Obsolete"  -> Obsolete
    "Verified"  -> Verified
    "Blocking"  -> Blocking
    "Done"      -> Done
    "Selected"  -> Selected
    s           -> error ("Cannot Parse, unknown State field: "++ s)

pPriority :: Value -> Parser YTField
pPriority = withObject "Object" $ \o -> do
  [value] <- o .: "value" :: Parser [String]
  pure . F9 $ case value of
    "ShowStopper" -> ShowStopper
    "High"        -> High
    "Major"       -> Major
    "Medium"      -> Medium
    "Low"         -> Low
    "Normal"      -> Normal
    "Critical"    -> Critical
    "Minor"       -> Minor
    s             -> error ("Cannot Parse, unknown Priority field: "++ s)

pReviewStatus :: Value -> Parser YTField
pReviewStatus = withObject "Object" $ \o -> do
  [value] <- o .: "value" :: Parser [String]
  pure . F10 $ case value of
    "WIP"             -> Wip
    "To be reviewed"  -> ToBeReviewed
    "Review done"     -> ReviewDone
    "Needs update"    -> NeedsUpdate
    "To be deleted"   -> ToBeDeleted
    s                 -> error ("Cannot Parse, unknown Review Status field: "++ s)


pAutomationStatus :: Value -> Parser YTField
pAutomationStatus = withObject "Object" $ \o -> do
  [value] <- o .: "value" :: Parser [String]
  pure . F11 $ case value of
    "Manual"            -> Manual
    "Automated"         -> Automated
    "To be automated"   -> ToBeAutomated
    s                   -> error ("Cannot Parse, unknown Automation Status field: "++ s)

pInRegressionSuite :: Value -> Parser YTField
pInRegressionSuite = withObject "Object" $ \o -> do
  [value] <- o .: "value" :: Parser [String]
  pure . F12 $ case (C.toLower <$> value) of
    "yes" -> True
    "no"  -> False
    s     -> error ("Cannot Parse, unknown Regression field: "++ s)


pInSmokeTest :: Value -> Parser YTField
pInSmokeTest = withObject "Object" $ \o -> do
  [value] <- o .: "value" :: Parser [String]
  pure . F13 $ case (C.toLower <$> value) of
    "yes" -> True
    "no"  -> False
    s     -> error ("Cannot Parse, unknown Smoke Test field: "++ s)


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
    "Passed"  -> Passed
    "Failed"  -> Failed
    s         -> error ("Cannot Parse, unknown Test Result field: "++ s)


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
    s               -> error ("Cannot Parse, unknown Target OS field: "++ s)

pTestingType :: Value -> Parser YTField
pTestingType = withObject "Testing Type" $ \o -> do
  values <- o .: "value" :: Parser [String]
  return $ F21 $ map decodeTestingType values
  where
  decodeTestingType value = case value of
    "Integration Test"  -> IntegrationTest
    "UI Test"           -> UITest
    "E2E Test"          -> E2ETest
    "API Test"          -> APITest
    "Component Test"    -> ComponentTest
    "UX Test"           -> UXTest
    "NFT"               -> NFT
    "Unit Test"         -> UnitTest
    s                   -> error ("Cannot Parse, unknown Testing Type field: "++ s)


pBrowserAndVersion :: Value -> Parser YTField
pBrowserAndVersion  = withObject "Browser and Version" $ \o -> do
  values <- o .: "value"
  return $ F22 $ map toBwv values
  where
    toBwv :: Text -> BrowserVersion
    toBwv val = case val of
      "Google Chrome 68"  -> GoogleChrome68
      "Opera 55"          -> Opera55
      "Google Chrome 69"  -> GoogleChrome69
      s                   -> error ("Cannot Parse, unknown Browser and Version field: "++ unpack s)

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
    return $ (yttpiIssueId .~ issueId) $ foldr ($) defaultYttpIssue fieldsetters


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


