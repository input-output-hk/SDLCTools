{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

module Meas.Test.Types
(
--  getAllTestIssues
--  , allTestIssuesForProjectJson

  AutomationStatus (..)
  , BrowserVersion (..)
  , Link (..)
  , Priority (..)
  , ReviewStatus (..)
  , TargetOS (..)
  , TestingType (..)
  , TestResult (..)
  , TypeVal (..)
  , State (..)
  , YttpIssue (..)

  , YTField (..)

  , defaultYttpIssue
)
where

--import Debug.Trace (trace)


import            Data.Time.Calendar
import            Data.Time.Clock
import            Data.Text (Text)
import            GHC.Generics


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

defaultYttpIssue :: YttpIssue
defaultYttpIssue = MkYttpIssue
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

