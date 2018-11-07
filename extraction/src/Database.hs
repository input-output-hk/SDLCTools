{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Database
where

import qualified  Data.Text as T
import            Database.PostgreSQL.Simple
import            Database.PostgreSQL.Simple.ToField
import            Database.PostgreSQL.Simple.ToRow

import Types


instance ToField TypeVal where
  toField  Review           = toField ("Review"::String)
  toField  DocumentRequest  = toField ("Document Request"::String)
  toField  Risk             = toField ("Risk"::String)
  toField  Action           = toField ("Action"::String)
  toField  Issue            = toField ("Issue"::String)
  toField  TestCase         = toField ("Test Case"::String)
  toField  Decision         = toField ("Decision"::String)


instance ToField State where
  toField v = toField (show v)

instance ToField Priority where
  toField v = toField (show v)

instance ToField ReviewStatus where
  toField Wip           = toField ("WIP"::String)
  toField ToBeReviewed  = toField ("To be reviewed"::String)
  toField ReviewDone    = toField ("Review done"::String)
  toField NeedsUpdate   = toField ("Needs update"::String)
  toField ToBeDeleted   = toField ("To be deleted"::String)


instance ToField AutomationStatus where
  toField Manual        = toField ("Manual"::String)
  toField Automated     = toField ("Automated"::String)
  toField ToBeAutomated = toField ("To be automated"::String)

instance ToField TestResult where
  toField v = toField (show v)

instance ToField TargetOS where
  toField Linux         = toField ("Linux"::String)
  toField Windows       = toField ("Windows"::String)
  toField AllPlatforms  = toField ("All Platforms"::String)
  toField MacOS         = toField ("MacOS"::String)
  toField AnySingle     = toField ("AnySingle"::String)

instance ToField TestingType where
  toField IntegrationTest   = toField ("Integration Test"::String)
  toField UITest            = toField ("UI Test"::String)
  toField E2ETest           = toField ("E2E Test"::String)
  toField APITest           = toField ("API Test"::String)
  toField ComponentTest     = toField ("Component Test"::String)
  toField UXTest            = toField ("UX Test"::String)
  toField NFT               = toField ("NFT"::String)


instance ToField BrowserVersion where
  toField GoogleChrome68  = toField ("Google Chrome 68"::String)
  toField Opera55         = toField ("Opera 55"::String)
  toField GoogleChrome69  = toField ("Google Chrome 69"::String)



-- REM : strange these instances should be imported from Database.PostgreSQL.Simple.ToRow ... But does not work

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f,
          ToField g, ToField h, ToField i, ToField j, ToField k)
    => ToRow (a,b,c,d,e,f,g,h,i,j,k) where
    toRow (a,b,c,d,e,f,g,h,i,j,k) =
        [toField a, toField b, toField c, toField d, toField e, toField f,
         toField g, toField h, toField i, toField j, toField k]

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f,
          ToField g, ToField h, ToField i, ToField j, ToField k, ToField l,
          ToField m, ToField n, ToField o, ToField p)
    => ToRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) where
    toRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) =
        [toField a, toField b, toField c, toField d, toField e, toField f,
         toField g, toField h, toField i, toField j, toField k, toField l,
         toField m, toField n, toField o, toField p]

saveTestProjectIssue :: Connection -> YttpIssue -> IO ()
saveTestProjectIssue conn issue@MkYttpIssue{..} = do
  let q =
        ( _yttpiIssueId
        , _yttpiType
        , _yttpiSummary
        , _yttpiDescription
        , _yttpiCreated
        , _yttpiUpdatedAt
        , _yttpiProject
        , _yttpiNumber
        , _yttpiState
        , _yttpiPriority
        , _yttpiReviewStatus
        , _yttpiAutomationStatus
        , _yttpiInRegressionSuite
        , _yttpiInSmokeTest
        , _yttpiExecutionTime
        , _yttpiTestResult
        )
  execute conn stmt q

  saveTargetOs conn issue
  saveTestingType conn issue
  savePassedInVersions conn issue
  saveFailedInVersions conn issue
  saveBlockedInVersions conn issue
  saveCoveredComponents conn issue
  saveBrowserVersion conn issue

  return ()

  where
  stmt = "insert into yttpIssueDetails (\
          \ yttpiIssueId, yttpiType, yttpiSummary, yttpiDescription, yttpiCreated, \
          \ yttpiUpdatedAt, yttpiProject, yttpiNumber, yttpiState, yttpiPriority,  \
          \ yttpiReviewStatus, yttpiAutomationStatus, yttpiInRegressionSuite,      \
          \ yttpiInSmokeTest, yttpiExecutiontime, yttpiTestResult) values          \
          \ (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"


saveTargetOs :: Connection -> YttpIssue -> IO ()
saveTargetOs conn MkYttpIssue{..} = do
  mapM_ saveOne _yttpiTargetOS
  where
  saveOne targetOS = do
    let q = ( _yttpiIssueId, targetOS)
    execute conn stmt q
  stmt = "insert into yttpTargetOS (yttpiIssueId, yttpTargetOS) values (?, ?)"


saveTestingType :: Connection -> YttpIssue -> IO ()
saveTestingType conn MkYttpIssue{..} = do
  mapM_ saveOne _yttpiTestingType
  where
  saveOne testingType = do
    let q = ( _yttpiIssueId, testingType)
    execute conn stmt q
  stmt = "insert into yttpTestingType (yttpiIssueId, yttpTestingType) values (?, ?)"


savePassedInVersions :: Connection -> YttpIssue -> IO ()
savePassedInVersions conn MkYttpIssue{..} = do
  mapM_ saveOne _yttpiPassedInVersions
  where
  saveOne version = do
    let q = ( _yttpiIssueId, version)
    execute conn stmt q
  stmt = "insert into yttpPassedVersions (yttpiIssueId, yttpVersion) values (?, ?)"


saveFailedInVersions :: Connection -> YttpIssue -> IO ()
saveFailedInVersions conn MkYttpIssue{..} = do
  mapM_ saveOne _yttpiFailedInVersions
  where
  saveOne version = do
    let q = ( _yttpiIssueId, version)
    execute conn stmt q
  stmt = "insert into yttpFailedVersions (yttpiIssueId, yttpVersion) values (?, ?)"


saveBlockedInVersions :: Connection -> YttpIssue -> IO ()
saveBlockedInVersions conn MkYttpIssue{..} = do
  mapM_ saveOne _yttpiBlockedInVersions
  where
  saveOne version = do
    let q = ( _yttpiIssueId, version)
    execute conn stmt q
  stmt = "insert into yttpBlockedVersions (yttpiIssueId, yttpVersion) values (?, ?)"


saveCoveredComponents :: Connection -> YttpIssue -> IO ()
saveCoveredComponents conn MkYttpIssue{..} = do
  mapM_ saveOne _yttpiCoveredComponents
  where
  saveOne component = do
    let q = ( _yttpiIssueId, component)
    execute conn stmt q
  stmt = "insert into yttpCoveredComponents (yttpiIssueId, yttpComponent) values (?, ?)"


saveBrowserVersion :: Connection -> YttpIssue -> IO ()
saveBrowserVersion conn MkYttpIssue{..} = do
  mapM_ saveOne _ytttpBrowserVersion
  where
  saveOne version = do
    let q = ( _yttpiIssueId, version)
    execute conn stmt q
  stmt = "insert into yttpBrowserAndVersions (yttpiIssueId, yttpBrowserVersionVal) values (?, ?)"

