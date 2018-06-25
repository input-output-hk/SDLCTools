{-# LANGUAGE RecordWildCards #-}

module Planning.Loader

where

--import            Control.Monad.Fail
import qualified  Data.List as L
import qualified  Data.Map.Strict as M
import            Data.Maybe (mapMaybe)
import qualified  Data.Text as T

import            Test.QuickCheck.Gen (Gen, shuffle, frequency, choose, vectorOf)


import            Gtsim.Types
import            Gtsim.Simulator as S
import            Meas.Extract.Types


iohkResources =
  [ MkResource "Squad 1"          (stdResourceEfficiency 2.5) (choose (1, 3))
  , MkResource "Squad 2"          (stdResourceEfficiency 1.0) (choose (1, 3))
  , MkResource "Squad 3"          (stdResourceEfficiency 2.0) (choose (1, 3))
  , MkResource "Squad 4"          (stdResourceEfficiency 2.5) (choose (1, 3))
  , MkResource "New Wallet Squad" (stdResourceEfficiency 4.5) (choose (1, 3))
  , MkResource "Core Squad"       (stdResourceEfficiency 5.0) (choose (1, 3))
  , MkResource "Network Squad"    (stdResourceEfficiency 2.5) (choose (1, 3))
  ]

iohkResourceMap = M.fromList $ map (\r -> (rId r, r)) iohkResources


resourceEfficiency :: [(Int, Float)] -> (Int -> ManDaysPerDay)
resourceEfficiency effs = go
  where
  go 0 = go 1
  go i = (M.fromList $ map (\(i, e) -> (i, toRational e)) effs) M.! i

stdResourceEfficiency :: Float -> (Int -> ManDaysPerDay)
stdResourceEfficiency baseEff =
  resourceEfficiency effs
  where
  effs = zipWith (\i f -> (i, let b = fromIntegral i * baseEff * (1+f) in 1.0 / b)) [1..] switchPercent
  switchPercent = [0, 0.1, 0.15, 0.20, 0.30, 0.45, 0.7, 1, 1.5, 2.0, 2.0, 3.0]


initialize :: [Resource] -> [YtIssue] -> (SimState, ProblemDefinition)
initialize resources ytIssues =
  S.initialize tasks resources dependencies runningAssignmentGen
  where
  tasks = mapMaybe mkTask ytIssues
  dependencies = M.fromList $ map (\t -> (tId t, [])) tasks
  runningAssignmentGen = frequency [(40, pure True), (60, pure False)]


mkTask :: YtIssue -> Maybe Task
mkTask (MkYtIssue {..}) = do
  romMandays <- _ytiROMManday
  let tId = T.unpack _ytiIssueId
  let tProjectId = T.unpack _ytiProject
  let tManDaysGen = romMandaysToGen romMandays
  let tCanBeDoneBy = map T.unpack _ytiPotentialSquad
  if L.null tCanBeDoneBy
    then fail ""
    else return $ MkTask {..}






{-


{-}
data ProblemDefinition = MkProblemDefinition
  { pdTasks                  :: M.Map TaskId Task         -- ^ All initial Tasks
  , pdResources              :: M.Map ResourceId Resource -- ^ All available Resources
  , pdTasksPerResource       :: [(ResourceId, TaskId)]    -- ^ (R, T): the resource R can work on task T
  , pdDependencies           :: M.Map TaskId [TaskId]     -- ^ The dependent Task (key) must start after all of the pre-requisites Tasks (values) are completed.
  , pdRunningAssignmentGen   :: Gen Bool                  -- ^ Generator of Running/Waiting flag. Corresponds to the WaitingAssignmentProb in the specs.
  }
-}

data Resource = MkResource
  { rId                       :: ResourceId           -- ^ Identifier of the Resource
  , rEfficiency               :: Int -> ManDaysPerDay -- ^ Efficiency wrt the number of concurrent running Assignment. rEfficiency 0 == rEfficiency 1
  , rNbActiveAssignmentsGen   :: Gen Int              -- ^ Generator for the number of concurrent Active Assignments for this Resource.
  }
data Task = MkTask
  { tId               :: TaskId         -- ^ Identifier of the Task
  , tProjectId        :: ProjectId      -- ^ Project the Task belongs to
  , tManDaysGen       :: Gen ManDays    -- ^ Generator of effort in man.days for this Task
  , tCanBeDoneBy      :: [ResourceId]   -- ^ Resources that can work on this Task
  }

data YtIssue = MkYtIssue
  { _ytiIssueId           :: T.Text
  , _ytiCreated           :: Int
  , _ytiProject           :: T.Text
  , _ytiNumber            :: Int
  , _ytiState             :: StateValue
  , _ytiWait              :: WaitValue
  , _ytiDueDate           :: Int
  , _ytiROMManday         :: Maybe ROMMandaysValue
  , _ytiPPriorities       :: (Int, Int, Int)
  , _ytiSquad             :: Maybe T.Text
  , _ytiOwner             :: Maybe T.Text
  , _ytiPotentialSquad    :: [T.Text]
  , _ytiTargetVersions    :: [T.Text]
  , _ytiLinks             :: [(LinkType, T.Text)]
  , _ytiChanges           :: [(Int, [ValueChange])]
  , _ytiStateTransitions  :: StateTransitions
  , _ytiBlockedDays       :: Integer
  , _ytiErrors            :: [String]
  }
  deriving (Show, Generic, NFData)-}


romMandaysToGen :: ROMMandaysValue -> Gen ManDays
romMandaysToGen Days      = choose (0.5 , 4.0::Float)    >>= (return . toRational)
romMandaysToGen Weeks     = choose (3.0 , 13.0::Float)   >>= (return . toRational)
romMandaysToGen Months    = choose (12.0, 45.0::Float)   >>= (return . toRational)
romMandaysToGen Quarters  = choose (40.0, 100.0::Float)  >>= (return . toRational)


