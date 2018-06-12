{-# LANGUAGE RecordWildCards #-}

module Gtsim.Types

where

import qualified Data.Map.Strict as M
import Test.QuickCheck.Gen (Gen)


data CompletedAssignment = MkCompletedAssignment
  { caTaskId        :: TaskId           -- ^ Task Id
  , caResourceId    :: ResourceId       -- ^ The Resource which worked on that Task
  , caManDays       :: ManDays          -- ^ effort to do the task, in man.day.
  , caStartTime     :: Day              -- ^ Day at which the resource started to work on that task
  , caEndTime       :: Day              -- ^ Day at which the resource completed that task
  }
  deriving Show


data ActiveAssignment = MkActiveAssignment
  { aaTaskId            :: TaskId           -- ^ Task Id
  , aaResourceId        :: ResourceId       -- ^ The Resource which is working on that Task
  , aaManDays           :: ManDays          -- ^ Effort to do the task, in man.day.
  , aaRemainingMandays  :: ManDays          -- ^ Current remaining effort to complete the task, in man.day.
  , aaRunning           :: Bool             -- ^ Is this assignment running (True = yes)
  , aaStartTime         :: Day              -- ^ Day at which the resource started to work on that task
  }
  deriving Show

data ProblemDefinition = MkProblemDefinition
  { pdTasks                  :: M.Map TaskId Task         -- ^ All initial Tasks
  , pdResources              :: M.Map ResourceId Resource -- ^ All available Resources
  , pdTasksPerResource       :: [(ResourceId, TaskId)]    -- ^ (R, T): the resource R can work on task T
  , pdDependencies           :: M.Map TaskId [TaskId]     -- ^ The dependent Task (key) must start after all of the pre-requisites Tasks (values) are completed.
  , pdRunningAssignmentGen   :: Gen Bool                  -- ^ Generator of Running/Waiting flag
  }


data SimState = MkSimState
  { sstCurrentTime            :: Day                      -- ^ Reference Time (Tc in the specs)
  , sstActiveAssignments      :: [ActiveAssignment]       -- ^ Active Assignements
  , sstCompletedAssignments   :: [CompletedAssignment]    -- ^ Completed Assignements
  }
  deriving Show

data Task = MkTask
  { tId               :: TaskId         -- ^ Identifier of the Task
  , tProjectId        :: ProjectId      -- ^ Project the Task belongs to
  , tManDaysGen       :: Gen ManDays    -- ^ Generator of effort in man.days for this Task
  , tCanBeDoneBy      :: [ResourceId]   -- ^ Resources that can work on this Task
  }

instance Show Task where
  show (MkTask{..}) = tId

data Resource = MkResource
  { rId                       :: ResourceId           -- ^ Identifier of the Resource
  , rEfficiency               :: Int -> ManDaysPerDay -- ^ Efficiency wrt the number of concurrent running Assignment. rEfficiency 0 == rEfficiency 1
  , rNbActiveAssignmentsGen   :: Gen Int              -- ^ Generator for the number of concurrent Active Assignments for this Resource.
  }

instance Show Resource where
  show (MkResource{..}) = rId


type ProjectId = String

type TaskId = String
type ManDays = Rational

type ResourceId = String
type ManDaysPerDay = Rational

type Day = Rational
