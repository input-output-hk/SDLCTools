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

-- QUESTION: What is the purpose of 'aaRunning'?
--
data ActiveAssignment = MkActiveAssignment
  { aaTaskId            :: TaskId           -- ^ Task Id
  , aaResourceId        :: ResourceId       -- ^ The Resource which is working on that Task
  , aaManDays           :: ManDays          -- ^ Effort to do the task, in man.day.
  , aaRemainingMandays  :: ManDays          -- ^ Current remaining effort to complete the task, in man.day.
  , aaRunning           :: Bool             -- ^ Is this assignment running (True = yes)
  , aaStartTime         :: Day              -- ^ Day at which the resource started to work on that task
  }
  deriving Show

-- QUESTION/TODO: It looks like 'pdTasksPerResource' and the
-- 'tCanBeDoneBy' fields in the 'Task' datatype contain the
-- same information in two different places. Why?
--
-- REMARK: The 'pdRunningAssignmentsGen' corresponds to the
-- 'WaitingAssignmentProb' mentioned in the spec as a global
-- parameter.
--
data ProblemDefinition = MkProblemDefinition
  { pdTasks                  :: M.Map TaskId Task         -- ^ All initial Tasks
  , pdResources              :: M.Map ResourceId Resource -- ^ All available Resources
  , pdTasksPerResource       :: [(ResourceId, TaskId)]    -- ^ (R, T): the resource R can work on task T
  , pdDependencies           :: M.Map TaskId [TaskId]     -- ^ The dependent Task (key) must start after all of the pre-requisites Tasks (values) are completed.
  , pdRunningAssignmentGen   :: Gen Bool                  -- ^ Generator of Running/Waiting flag
  }

-- TODO: Needs documentation as to why the tasks are not
-- included here.
--
data SimState = MkSimState
  { sstCurrentTime            :: Day                      -- ^ Reference Time (Tc in the specs)
  , sstActiveAssignments      :: [ActiveAssignment]       -- ^ Active Assignements
  , sstCompletedAssignments   :: [CompletedAssignment]    -- ^ Completed Assignements
  }
  deriving Show

-- QUESTION: Why include potential resources directly in the task,
-- but not task dependencies?
--
data Task = MkTask
  { tId               :: TaskId         -- ^ Identifier of the Task
  , tProjectId        :: ProjectId      -- ^ Project the Task belongs to
  , tManDaysGen       :: Gen ManDays    -- ^ Generator of effort in man.days for this Task
  , tCanBeDoneBy      :: [ResourceId]   -- ^ Resources that can work on this Task
  }

-- TODO: If you want a domain-specific way to show a particular
-- entity, then do not use the 'Show' class and keep the 'Show'
-- class for showing things in such a way that the resulting
-- values could be used in a Haskell program again.
--
instance Show Task where
  show (MkTask{..}) = tId

data Resource = MkResource
  { rId                       :: ResourceId           -- ^ Identifier of the Resource
  , rEfficiency               :: Int -> ManDaysPerDay -- ^ Efficiency wrt the number of concurrent running Assignment. rEfficiency 0 == rEfficiency 1
  , rNbActiveAssignmentsGen   :: Gen Int              -- ^ Generator for the number of concurrent Active Assignments for this Resource.
  }

instance Show Resource where
  show (MkResource{..}) = rId


-- TODO: This should be a newtype.
type ProjectId = String

-- TODO: This should be a newtype.
type TaskId = String

-- TODO: This should be a newtype.
type ManDays = Rational

-- TODO: This should be a newtype.
type ResourceId = String

-- TODO: This should be a newtype.
type ManDaysPerDay = Rational

-- TODO: This should be a newtype.
--
-- This also needs documentation of what it actually means.
-- I assume this is a relative time counter that starts at
-- 0 at the beginning of the simulation.
--
type Day = Rational
