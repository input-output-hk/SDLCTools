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
-- J-C That is a temporary information which indicates whether this Assignment is actively
-- being worked on during the current step.
-- It represents the Wait field in YT (see the Soft. Dev. Process doc)
-- Given your remark for the 'selectRunningAssignments' function, it could be removed.
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
-- J-C : yes that is true. As far as the algo is concerned, we can remove 'tCanBeDoneBy'
-- and dircetly pass a '[(ResourceId, TaskId)]' in 'initialize'.
--
-- REMARK: The 'pdRunningAssignmentsGen' corresponds to the
-- 'WaitingAssignmentProb' mentioned in the spec as a global
-- parameter.
-- J-C : yes, comment upated
data ProblemDefinition = MkProblemDefinition
  { pdTasks                  :: M.Map TaskId Task         -- ^ All initial Tasks
  , pdResources              :: M.Map ResourceId Resource -- ^ All available Resources
  , pdTasksPerResource       :: [(ResourceId, TaskId)]    -- ^ (R, T): the resource R can work on task T
  , pdDependencies           :: M.Map TaskId [TaskId]     -- ^ The dependent Task (key) must start after all of the pre-requisites Tasks (values) are completed.
  , pdRunningAssignmentGen   :: Gen Bool                  -- ^ Generator of Running/Waiting flag. Corresponds to the WaitingAssignmentProb in the specs.
  }

-- TODO: Needs documentation as to why the tasks are not
-- included here.
-- J-C : Tasks are part of the problem definition.
data SimState = MkSimState
  { sstCurrentTime            :: Day                      -- ^ Reference Time (Tc in the specs)
  , sstActiveAssignments      :: [ActiveAssignment]       -- ^ Active Assignements
  , sstCompletedAssignments   :: [CompletedAssignment]    -- ^ Completed Assignements
  }
  deriving Show

-- QUESTION: Why include potential resources directly in the task,
-- but not task dependencies?
-- J-C : potential resources directly in the task : see my answer 'tCanBeDoneBy' (above).
-- dependencies in Task: it could. That is a matter of personal taste and I always try to avoid hierachical data structures.
-- as they are harder to query. But no strong point against it, though.
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
-- J-C:what do you suggest? a Task specific show function such as `showTask`?
instance Show Task where
  show (MkTask{..}) = tId

data Resource = MkResource
  { rId                       :: ResourceId           -- ^ Identifier of the Resource
  , rEfficiency               :: Int -> ManDaysPerDay -- ^ Efficiency wrt the number of concurrent running Assignment. rEfficiency 0 == rEfficiency 1
  , rNbActiveAssignmentsGen   :: Gen Int              -- ^ Generator for the number of concurrent Active Assignments for this Resource.
  }

instance Show Resource where
  show (MkResource{..}) = rId


-- J-C : Type synonym and not newtype.
-- In this kind of problems, when do you use type synomyms and when do you use newtype?
type InvariantResult = (Bool, String)

-- TODO: This should be a newtype.
-- J-C: I often feel newtypes make the code more complex.
-- But, no strong objection here.
-- Deepak can do it
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
-- J-C Yes: The simulation starts at Tc=0 (see the specs)
-- It could have called `Time`if you prefer.
type Day = Rational
