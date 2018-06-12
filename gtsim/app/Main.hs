{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}


module Main where

--import Lib

import Data.Ratio

import Data.Decimal
import Debug.Trace(trace)
import Control.Monad.State
import Control.Monad
import Data.Monoid

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.List as L




import Gtsim.Types
{-
tests = [
   testProperty "testRangeProp" testRangeProp
    ]


-}

main :: IO ()
main = do
 -- quickCheck testInvariantsOnly
 -- quickCheck testOneResource
 -- quickCheck testMultipleResources
 -- quickCheck testMultipleResourcesWithParallism
 -- quickCheck testIndependentProjects
  return ()



{-}

data ProblemDefinition = MkProblemDefinition
  { pdTasks                  :: M.Map TaskId Task
  , pdResources              :: M.Map ResourceId Resource
  , pdTasksPerResource       :: [(ResourceId, TaskId)]    -- ^ for each resource, the compatible tasks
  , pdDependencies           :: M.Map TaskId [TaskId]
  , pdRunningAssignmentGen   :: Gen Bool
  }

  -}
--initialize :: [Task] -> [Resource] -> M.Map TaskId [TaskId] -> (SimState, ProblemDefinition)




