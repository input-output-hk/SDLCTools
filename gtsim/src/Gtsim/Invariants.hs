{-# LANGUAGE RecordWildCards #-}

module Gtsim.Invariants

where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Gtsim.Types


-- Invariants
completedAssignmentInv :: CompletedAssignment -> (Bool, String)
completedAssignmentInv MkCompletedAssignment{..} =
  (caStartTime <= caEndTime && caManDays > 0, "CA: Invalid Elem")


completedAssignmentsInv :: [CompletedAssignment] -> [(Bool, String)]
completedAssignmentsInv elems =
  b1 : b2
  where
  -- tasks are unique
  b1 = ((S.size $ S.fromList $ L.map caTaskId elems) == L.length elems, "CA: Unique tasks")
  -- gantt chart elem inv
  b2 =  L.map completedAssignmentInv elems

activeAssignmentInv :: ActiveAssignment -> (Bool, String)
activeAssignmentInv MkActiveAssignment{..} =
  (aaRemainingMandays <= aaManDays, "AA: Invalid Elem")


activeAssignmentsInv :: [ActiveAssignment] -> [(Bool, String)]
activeAssignmentsInv elems =
  b1 : b2
  where
  -- tasks are unique
  b1 = ((S.size $ S.fromList $ L.map aaTaskId elems) == L.length elems, "AA: Unique tasks")
  -- gantt chart elem inv
  b2 =  L.map activeAssignmentInv elems



simStateInv :: SimState -> [(Bool, String)]
simStateInv MkSimState{..} =
  b3:b4:(b1 ++ b2)
  where
  -- gantt chart inv for active tasks
  b1 = activeAssignmentsInv sstActiveAssignments
  -- gantt chart inv for done tasks
  b2 = completedAssignmentsInv sstCompletedAssignments
  --  The same task can be active and done.
  b3 = let
    doneTaskIds = L.map caTaskId sstCompletedAssignments
    activeTaskIds = L.map aaTaskId sstActiveAssignments
    in
    (L.null $ L.intersect doneTaskIds activeTaskIds, "Task is done xor active")
  b4 = let
    endTimes = L.map caEndTime sstCompletedAssignments
    in
    (L.sort endTimes == L.reverse endTimes, "Done tasks are ordered by end time desc")

-- compatible resources work on tasks
compatibleResourceInv :: ProblemDefinition -> SimState -> (Bool, [Char])
compatibleResourceInv MkProblemDefinition{..} MkSimState{..} =
  (L.all go sstCompletedAssignments, "Incompatible resource works on task")
  where
  go MkCompletedAssignment{..} = L.elem (caResourceId, caTaskId) pdTasksPerResource

finalSimStateAdditionalInv :: ProblemDefinition -> SimState -> [(Bool, [Char])]
finalSimStateAdditionalInv problemDefinition simState@MkSimState{..} =
  [b1, b2, b3]
  where
  b1 = (L.null sstActiveAssignments, "No Active Tasks")

  -- all task dependencies are respected
  b2 = dependenciesRespectedInv problemDefinition simState

  -- only compatible resources work on tasks
  b3 = compatibleResourceInv problemDefinition simState


dependenciesRespectedInv :: ProblemDefinition -> SimState -> (Bool, [Char])
dependenciesRespectedInv problemDefinition simState =
  (L.all id rightOrder, "Dependencies not respected")
  where
  rightOrder  = do
    (dependentTaskId, prTaskIds) <- M.toList (pdDependencies problemDefinition)
    preRequisiteTaskId <- prTaskIds
    let depCa = assMap M.! dependentTaskId
    let prCa = assMap M.! preRequisiteTaskId
    return (caEndTime prCa <= caStartTime depCa)
  assMap = M.fromList $ L.map (\ca -> (caTaskId ca, ca)) $ sstCompletedAssignments simState

