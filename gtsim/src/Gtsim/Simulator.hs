{-# LANGUAGE RecordWildCards #-}


module Gtsim.Simulator

where

--import Debug.Trace (trace)

import            Control.Monad
import qualified  Data.List as L
import qualified  Data.Map.Strict as M
import qualified  Data.Set as S
import            Test.QuickCheck (Gen, shuffle)

import Gtsim.Types


runSimulation :: (SimState -> [(Bool, [Char])])
                 -> (ProblemDefinition -> SimState -> [(Bool, [Char])])
                 -> ProblemDefinition
                 -> Day
                 -> SimState
                 -> Gen (Either [[Char]] SimState)
runSimulation inv finalInv problemDefinition deltaTime simState  = do
  go simState
  where
  go simState0 = do
    let nextTime = sstCurrentTime simState0 + deltaTime

    simState1 <- chooseNewActiveAssignments problemDefinition simState0
    simState2 <- selectRunningAssignments problemDefinition simState1
    simState3 <- update simState2 nextTime

    checkInv (inv simState3) $ do
      if L.null (sstActiveAssignments simState0) && L.null (sstActiveAssignments simState3)
        then return $ Left  ["Incorrect problem statement"]
        else case L.length (sstCompletedAssignments simState3) == M.size (pdTasks problemDefinition) of
                True -> checkInv (finalInv problemDefinition simState3) (return $ Right simState3)
                False -> go simState3

  update simState0' nextTime = do
    (simState1', endTimes) <- updateActiveAssignments problemDefinition nextTime simState0'
    let uniqueEndTimes = S.toList $ S.fromList endTimes
    case uniqueEndTimes of
      [] -> do
        -- no task completed during this step, we are done with the update
        return $ simState1' {sstCurrentTime = nextTime}
      _ -> do
        -- at least one task completed
        -- by construction, all uniqueEndTimes < nextTime
        -- we went too far in time, restart with nextTime = smallest uniqueEndTimes
        let minNextTime = L.minimum uniqueEndTimes
        (simState1'', _) <- updateActiveAssignments problemDefinition minNextTime simState0'
        return $ simState1'' {sstCurrentTime = minNextTime}

  checkInv invR k = do
    case L.all fst invR of
      True -> k
      False -> do
        let msgs = L.map snd $ L.filter (not . fst) invR
        return $ Left msgs



{-
Step 2
-}
chooseNewActiveAssignments :: ProblemDefinition -> SimState  -> Gen SimState
chooseNewActiveAssignments problemDefinition simState = do
  resources <- shuffle $ M.elems (pdResources problemDefinition)
  foldM (chooseNewActiveAssignmentsForResource problemDefinition) simState resources

chooseNewActiveAssignmentsForResource :: ProblemDefinition -> SimState -> Resource -> Gen SimState
chooseNewActiveAssignmentsForResource MkProblemDefinition{..} simState@MkSimState{..} MkResource{..} = do
  let activeAssignments = getActiveAssignmentByResource rId sstActiveAssignments
  let naa = L.length activeAssignments
  nca <- rNbActiveAssignmentsGen

  case nca > naa of
    True -> do
      -- Yes, we can choose (nca - naa) new tasks amongst the available ones
      -- which are compatible with the resource.
      availableTaskIds <- shuffle $ getAvailableTaskIds sstCompletedAssignments sstActiveAssignments pdDependencies
      let compatibleTaskIds = selectCompatibleTasksForResource pdTasksPerResource availableTaskIds rId
      -- then we select a subset of these taskIds and create new Active Assignments.
      newActiveAssignments <- mapM (\tId -> do
            mandays <- tManDaysGen (pdTasks M.! tId)
            return $ MkActiveAssignment tId rId mandays mandays True sstCurrentTime
            ) (L.take (nca - naa) compatibleTaskIds)

      -- update the state with these new Active Assignments.
      -- and we are done with this resource
      return $ simState {sstActiveAssignments = newActiveAssignments ++ sstActiveAssignments }
    False -> return simState  -- we can't add new Active Assignments
  where
  selectCompatibleTasksForResource taskPerResource taskIds resourceId =
    L.filter (\tId -> L.elem (resourceId, tId) taskPerResource) taskIds

{-
Step 3
-}
selectRunningAssignments :: ProblemDefinition -> SimState -> Gen SimState
selectRunningAssignments MkProblemDefinition{..}  simState@MkSimState{..} = do
  activeAssignments <- mapM go sstActiveAssignments
  return $ simState {sstActiveAssignments = activeAssignments}
  where
  go aa@MkActiveAssignment{..} = do
    running <- pdRunningAssignmentGen
    return $ aa {aaRunning = running}


{-
Step 4
-}
updateActiveAssignments :: ProblemDefinition -> Day -> SimState -> Gen (SimState, [Day])
updateActiveAssignments problemDefinition nextTime simState@MkSimState{..} = do
  foldM (updateActiveAssignmentsForResource problemDefinition nextTime) (simState, []) $ M.elems (pdResources problemDefinition)


updateActiveAssignmentsForResource :: ProblemDefinition -> Day -> (SimState, [Day]) -> Resource -> Gen (SimState, [Day])
updateActiveAssignmentsForResource MkProblemDefinition{..} nextTime (simState@MkSimState{..}, endTimes) MkResource{..} = do
  let (activeAssignments, others) = partitionActiveAssignmentsByResource rId sstActiveAssignments

  let activeAssignmentsToWorkOn = L.filter aaRunning activeAssignments
  -- determine the efficiency
  let eff = rEfficiency $ L.length activeAssignmentsToWorkOn

  -- update the remaining mandays, possibly completing tasks
  -- verify strictness here
  let (newActiveAssignments, newCompletedAssignments, endTimes') = L.foldl (updateProc eff nextTime) (others, sstCompletedAssignments, endTimes) activeAssignments

  -- Update the states with these new Active/Completed Assignments
  return $ (simState {sstActiveAssignments = newActiveAssignments, sstCompletedAssignments = newCompletedAssignments}, endTimes')
  where
  updateProc eff nextTime' (aas, cas, endTimes') activeAssignment@MkActiveAssignment{..} =
    case remaingMandays' <= 0 of
    True -> let
      -- Assignment is complete.
      -- We do not retain it (in aas) but we create and save a new Complete Assignment (in cas).
      -- We also register the end time for that Assignment.
      te = sstCurrentTime +  aaRemainingMandays / eff
      newCompletedAssignment = MkCompletedAssignment aaTaskId aaResourceId aaManDays aaStartTime te
      in (aas, newCompletedAssignment : cas, te : endTimes')
    False -> let
      -- Not finished yet, we update it and keep it in aas.
      newActiveAssignment = activeAssignment {aaRemainingMandays = remaingMandays'}
      in (newActiveAssignment : aas, cas, endTimes')
    where
    remaingMandays' = if aaRunning then aaRemainingMandays - eff * (nextTime' - sstCurrentTime) else aaRemainingMandays


getAvailableTaskIds :: [CompletedAssignment] -> [ActiveAssignment] -> M.Map TaskId [TaskId] -> [TaskId]
getAvailableTaskIds completedAssignments activeAssignments dependencies =
  availableTaskIds
  where
  completedTaskIds = map caTaskId completedAssignments
  activeTaskIds = map aaTaskId activeAssignments
  allAvailableTaskIds = possibleTasks completedTaskIds dependencies
  -- make this better
  -- Make sure we don't return already completed/active tasks.
  availableTaskIds = (allAvailableTaskIds L.\\ completedTaskIds) L.\\ activeTaskIds


partitionActiveAssignmentsByResource :: ResourceId
                                        -> [ActiveAssignment]
                                        -> ([ActiveAssignment], [ActiveAssignment])
partitionActiveAssignmentsByResource resourceId elems =
  L.partition (\aa -> aaResourceId aa == resourceId) elems

getActiveAssignmentByResource :: ResourceId  -> [ActiveAssignment] -> [ActiveAssignment]
getActiveAssignmentByResource resourceId elems =
  L.filter (\aa -> aaResourceId aa == resourceId) elems


initialize :: [Task] -> [Resource] -> M.Map TaskId [TaskId] -> Gen Bool -> (SimState, ProblemDefinition)
initialize tasks resources dependencies runningAssignmentGen =
  ( MkSimState 0 [] []
  , MkProblemDefinition pdTasks pdResources pdTasksPerResource dependencies runningAssignmentGen
  )
  where
  pdTasks = M.fromList $ map (\t -> (tId t, t)) tasks
  pdResources = M.fromList $ map (\r -> (rId r, r)) resources
  pdTasksPerResource = do
      t <- tasks
      rId <- tCanBeDoneBy t
      return (rId, tId t)


{-
  (t1, t2): t2 starts once t1 is finished

  Map t1 t2s : t1 starts when all t2s are finished
-}

-- assume the list of pre-requisite is sorted
possibleTasks :: Ord t => [t] -> M.Map t [t] -> [t]
possibleTasks completedTasks dependencies =
  S.toList $ M.foldrWithKey' proc S.empty dependencies
  where
  completedTasks' = L.sort completedTasks
  proc t ts acc =
    case L.isSubsequenceOf (L.sort ts) completedTasks' of
      True -> S.insert t acc
      False -> acc




