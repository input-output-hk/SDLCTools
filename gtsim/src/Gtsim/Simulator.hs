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

-- GENERAL REMARKS:
--
-- - I would somewhat prefer to have a different
-- type / newtype for durations (time deltas) than for absolute
-- times.
--
-- - There are quite a few situations again where mapM / foldM
-- is being used and streaming might be somewhat better. But
-- here, at least, the lists this is being run on are typically
-- bounded by the problem size, so it's probably still ok in
-- practice.

-- TODO: Needs documentation as to what the arguments mean.
-- I will try to guess and add comments myself.
--
-- The result of 'runSimulation' seems to be either a list
-- of errors or a final state.
--
-- There should probably be a type synonym such as
--
-- 'type Invariant = (Bool, String)'
--
-- so that we can make the first two arguments more readable.
--
runSimulation :: (SimState -> [(Bool, [Char])]) -- ^ Should be explained better
                 -> (ProblemDefinition -> SimState -> [(Bool, [Char])]) -- ^ Should be explained better
                 -> ProblemDefinition -- ^ actual problem definition
                 -> Day -- ^ time granularity
                 -> SimState -- ^ current state
                 -> Gen (Either [[Char]] SimState)
runSimulation inv finalInv problemDefinition deltaTime = go
  where
  go :: SimState -> Gen (Either [[Char]] SimState)
  go simState0 = do
    let nextTime = sstCurrentTime simState0 + deltaTime

    simState1 <- chooseNewActiveAssignments problemDefinition simState0 -- step 2
    simState2 <- selectRunningAssignments problemDefinition simState1 -- step 3
    simState3 <- update simState2 nextTime -- step 4

    checkInv (inv simState3) $ do
      if L.null (sstActiveAssignments simState0) && L.null (sstActiveAssignments simState3)
        then return $ Left  ["Incorrect problem statement"]
        else case L.length (sstCompletedAssignments simState3) == M.size (pdTasks problemDefinition) of
                True -> checkInv (finalInv problemDefinition simState3) (return $ Right simState3)
                False -> go simState3

  update simState0' nextTime = do
    (simState1', endTimes) <- updateActiveAssignments problemDefinition nextTime simState0'
    -- TODO: Doing this uniqueness step seems wrong and useless, as
    -- we're only checking whether the list is empty and compute what
    -- the minimum is. This is going to me more efficient overall
    -- without the extra datastructure conversion.
    let uniqueEndTimes = S.toList $ S.fromList endTimes
    -- TODO: This needs documentation. The purpose here seems to be to
    -- correct our computed time for the next step if a task ends early.
    -- This is to enable the resource to pick up a new task as early
    -- as possible, of course.
    --
    -- But it seems problematic, not just for efficiency reasons. It means
    -- that the number of times certain probability distributions get
    -- samples depends not just on the granularity, but also on the
    -- number of times we need to do these correction steps.
    --
    case uniqueEndTimes of
      [] -> do
        -- no task completed during this step, we are done with the update
        return $ simState1' {sstCurrentTime = nextTime}
      _ -> do
        -- at least one task completed
        -- by construction, all uniqueEndTimes < nextTime
        -- we went too far in time, restart with nextTime = smallest uniqueEndTimes
        let minNextTime = L.minimum uniqueEndTimes
        -- REMARK: This actually *relies* on updateActiveAssignments not
        -- using 'Gen' to even be correct. One more reason to turn it into
        -- a pure function.
        (simState1'', _) <- updateActiveAssignments problemDefinition minNextTime simState0'
        return $ simState1'' {sstCurrentTime = minNextTime}

-- REMARK: Should be global, so I changed it.
--
-- TODO: This should be pure.
--
checkInv :: [(Bool, String)] -> Gen (Either [String] a) -> Gen (Either [String] a)
checkInv invR k
 | L.all fst invR = k
 | otherwise =
     let
       msgs = L.map snd $ L.filter (not . fst) invR
     in
       return $ Left msgs



{-
Step 2
-}
chooseNewActiveAssignments :: ProblemDefinition -> SimState  -> Gen SimState
chooseNewActiveAssignments problemDefinition simState = do
  resources <- shuffle $ M.elems (pdResources problemDefinition)
  foldM (chooseNewActiveAssignmentsForResource problemDefinition) simState resources

-- Still part of step 2.
chooseNewActiveAssignmentsForResource :: ProblemDefinition -> SimState -> Resource -> Gen SimState
chooseNewActiveAssignmentsForResource MkProblemDefinition{..} simState@MkSimState{..} MkResource{..} = do
  let activeAssignments = getActiveAssignmentByResource rId sstActiveAssignments
  let naa = L.length activeAssignments
  nca <- rNbActiveAssignmentsGen

  -- REMARK: Personally, I find if-then-else more idiomatic than case of True/False.
  case nca > naa of
    True -> do
      -- Yes, we can choose (nca - naa) new tasks amongst the available ones
      -- which are compatible with the resource.
      availableTaskIds <- shuffle $ getAvailableTaskIds sstCompletedAssignments sstActiveAssignments pdDependencies
      let compatibleTaskIds = selectCompatibleTasksForResource pdTasksPerResource availableTaskIds rId
      -- MINOR REMARK: I would probably determine the compatible tasks first, and then shuffle.
      -- then we select a subset of these taskIds and create new Active Assignments.
      newActiveAssignments <- mapM (\tId -> do
            mandays <- tManDaysGen (pdTasks M.! tId) -- determine the time the task is actually going to take
            return $ MkActiveAssignment tId rId mandays mandays True sstCurrentTime -- create an active assignment for selected task
            ) (L.take (nca - naa) compatibleTaskIds)
      -- MINOR REMARK: forM might be better here than mapM. and also, consider
      -- extracting 'activateAssignment' into its own function.

      -- update the state with these new Active Assignments.
      -- and we are done with this resource
      return $ simState {sstActiveAssignments = newActiveAssignments ++ sstActiveAssignments }
    False -> return simState  -- we can't add new Active Assignments

-- REMARK: This was a local function for 'chooseNewActiveAssignmentsForResource',
-- but there was nothing local about it. As a rule of thumb, anything that can
-- be global should be global, because then it's independently testable and also
-- usable in GHCi. If locality is desired from an abstraction viewpoint, then
-- the module system should be used to hide internal definitions at some point.
--
selectCompatibleTasksForResource ::
  [(ResourceId, TaskId)] -> [TaskId] -> ResourceId -> [TaskId]
selectCompatibleTasksForResource taskPerResource taskIds resourceId =
  L.filter (\tId -> L.elem (resourceId, tId) taskPerResource) taskIds

{-
Step 3
-}
-- REMARK: I guess this function "justifies" the inclusion of the
-- "running" flag in the simulation state. But really this is just
-- temporary information needed, so it should be computed and passed
-- directly, rather than as part of the simulation state.
--
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
-- TODO: This needs more documentation, even given the explanations
-- in the spec. What does it return, for example?
--
-- Also, I think this should be a pure function.
-- Nothing in here seems to need sampling.
updateActiveAssignments :: ProblemDefinition -> Day -> SimState -> Gen (SimState, [Day])
updateActiveAssignments problemDefinition nextTime simState@MkSimState{..} =
  foldM
    (updateActiveAssignmentsForResource problemDefinition nextTime)
    (simState, [])
    (M.elems (pdResources problemDefinition))


-- TODO: This should be a pure function.
updateActiveAssignmentsForResource :: ProblemDefinition -> Day -> (SimState, [Day]) -> Resource -> Gen (SimState, [Day])
updateActiveAssignmentsForResource MkProblemDefinition{..} nextTime (simState@MkSimState{..}, endTimes) MkResource{..} = do
  let (activeAssignments, others) = partitionActiveAssignmentsByResource rId sstActiveAssignments

  -- REMARK: As I said for selectRunningAssignments already, if we'd
  -- simply let that function compute the per-resource running assignments
  -- and then pass these as an input here, we would have to do less work
  -- in taking the SimState apart again.
  let activeAssignmentsToWorkOn = L.filter aaRunning activeAssignments
  -- determine the efficiency
  let eff = rEfficiency $ L.length activeAssignmentsToWorkOn

  -- update the remaining mandays, possibly completing tasks
  -- verify strictness here
  --
  -- TODO: strictness indeed looks problematic, because we're
  -- accumulating over a lazy triple.
  let (newActiveAssignments, newCompletedAssignments, endTimes') =
        L.foldl'
          (updateProc eff)
          (others, sstCompletedAssignments, endTimes)
          activeAssignments

  -- Update the states with these new Active/Completed Assignments
  return $ (simState {sstActiveAssignments = newActiveAssignments, sstCompletedAssignments = newCompletedAssignments}, endTimes')
  where
  -- TODO: Needs a type signature, I'll try to add one now.
  --
  -- REMARK: This looks surprisingly convoluted. Do we really need
  -- to accumulate over this triple? Can we not just write this as
  -- a map and post-process the results?
  --
  -- This whole steps feels very imperative and low-level to me.
  -- I would look for a solution where can talk about all assignments
  -- at once. Basically, we first want to update all the active
  -- assignments (with their new remaining time). Then turn some
  -- of the active assignments into completed assignments.
  --
  updateProc ::
       ManDaysPerDay
    -> ([ActiveAssignment], [CompletedAssignment], [Day])
    -> ActiveAssignment
    -> ([ActiveAssignment], [CompletedAssignment], [Day])
  updateProc eff (aas, cas, endTimes') activeAssignment@MkActiveAssignment{..} =
    case remainingMandays' <= 0 of -- TODO: more idiomatic using guards
    True ->
      let
        -- Assignment is complete.
        -- We do not retain it (in aas) but we create and save a new Complete Assignment (in cas).
        -- We also register the end time for that Assignment.
        te = sstCurrentTime +  aaRemainingMandays / eff
        newCompletedAssignment = MkCompletedAssignment aaTaskId aaResourceId aaManDays aaStartTime te
          -- TODO: the above line I would extract into a helper function, simply because it is
          -- so conceptually expected to have a function called
          -- 'completeAssignment :: ActiveAssignment -> EndTime -> CompletedAssignment'
      in
        (aas, newCompletedAssignment : cas, te : endTimes')
    False ->
      let
        -- Not finished yet, we update it and keep it in aas.
        newActiveAssignment = activeAssignment {aaRemainingMandays = remainingMandays'}
      in
        (newActiveAssignment : aas, cas, endTimes')
    where
    remainingMandays' =
      if aaRunning
        then aaRemainingMandays - eff * (nextTime - sstCurrentTime)
        else aaRemainingMandays

-- REMARK:
-- Given that this function is still considerable work
-- (both in lines of code and computationally), I wonder
-- if it wasn't better after all to at least keep the
-- list of tasks still to do directly as part of the
-- simulation state.
--
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

-- MINOR REMARK:
--
-- I had to look up what this function means, and it would
-- have cost me less time if this had just been inlined.
--
partitionActiveAssignmentsByResource :: ResourceId
                                        -> [ActiveAssignment]
                                        -> ([ActiveAssignment], [ActiveAssignment])
partitionActiveAssignmentsByResource resourceId elems =
  L.partition (\aa -> aaResourceId aa == resourceId) elems

-- MINOR REMARK: Similar as 'partitionActiveAssignmentsByResource'.
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




