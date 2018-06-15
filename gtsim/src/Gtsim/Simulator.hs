{-# LANGUAGE RecordWildCards #-}


module Gtsim.Simulator

where

--import Debug.Trace (trace)

import            Control.Monad
import qualified  Data.List as L
import qualified  Data.Map.Strict as M
import            Data.Map.Strict (Map)
import qualified  Data.Set as S
import            Test.QuickCheck (Gen, shuffle)

import Gtsim.Types

-- GENERAL REMARKS:
--
-- - I would somewhat prefer to have a different
-- type / newtype for durations (time deltas) than for absolute
-- times.
-- J-C : I don't see clear benefits here. But why not, I'll see how it looks like.
-- ANDRES: The benefit is that types are more self-documenting.
--
-- - There are quite a few situations again where mapM / foldM
-- is being used and streaming might be somewhat better. But
-- here, at least, the lists this is being run on are typically
-- bounded by the problem size, so it's probably still ok in
-- practice.
-- J-C : yes I think so.

-- TODO: Needs documentation as to what the arguments mean.
-- I will try to guess and add comments myself.
--
-- The result of 'runSimulation' seems to be either a list
-- of errors or a final state.
--
-- There should probably be a type synonym such as
--
-- 'type Invariant = (Bool, String)'
-- J-C : ok added in Type.hs
--
-- so that we can make the first two arguments more readable.
--
-- J-C : BTW you see how invariants are checked inside the algo. It is quite useful when testing but I
-- wonder whether there is a better way to achieve it.
-- ANDRES: I think optionally passing in invariants to check
-- is fine. The only other option I can think of is to hardcode
-- the invariants to check in the algorithm, but have a flag
-- to the algorithm that determines whether they're run.
--
runSimulation :: (SimState -> [InvariantResult]) -- ^ Checks transient invariants (at the end of each time step)
                 -> (ProblemDefinition -> SimState -> [InvariantResult]) -- ^ Checks final invariants (at the end of the simulation)
                 -> ProblemDefinition -- ^ actual problem definition
                 -> Day -- ^ time granularity
                 -> SimState -- ^ initial state
                 -> Gen (Either [String] SimState) -- ^ The final simulation state or errors if the simulation failed (i.e invariants not respected).
runSimulation inv finalInv problemDefinition deltaTime = go
  where
  go :: SimState -> Gen (Either [String] SimState)
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
    let (simState1', endTimes) = updateActiveAssignments problemDefinition nextTime simState0'

    -- TODO: Doing this uniqueness step seems wrong and useless, as
    -- we're only checking whether the list is empty and compute what
    -- the minimum is. This is going to me more efficient overall
    -- without the extra datastructure conversion.

    -- D-K : Done ^^ removed uniqueness step

    -- TODO: This needs documentation. The purpose here seems to be to
    -- correct our computed time for the next step if a task ends early.
    -- This is to enable the resource to pick up a new task as early
    -- as possible, of course.
    -- J-C : yes the reason is explained in the specs.
    --
    -- But it seems problematic, not just for efficiency reasons. It means
    -- that the number of times certain probability distributions get
    -- samples depends not just on the granularity, but also on the
    -- number of times we need to do these correction steps.

    --J-C :
    -- * efficiency:
    -- The cost has basically 2 components:
    -- 1) update the remaining effort for each running assignments + end time.
    -- 2) update of the sim state
    -- In case the list of end times is empty, there is not loss of efficiency.
    -- In case that list is not empty, it is less efficient not because we have to redo the computation
    -- with a smaller time (tmin) but because we do not take advantage of the knowledge about the Assignments which are just finished.
    -- These will be recomputed once again.
    -- REM: due to lazyness, I am not even sure that the simulation state is updated in that case.
    -- Now, in a real situation, I don't think this is a real problem because:
    -- 1) most of the time, only 1 assignment is finished at t=tmin.
    -- 2) deltaTime (about a day) <<< elapsed time to complete a task. So the list of end times will often be empty.

    -- Dependency between timeSteps and distribution:
    -- Waiting/Running distr: no impact.
    -- Nb of tasks assigned to a resource: yes there is one but we have it anyway, independently of this little time
    -- adjustment. I discussed that in the specs, in my answser to one of your previous questions. And there is a solution to it.

    case endTimes of
      [] -> do
        -- no task completed during this step, we are done with the update
        return $ simState1' {sstCurrentTime = nextTime}
      _ -> do
        -- at least one task completed
        -- by construction, all uniqueEndTimes < nextTime
        -- we went too far in time, restart with nextTime = smallest uniqueEndTimes
        let minNextTime = L.minimum endTimes
        -- REMARK: This actually *relies* on updateActiveAssignments not
        -- using 'Gen' to even be correct. One more reason to turn it into
        -- a pure function.
        -- J-C : sure, you are right. I did it in haste.
            (simState1'', _) = updateActiveAssignments problemDefinition minNextTime simState0'
        return $ simState1'' {sstCurrentTime = minNextTime}

-- REMARK: Should be global, so I changed it.
--
-- TODO: This should be pure.
-- J-C: indeed!
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
  let activeAssignments = L.filter (\aa -> aaResourceId aa == rId) sstActiveAssignments
  let naa = L.length activeAssignments
  nca <- rNbActiveAssignmentsGen

  -- REMARK: Personally, I find if-then-else more idiomatic than case of True/False.
  -- J-C : fine with me, I always ask myself the question. If find the 'case' construct more beautiful, but... It is me.
  case nca > naa of
    True -> do
      -- Yes, we can choose (nca - naa) new tasks amongst the available ones
      -- which are compatible with the resource.

      let availableTaskIds = getAvailableTaskIds sstCompletedAssignments sstActiveAssignments pdDependencies
      newCompatibleTaskIds <- fmap (L.take (nca - naa)) . shuffle $ selectCompatibleTasksForResource pdTasksPerResource availableTaskIds rId

      -- MINOR REMARK: I would probably determine the compatible tasks first, and then shuffle.
      -- then we select a subset of these taskIds and create new Active Assignments.
      --
      -- D-K : ^^ Done
      newActiveAssignments <- forM newCompatibleTaskIds $ createActiveAssignment pdTasks rId sstCurrentTime

      -- MINOR REMARK: forM might be better here than mapM. and also, consider
      -- extracting 'activateAssignment' into its own function.
      -- J-C : Oops: I don't understand this remark.
      --
      -- D-K : Done ^^

      -- update the state with these new Active Assignments.
      -- and we are done with this resource
      return $ simState {sstActiveAssignments = newActiveAssignments ++ sstActiveAssignments }
    False -> return simState  -- we can't add new Active Assignments

-- | @createActiveAssignment@ creates a new ActiveAssignment for the given Task and Resource.
createActiveAssignment :: Map TaskId Task -> ResourceId -> Day -> TaskId -> Gen ActiveAssignment
createActiveAssignment taskMap rid time tid = do
  mandays <- tManDaysGen (taskMap M.! tid)                       -- determine the time the task is actually going to take
  return $ MkActiveAssignment tid rid mandays mandays True time  -- create an active assignment for selected task


-- REMARK: This was a local function for 'chooseNewActiveAssignmentsForResource',
-- but there was nothing local about it. As a rule of thumb, anything that can
-- be global should be global, because then it's independently testable and also
-- usable in GHCi. If locality is desired from an abstraction viewpoint, then
-- the module system should be used to hide internal definitions at some point.
--
-- J-C : Good to know. I always hesitate between local and global function.
-- I am not a big fan of complex local function, though.
-- My criterion here is that it was only used locally.
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
-- J-C : Yes (see my remark about that field in Type.hs). it was done in haste...
-- In fact, I noticed the need for this little go-back-time step while I was testing the code.
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

-- J-C : pure function: yes.
-- The initial version included the selection of  running assignments.
-- That last minute refactoring was done in haste (you have guessed it by now...)
--
-- D-K : Done ^^ , Changed to a pure function

updateActiveAssignments :: ProblemDefinition -> Day -> SimState -> (SimState, [Day])
updateActiveAssignments problemDefinition nextTime simState@MkSimState{..} =
  L.foldl'
    (updateActiveAssignmentsForResource problemDefinition nextTime)
    (simState, [])
    (M.elems (pdResources problemDefinition))


-- TODO: This should be a pure function.
-- J-C : yes (see my comment above)
-- D-K : Done ^^
updateActiveAssignmentsForResource :: ProblemDefinition -> Day -> (SimState, [Day]) -> Resource -> (SimState, [Day])
updateActiveAssignmentsForResource MkProblemDefinition{..} nextTime (simState@MkSimState{..}, endTimes) MkResource{..} =
  let (activeAssignments, others) = L.partition (\aa -> aaResourceId aa == rId) sstActiveAssignments

  -- REMARK: As I said for selectRunningAssignments already, if we'd
  -- simply let that function compute the per-resource running assignments
  -- and then pass these as an input here, we would have to do less work
  -- in taking the SimState apart again.

  -- J-C: yes, I am sure there is a more elegant solution here.
  -- To be honest, I am not really satisfied with this part of the code.
  -- And moreover, I had to move from `Double` to `Rational` to make the algo terminate due to rounding issue.
  -- So although it is correct on paper, it is not a robust implementation.
  -- That could be a good exercice for Deepak.
      activeAssignmentsToWorkOn = L.filter aaRunning activeAssignments
  -- determine the efficiency
      eff = rEfficiency $ L.length activeAssignmentsToWorkOn

  -- update the remaining mandays, possibly completing tasks
  -- verify strictness here
  --
  -- TODO: strictness indeed looks problematic, because we're
  -- accumulating over a lazy triple.
  -- J-C: could Deepak look at it?
  -- Now, given my remark above about the "back to the future" trick, I think we need some degree of laziness for 'newActiveAssignments' and 'newCompletedAssignments'.

      (newActiveAssignments, newCompletedAssignments, endTimes') =
        L.foldl'
          (updateProc eff)
          (others, sstCompletedAssignments, endTimes)
          activeAssignments

  -- Update the states with these new Active/Completed Assignments
  in (simState {sstActiveAssignments = newActiveAssignments, sstCompletedAssignments = newCompletedAssignments}, endTimes')
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
  -- J-C : Ok so:
  -- 1) a map to update remaing mandays.
  -- 2) a fold to turn Active Assignments into Complete one, based on the predicate 'aaRemainingMandays==0'
  -- 3) Not forgetting to maintain the list of end times.
  -- Fine with me.
  updateProc ::
       ManDaysPerDay
    -> ([ActiveAssignment], [CompletedAssignment], [Day])
    -> ActiveAssignment
    -> ([ActiveAssignment], [CompletedAssignment], [Day])
  updateProc eff (aas, cas, endTimes') activeAssignment@MkActiveAssignment{..} =
    case remainingMandays' <= 0 of -- TODO: more idiomatic using guards. J-C : ok
    True ->
      let
        -- Assignment is complete.
        -- We do not retain it (in aas) but we create and save a new Complete Assignment (in cas).
        -- We also register the end time for that Assignment.
        te = sstCurrentTime +   aaRemainingMandays / eff
        newCompletedAssignment = MkCompletedAssignment aaTaskId aaResourceId aaManDays aaStartTime te
          -- TODO: the above line I would extract into a helper function, simply because it is
          -- so conceptually expected to have a function called
          -- 'completeAssignment :: ActiveAssignment -> EndTime -> CompletedAssignment'
          -- J-C : ok
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
-- J-C : isn't there any clever "functional pearl"-like technique to speed it up?
-- or there could be another solution such as
-- data AvailableTaskProvider :: MkAvailableTaskProvider
--        {unfct :: [CompletedAssignment] -> [ActiveAssignment] -> ([TaskId], AvailableTaskProvider)}

-- But of course, it boils down to maintaining a state but that state will be separated form SimState (somewhat).
-- Anyway, just an idea. Feel free to find something adequate here.
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
-- J-C : inlined? You mean: not put into a separate function?
-- ANDRES: yes
-- D-K : Done ^^

-- MINOR REMARK: Similar as 'partitionActiveAssignmentsByResource'.
-- D-K : Done ^^

initialize :: [Task] -> [Resource] -> M.Map TaskId [TaskId] -> [(ResourceId, TaskId)] -> Gen Bool -> (SimState, ProblemDefinition)
initialize tasks resources dependencies tasksPerResource runningAssignmentGen =
  ( MkSimState 0 [] []
  , MkProblemDefinition pdTasks pdResources pdTasksPerResource dependencies runningAssignmentGen
  )
  where
    pdTasks = M.fromList $ map (\t -> (tId t, t)) tasks
    pdResources = M.fromList $ map (\r -> (rId r, r)) resources
    pdTasksPerResource = tasksPerResource


{-
  (t1, t2): t2 starts once t1 is finished
  Map t1 t2s : t1 starts when all t2s are finished
-}

-- assume the list of pre-requisite is sorted

-- J-C could also be improved.
possibleTasks :: Ord t => [t] -> M.Map t [t] -> [t]
possibleTasks completedTasks dependencies =
  S.toList $ M.foldrWithKey' proc S.empty dependencies
  where
  completedTasks' = L.sort completedTasks
  proc t ts acc =
    case L.isSubsequenceOf (L.sort ts) completedTasks' of
      True -> S.insert t acc
      False -> acc
