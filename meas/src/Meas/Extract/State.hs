{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}


{-# LANGUAGE TemplateHaskell #-}

module Meas.Extract.State
where

import Debug.Trace (trace)

import qualified  Data.List as L
import            Data.Maybe (catMaybes)
import            Data.Time.Calendar



import Meas.Extract.Misc
import Meas.Extract.Types




getStateTransitions :: Int -> [(Int, [ValueChange])] -> StateTransitions
getStateTransitions createTime changes =
  waitingBacklog $ removeConstantTransitions $ L.sortOn fst stateChanges'
  where
  -- Detect the initial state (which might be different from Backlog)
  -- The historical data does not create a change event upon issue creation
  stateChanges' =
    case stateChanges of
    [] -> [(createTime, Backlog)] -- By default we suppose the issue was created in the Backlog.
    (_, os, _):_ ->
      -- we use the old state of the first transition to get the initial state.
      -- And we prefix with state=Backlog.
      (createTime-1, Backlog) : (createTime, os) : L.map (\(t, _, ns) -> (t, ns)) stateChanges
  stateChanges = getStateChanges changes

-- extract unique state changes
getStateChanges :: [(Int, [ValueChange])] -> [(Int, StateValue, StateValue)]
getStateChanges changes = do
  (t, cgs) <- changes
  e <-  case catMaybes $ L.map getStateChange cgs of
          [] -> []
          ((os, ns):_) -> [(t, os, ns)]
  return e
  where
  getStateChange (StateChange os ns) = Just (os, ns)
  getStateChange _ = Nothing

{-
Identify a sequence of state transitions which follows the pattern:
~Backlog -> Backlog -> Selected -> WIP <-> WIP -> Done -> ....
where WIP stands for either InProgress or Review.

The sequence of state transitions can be not complete.
I.E
~Backlog -> Backlog -> InProgress -> ~Done
-}
waitingBacklog :: [(Int, StateValue)] -> StateTransitions
waitingBacklog [] = STIllegalStateTransitions
waitingBacklog ((t, Backlog):rest) = foundBacklog t rest
waitingBacklog ((t, Planning):rest) = foundBacklog t rest
waitingBacklog ((t, Selected):rest) = foundSelected t t rest
waitingBacklog l@((t, InProgress):_) = enterWip t t l
waitingBacklog l@((t, Review):_) = enterWip t t l
waitingBacklog ((t, Done):_) = STDone t t t t t

-- Backlog is found, try to find Selected
-- ignoring all backwards state transitions
foundBacklog :: Int -> [(Int, StateValue)] -> StateTransitions
foundBacklog tBacklog [] = STBacklog tBacklog
foundBacklog tBacklog ((_, Backlog):rest) = foundBacklog tBacklog rest
foundBacklog tBacklog ((_, Planning):rest) = foundBacklog tBacklog rest
foundBacklog tBacklog ((t, Selected):rest) = foundSelected tBacklog t rest
foundBacklog tBacklog l@((t, InProgress):_) = enterWip tBacklog t l
foundBacklog tBacklog l@((t, Review):_) = enterWip tBacklog t l
foundBacklog tBacklog ((t, Done):_) = STDone tBacklog t t t t

-- Selected is found, try to find WIP
-- ignoring all backwards state transitions
foundSelected :: Int -> Int -> [(Int, StateValue)] -> StateTransitions
foundSelected tBacklog tSelected [] = STSelected tBacklog tSelected
foundSelected tBacklog tSelected ((_, Backlog):rest) = foundSelected tBacklog tSelected rest
foundSelected tBacklog tSelected ((_, Planning):rest) = foundSelected tBacklog tSelected rest
foundSelected tBacklog tSelected ((_, Selected):rest) = foundSelected tBacklog tSelected rest
foundSelected tBacklog tSelected l@((_, InProgress):_) = enterWip tBacklog tSelected l
foundSelected tBacklog tSelected l@((_, Review):_) = enterWip tBacklog tSelected l
foundSelected tBacklog tSelected ((t, Done):_) = STDone tBacklog tSelected t t t



{-
Identify a sequence of either InProgress or Review transitions, possibly ending with a Done Transition.

Preconditions:
The list of transitions starts with either InProgress or Review
Transitions are sorted by ascending transition time.
No 2 consecutive transitions have the same state.
-}

enterWip :: Int -> Int -> [(Int, StateValue)] -> StateTransitions
enterWip _ _ [] = error "should not happen"
enterWip tBacklog tSelected trs@((tEnterWip, firstWipState):_) =
  if isBacklogWhileInWip trs
  then
    -- This state is a InProgress state, followed by a Backlog like state.
    -- Restart the whole state transition procedure but not taking into account the current
    -- state. Otherwise, we'll enter in a infinite loop
    waitingBacklog (L.tail trs)
  else
    case (rest1, timeInProgress, timeInReview, firstWipState) of
    -- Not Done state found
    ([], 0, 0, InProgress)    ->  STInProgress tBacklog tSelected tEnterWip
    ([], 0, 0, Review)        ->  STInReview tBacklog tSelected tEnterWip tEnterWip

    ([], 0, _, InProgress)    ->  STIllegalStateTransitions -- should never happen by construction
    ([], 0, _, Review)        ->  STInReview tBacklog tSelected tEnterWip tEnterWip

    ([], tip, 0, _)           ->  STInReview tBacklog tSelected tEnterWip (tEnterWip + tip)

    ([], _, _, _)             -> STIllegalStateTransitions  -- should never happen by construction

    -- At least one Done state found
    ((_, Done):_, 0, 0, _)    -> STIllegalStateTransitions
    ((td, Done):_, tip, _, _) -> STDone tBacklog tSelected tEnterWip (tEnterWip + tip) td
    _                         -> STIllegalStateTransitions

  where
  -- keep all transitions until we find a Done transition
  (trs1, rest1) = L.break (\(_, s) -> s == Done) trs
  -- remove spurious states, keep only InProgress and in Review
  trs2 = L.filter (\(_, s) -> s == InProgress || s == Review) trs1
  trs3 = if L.null rest1 then trs2 else trs2 ++ [L.head rest1]
  -- compute period between 2 consecutive transitions
  trPeriods = L.zipWith (\(t0, s0) (t1, _) -> (t1-t0, s0)) trs3 $ L.tail trs3
  -- sum periods where state = InProgress
  timeInProgress =  timeInState InProgress trPeriods
  timeInReview =  timeInState Review trPeriods

-- Is there a backlog state (Backlog|Selected|Planning)
isBacklogWhileInWip :: [(Int, StateValue)] -> Bool
isBacklogWhileInWip =
  go
  where
  go [] = False
  go ((_, Backlog):_) = True
  go ((_, Planning):_) = True
  go ((_, Selected):_) = True
  go (_:rest) = go rest


{-
When encountering 2 consecutive transitions with same State, remove the second.
This guarantees that each transition has a original state different from the target state
-}
removeConstantTransitions :: Eq a => [(Int, a)] -> [(Int, a)]
removeConstantTransitions trs =
  case trs of
  []  -> []
  [_] -> trs
  (h:t) -> go h t
  where
  go tr0 [] = [tr0]
  go (t0, s0) ((_, s1):rest) | s0 == s1 = go (t0, s0) rest
  go tr0 (tr1:rest) = tr0 : go tr1 rest


{- Given:
* a list of (period, state) where period is the period of time the state `state`is active.
* a state of reference `state'`.

Sum the period of times when `state'` is active.
-}
timeInState :: Eq s => s -> [(Int, s)] -> Int
timeInState state trPeriods = L.sum $ L.map fst $ L.filter (\(_, s) -> s == state) trPeriods


-- compute the age of the issue in its current state
timeOfCurrentState :: Int ->  [(Int, [ValueChange])] -> Int
timeOfCurrentState createdTime changes =
  L.last $ L.map fst stateChanges
  where
  stateChanges =  removeConstantTransitions $ L.sortOn fst ((createdTime, Backlog) : (L.map (\(t, _, ns) -> (t, ns)) $ getStateChanges changes))

ageInCurrentState :: Day -> Int -> [(Int, [ValueChange])] -> Integer
ageInCurrentState currentDay createdTime changes =
  diffDays currentDay dayCurrentState
  where
  dayCurrentState = toDay $ timeOfCurrentState createdTime changes


cycleTime :: Day -> StateTransitions -> Maybe Integer
cycleTime _          (STBacklog _)              = Nothing
cycleTime _          (STSelected _ _)           = Nothing
cycleTime currentDay (STInProgress _ _ t)       = Just $ diffDays currentDay $ toDay t
cycleTime currentDay (STInReview _ _ t _)       = Just $ diffDays currentDay $ toDay t
cycleTime currentDay (STDone _ _ t _ _)         = Just $ diffDays currentDay $ toDay t
cycleTime _          STIllegalStateTransitions  = Nothing



