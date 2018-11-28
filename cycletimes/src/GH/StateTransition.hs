{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE RecordWildCards   #-}

module GH.StateTransition
(
  getStateTransition
)
where

import GH.Types

-- | Given the creation time and a list of StateChange Events returns the
--   final StateTransition
--   PreCondition : The list of StateChange Events is assumed to be sorted
--   according to time
getStateTransition :: TimeStamp -> [StateEvent] -> StateTransitions
getStateTransition t stateChanges = go (STBacklog t) stateChanges
  where
    go :: StateTransitions -> [StateEvent] -> StateTransition
    go finalState [] = finalState
    go currState (se:moreEvents) = go (transitionStep currState se) moreEvents


-- | The Core logic of StateTransition from one State to another
transitionStep :: StateTransition -> StateEvent -> StateTransition
transitionStep (STBacklog tb)        (Transition Backlog InProgress tp)  = STInProgress tb tp
transitionStep (STBacklog tb)        (Transition Backlog InReview tr)    = STInReview tb tr tr
transitionStep (STInProgress tb tp)  (Transition InProgress Backlog _)   = STInProgress tb tp
transitionStep (STInProgress tb tp)  (Transition InProgress InReview tr) = STInReview tb tp tr
transitionStep (STInProgress tb tp)  (Transition InProgress Done td)     = STDone tb tp td td
transitionStep (STInReview tb tp tr) (Transition InReview  Backlog _)    = STInReview tb tp tr
transitionStep (STInReview tb tp tr) (Transition InReview  InProgress _) = STInReview tb tp tr
transitionStep (STInReview tb tp tr) (Transition InReview  Done td)      = STDone tb tp tr td
transitionStep (STDone tb tp tr td)  (Transition Done InProgress _)      = STInReview tb tp tr
transitionStep (STDone tb tp tr td)  (Transition Done InReview _)        = STInReview tb tp tr
transitionStep _ _ = STIllegalStateTransitions


