{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE RecordWildCards   #-}

module GH.StateTransition
(
  getStateTransitions
, getStateEvents
)
where

import           GH.Types
import qualified Data.List as L

-- | Given the creation time and a list of StateChange Events returns the
--   final StateTransition
--   PreCondition : The list of StateChange Events is assumed to be sorted
--   according to time
getStateTransitions :: TimeStamp -> [StateEvent] -> StateTransitions
getStateTransitions t stateChanges = go (STBacklog t) stateChanges
  where
    go :: StateTransitions -> [StateEvent] -> StateTransitions
    go finalState [] = finalState
    go currState (se:moreEvents) = go (transitionStep currState se) moreEvents


-- | The Core logic of StateTransition from one State to another
transitionStep :: StateTransitions -> StateEvent -> StateTransitions
transitionStep (STBacklog tb)        (StateEvent Backlog Backlog _)       = STBacklog tb
transitionStep (STBacklog tb)        (StateEvent Backlog InProgress tp)   = STInProgress tb tp
transitionStep (STBacklog tb)        (StateEvent Backlog InReview tr)     = STInReview tb tr tr
transitionStep (STInProgress tb tp)  (StateEvent InProgress Backlog _)    = STInProgress tb tp
transitionStep (STInProgress tb tp)  (StateEvent InProgress InProgress _) = STInProgress tb tp
transitionStep (STInProgress tb tp)  (StateEvent InProgress InReview tr)  = STInReview tb tp tr
transitionStep (STInProgress tb tp)  (StateEvent InProgress Done td)      = STDone tb tp td td
transitionStep (STInReview tb tp tr) (StateEvent InReview  Backlog _)     = STInReview tb tp tr
transitionStep (STInReview tb tp tr) (StateEvent InReview  InProgress _)  = STInReview tb tp tr
transitionStep (STInReview tb tp tr) (StateEvent InReview  InReview _)    = STInReview tb tp tr
transitionStep (STInReview tb tp tr) (StateEvent InReview  Done td)       = STDone tb tp tr td
transitionStep (STDone tb tp tr td)  (StateEvent Done InProgress _)       = STInReview tb tp tr
transitionStep (STDone tb tp tr td)  (StateEvent Done InReview _)         = STInReview tb tp tr
transitionStep (STDone tb tp tr _)   (StateEvent Done Done td)            = STDone tb tp tr td
transitionStep _ _ = STIllegalStateTransitions

{-}


data IssueEvent = GHE GHIssueEvent | ZHE ZHIssueEvent
  deriving (Show, Eq)

instance Ord IssueEvent where
  compare ie1 ie2 = compare (getTime ie1) (getTime ie2)
    where
      getTime (ZHE (ZHEvtTransferState _ _ t)) = t
      getTime (GHE (GHEvtCloseEvent t))        = t
      getTime (GHE (GHEvtReOpenEvent t))       = t


-- | Given a list of GHIssueEvent and ZHIssueEvent returns a list of StateEvent by merging both
getStateEvents :: [GHIssueEvent] -> [ZHIssueEvent] -> [StateEvent]
getStateEvents ghs zhs =
  case evts of
  [] -> []
  ZHE (ZHEvtTransferState Backlog sf t):rest -> go Backlog sf [StateEvent Backlog sf t] rest
  ZHE (ZHEvtTransferState _ _ _):_ -> error "Initial State is not Backlog on the 1st ZH transition"
  GHE (GHEvtCloseEvent t):rest -> go Backlog Done [StateEvent Backlog Done t] rest
  GHE (GHEvtReOpenEvent _):_ -> error "Reopening non closed issue"
  where
  evts = (L.sort $ (GHE <$> ghs) ++ (ZHE <$> zhs))


go _ _ acc [] = reverse acc
go _ currentState acc (ZHE (ZHEvtTransferState si sf t):rest) =
  if currentState == si
  then go si sf (StateEvent si sf t : acc) rest
  else error $ "Bad current state (" ++ show currentState ++ ") in case of ZH event"

go previousState currentState acc (GHE (GHEvtReOpenEvent t):rest) =
  case currentState of
    Done -> go currentState  previousState (StateEvent currentState previousState t : acc) rest
    _    -> error $ "Bad current state (" ++ show currentState ++ ") in case of re-open event"

go _ currentState acc (GHE (GHEvtCloseEvent t):rest) =
  go currentState Done (StateEvent currentState Done t : acc) rest




-- data GHIssueEvent =
--   GHEvtCloseEvent TimeStamp
--   |GHEvtReOpenEvent TimeStamp
--   deriving (Show, Eq)

packEvts :: [GHIssueEvent] -> [ZHIssueEvent] -> [StateEvent]
packEvts ghEvts zhEvts = L.sort $ map CGHEvt ghEvts ++ map CZHEvt zhEvts
-}



-- future test code (2nd, independent, implementation)

data CEvt =
  CGHEvt GHIssueEvent
  |CZHEvt ZHIssueEvent
  deriving (Show, Eq)

instance Ord CEvt where
  compare ge1 ge2 =
    compare (getTime ge1) (getTime ge2)
    where
    getTime (CGHEvt (GHEvtCloseEvent t)) = t
    getTime (CGHEvt (GHEvtReOpenEvent t)) = t
    getTime (CZHEvt (ZHEvtTransferState _ _ t)) = t


getStateEvents :: [GHIssueEvent] -> [ZHIssueEvent] -> [StateEvent]
getStateEvents ghEvts zhEvts =
  case cevts of
  []                                        -> []
  CZHEvt (ZHEvtTransferState si sf t):rest  -> go [StateEvent si sf t] si sf rest
  CGHEvt (GHEvtCloseEvent t):rest           -> go [StateEvent Backlog Done t] Backlog Done rest
  CGHEvt (GHEvtReOpenEvent t):rest          -> error "Cannot Reopen non closed issue"
  where
  cevts = L.sort $ map CGHEvt ghEvts ++ map CZHEvt zhEvts

  go acc _ _ [] = L.sort acc
  go acc previousState currentState (CZHEvt (ZHEvtTransferState si sf t):rest) =
    go (StateEvent si sf t : acc) currentState sf rest

  go acc previousState currentState (CGHEvt (GHEvtCloseEvent t):rest) =
    go (StateEvent currentState Done t : acc) currentState Done rest

  go acc previousState currentState (CGHEvt (GHEvtReOpenEvent t):rest) =
    go (StateEvent currentState previousState t : acc) currentState previousState rest






