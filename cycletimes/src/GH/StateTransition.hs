{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE RecordWildCards   #-}

module GH.StateTransition
(
  getStateTransitions
, getStateEvents
, stateEventSummary
)
where

import qualified  Data.List as L
import qualified  Data.Text as T
import            Data.Time.Clock
import            Data.Time.Format


import            GH.Types


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
transitionStep (STBacklog tb)        (StateEvent _ Backlog _)       = STBacklog tb
transitionStep (STBacklog tb)        (StateEvent _ InProgress tp)   = STInProgress tb tp
transitionStep (STBacklog tb)        (StateEvent _ InReview tr)     = STInReview tb tr tr
transitionStep (STBacklog tb)        (StateEvent _ Neutral _)       = STBacklog tb

transitionStep (STInProgress tb tp)  (StateEvent _ Backlog _)       = STInProgress tb tp
transitionStep (STInProgress tb tp)  (StateEvent _ InProgress _)    = STInProgress tb tp
transitionStep (STInProgress tb tp)  (StateEvent _ InReview tr)     = STInReview tb tp tr
transitionStep (STInProgress tb tp)  (StateEvent _ Done td)         = STDone tb tp td td
transitionStep (STInProgress tb tp)  (StateEvent _ Neutral _)       = STInProgress tb tp

transitionStep (STInReview tb tp tr) (StateEvent _  Backlog _)      = STInReview tb tp tr
transitionStep (STInReview tb tp tr) (StateEvent _  InProgress _)   = STInReview tb tp tr
transitionStep (STInReview tb tp tr) (StateEvent _  InReview _)     = STInReview tb tp tr
transitionStep (STInReview tb tp tr) (StateEvent _  Done td)        = STDone tb tp tr td
transitionStep (STInReview tb tp tr) (StateEvent _  Neutral _)      = STInReview tb tp tr

transitionStep (STDone tb tp tr _)   (StateEvent _ InProgress _)    = STInReview tb tp tr
transitionStep (STDone tb tp tr _)   (StateEvent _ InReview _)      = STInReview tb tp tr
transitionStep (STDone tb tp tr _)   (StateEvent _ Done td)         = STDone tb tp tr td
transitionStep (STDone tb tp tr _)   (StateEvent _ Neutral _)       = STInReview tb tp tr

transitionStep _ _ = STIllegalStateTransitions





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
  go _ currentState acc (ZHE (ZHEvtTransferState _ sf t):rest) =
    go currentState sf (StateEvent currentState sf t : acc) rest

  go previousState currentState acc (GHE (GHEvtReOpenEvent t):rest) =
    case currentState of
      Done -> go currentState  previousState (StateEvent currentState previousState t : acc) rest
      _    -> error $ "Bad current state (" ++ show currentState ++ ") in case of re-open event"

  go _ currentState acc (GHE (GHEvtCloseEvent t):rest) =
    go currentState Done (StateEvent currentState Done t : acc) rest




stateEventSummary :: Issue -> [String]
stateEventSummary MkIssue{..} =
  (T.unpack iRepoName ++ "-" ++ show ghiNumber):(utcTimeToDateString ghiCreationTime ++ "  Created") : (L.map go evts)
          ++ ("": L.map go' stateChanges)
  where
  stateChanges = getStateEvents iGHIssueEvents iZHIssueEvents
  MkGHIssue {..} = iGHIssue
  evts = (L.sort $ (GHE <$> iGHIssueEvents) ++ (ZHE <$> iZHIssueEvents))
  go (ZHE (ZHEvtTransferState si sf t)) = utcTimeToDateString t ++ "  ZH: " ++ show si ++ " => " ++ show sf
  go (GHE (GHEvtCloseEvent t))          = utcTimeToDateString t ++ "  Closed"
  go (GHE (GHEvtReOpenEvent t))         = utcTimeToDateString t ++ "  Re Opened"
  go' (StateEvent si sf t)              = utcTimeToDateString t ++ "  TR: " ++ show si ++ " => " ++ show sf


  utcTimeToDateString :: UTCTime -> String
  utcTimeToDateString t = formatTime defaultTimeLocale  "%d-%m-%Y" t



