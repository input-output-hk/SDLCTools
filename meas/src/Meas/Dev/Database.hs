
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Meas.Dev.Database
where


import qualified  Data.Text as T
import            Database.PostgreSQL.Simple
import            Database.PostgreSQL.Simple.ToField
import            Database.PostgreSQL.Simple.ToRow

import Meas.Misc
import Meas.Dev.State
import Meas.Dev.Types

instance ToField PriorityValue where
  toField ShowStopper = toField ("ShowStopper"::String)
  toField Critical    = toField ("Critical"::String)
  toField Major       = toField ("Major"::String)
  toField Normal      = toField ("Normal"::String)
  toField Minor       = toField ("Minor"::String)

instance ToField StateValue where
  toField Backlog     = toField ("Backlog"::String)
  toField Planning    = toField ("Planning"::String)
  toField Selected    = toField ("Selected"::String)
  toField InProgress  = toField ("InProgress"::String)
  toField Review      = toField ("Review"::String)
  toField Done        = toField ("Done"::String)
  toField Neutral     = toField ("Neutral"::String)

instance ToField WaitValue where
  toField Running = toField ("Running"::String)
  toField Waiting = toField ("Waiting"::String)


instance ToField TypeValue where
  toField TaskType  = toField ("Task"::String)
  toField IssueType = toField ("Issue"::String)
  toField OtherType = toField ("Other"::String)

instance ToField ThreeDValue where
  toField Design        = toField ("Design"::String)
  toField Development   = toField ("Development"::String)
  toField Documentation = toField ("Documentation"::String)
  toField Test          = toField ("Test"::String)

instance ToField ROMMandaysValue where
  toField Days      = toField ("Days"::String)
  toField Weeks     = toField ("Weeks"::String)
  toField Months    = toField ("Months"::String)
  toField Quarters  = toField ("Quarters"::String)

instance ToField ResolutionValue where
  toField Successful  = toField ("Successful"::String)
  toField Aborted     = toField ("Aborted"::String)
  toField Duplicate   = toField ("Duplicate"::String)
  toField Obsolete    = toField ("Obsolete"::String)


instance ToField LinkType where
  toField t         = toField $ show t


saveTicket :: Connection -> T.Text -> T.Text -> IO ()
saveTicket conn ticketId ticketType = do
  let q = (ticketId, ticketType)
  execute conn stmt q
  return ()
  where
  stmt = "insert into tickets (ticketId, ticketType) values (?, ?)"

saveIssue :: Connection -> YtIssue -> IO ()
saveIssue conn issue@(MkYtIssue{..}) = do
  saveTicket conn _ytiIssueId _ytiType

  saveStateTransitions conn _ytiIssueId _ytiStateTransitions

  let q =
        [ toField _ytiIssueId
        , toField _ytiType
        , toField _ytiSummary
        , toField _ytiDescription
        , toField _ytiCreated
        , toField _ytiUpdated
        , toField _ytiProject
        , toField _ytiNumber
        , toField _ytiState
        , toField _ytiWait
        , toField _ytiDueDate
        , toField _ytiROMManday
        , toField _ytiSquad
        , toField _ytiOwner
        , toField _ytiResolution
        , toField _ytiBlockedDays
        ]
  execute conn stmt q

  saveTargetVersions conn issue
  saveLinkedTickets conn _ytiIssueId _ytiLinks
  return ()
  where
  stmt = "insert into ytIssueDetails (\
          \ ytiIssueId, ytiType, ytiSummary, ytiDescription, ytiCreated, ytiUpdatedAt, \
          \ ytiProject, ytiNumber, ytiState, ytiWait, ytiDueDate, ytiROMManDay, ytiSquad, \
          \ ytiOwner, ytiResolution, ytiBlockedDays) \
          \ values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"



saveStateTransitions :: Connection -> T.Text -> StateTransitions -> IO ()
saveStateTransitions conn ticketId trans = do
  let q = case trans of
            STBacklog tb              -> (ticketId, "Backlog"::String, tb, defUTCTime, defUTCTime, defUTCTime, defUTCTime)
            STSelected tb ts          -> (ticketId, "Selected"::String, tb, ts, defUTCTime, defUTCTime, defUTCTime)
            STInProgress tb ts tp     -> (ticketId, "InProgress"::String, tb, ts, tp, defUTCTime, defUTCTime)
            STInReview tb ts tp tr    -> (ticketId, "InReview"::String, tb, ts, tp, tr, defUTCTime)
            STDone tb ts tp tr td     -> (ticketId, "Done"::String, tb, ts, tp, tr, td)
            STIllegalStateTransitions -> (ticketId, "IllegalStateTransitions"::String, defUTCTime, defUTCTime, defUTCTime, defUTCTime, defUTCTime)

  execute conn stmt q
  return ()
  where
  stmt = "insert into stateTransitions (\
          \ stateTransitionId, stateTransitionVal, backlogTime, selectedTime, \
          \ progressStartTime, reviewStartTime, doneTime) \
          \ values (?, ?, ?, ?, ?, ?, ?)"


saveTargetVersions :: Connection -> YtIssue -> IO ()
saveTargetVersions conn issue@(MkYtIssue{..}) = do
  mapM_ saveOne _ytiTargetVersions
  where
  saveOne version = do
    let q = (_ytiIssueId, version)
    execute conn stmt q
    return ()

  stmt = "insert into targetVersion (ytiIssueId, targetVersion) values (?, ?)"


saveLinkedTickets :: Connection -> T.Text -> [(LinkType, T.Text)] -> IO ()
saveLinkedTickets conn ticketId links = do
  mapM_ saveOne links
  where
  saveOne (linkType, linkedTicketId) = do
    let q = (ticketId, linkType, linkedTicketId)
    execute conn stmt q
    return ()

  stmt = "insert into links (ticketId, linkType, linkedTicketId) values (?, ?, ?)"


saveTask :: Connection -> YtTask -> IO ()
saveTask conn task@(MkYtTask{..}) = do
  saveTicket conn _yttTaskId "Task"

  saveStateTransitions conn _yttTaskId _yttStateTransitions

  let q =
        [ toField _yttTaskId
        , toField _yttSummary
        , toField _yttDescription
        , toField _yttCreated
        , toField _yttUpdated
        , toField _yttProject
        , toField _yttNumber
        , toField _yttState
        , toField _yttWait
        , toField _ytt3D
        , toField _yttBlockedDays
        , toField _yttParent
        ]
  execute conn stmt q
  saveLinkedTickets conn _yttTaskId _yttLinks
  saveTaskAssignees conn task
  return ()
  where
  stmt = "insert into ytTaskDetails (\
          \ yttTaskId, yttSummary, yttDescription, yttCreated, yttUpdatedAt, \
          \ yttProject, yttNumber, yttState, yttWait, yttThreeDVal, yttBlockedDays, yttParent) \
          \ values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"


saveTaskAssignees :: Connection -> YtTask -> IO ()
saveTaskAssignees conn task@(MkYtTask{..}) = do
  mapM_ saveOne _yttAssignees
  where
  saveOne assignee = do
    let q = (_yttTaskId, assignee)
    execute conn stmt q
    return ()

  stmt = "insert into TaskAssignee (yttTaskId, developerName) values (?, ?)"


deleteDevProjectData :: Connection -> IO ()
deleteDevProjectData conn =
  mapM_ (\stmt -> execute conn stmt ()) stmts
  where
  stmts =
    [ "delete from links"
    , "delete from TaskAssignee"
    , "delete from targetVersion"
    , "delete from ytIssueDetails"
    , "delete from ytTaskDetails"
    , "delete from stateTransitions"
    , "delete from tickets"
    ]



