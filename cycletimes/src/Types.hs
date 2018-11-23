{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE RecordWildCards   #-}
module Types where

import           Control.Monad
import           Control.Applicative
import           GHC.Generics
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text
import           Data.Vector      (toList)

import            Data.Time.Calendar
import            Data.Time.Clock
import            Data.Time.Clock.POSIX
import            Data.Time.Format


toUTCTime :: Int -> UTCTime
toUTCTime n = posixSecondsToUTCTime (fromIntegral $ n `div` 1000)


data Event =
  TransferState State State TimeStamp UserId
  | SetState State TimeStamp UserId
  | NotImportant
  deriving (Eq, Show, Generic)

instance FromJSON Event where
  parseJSON = withObject "Event" $ \o -> do
    eventType <- o .: "type" :: Parser Text
    case eventType of
      "transferIssue" -> do
        timeStamp <- toUTCTime <$> (o .: "created_at" :: Parser Int)

        let transferState  = do
              fromPipeLine <-  o .: "from_pipeline"
              fromState <- nameToState <$> (fromPipeLine .: "name" :: Parser Text)
              toPipeLine <-  o .: "to_pipeline"
              toState <- nameToState <$> (toPipeLine .: "name" :: Parser Text)
              pure $ TransferState fromState toState (toUTCTime 1) 1

        let setState  = do
              toPipeLine <-  o .: "to_pipeline"
              toState <- nameToState <$> (toPipeLine .: "name" :: Parser Text)
              pure $ SetState toState (toUTCTime 1) 1

        transferState <|> setState

      _ -> pure NotImportant

data State =
  Backlog
  |InProgress
  |InReview
  |Done
  deriving (Eq, Show, Generic)

type UserId = Int
type TimeStamp = UTCTime

parseEvents :: Value -> Parser [Event]
parseEvents (Array arr) = do
  forM (toList arr) parseJSON :: Parser [Event]

nameToState :: Text -> State
nameToState "New Issues"  = Backlog
nameToState "In Progress" = InProgress
nameToState "Review/QA"   = InReview
nameToState "Closed"      = Done



-- J-C view on events

data Events =
  Created TimeStamp    -- ^ time the issue is created, time associated with Backlog state, from GH
  |Transition State State TimeStamp  -- ^ state transition, from ZenHub
  |Closed TimeStamp  -- ^ time when the issue is closed, from GH
  |ReOpen TimeStamp  -- ^ time when the issue is re-opened, from GH



data Events =
  Transition State State TimeStamp  -- ^ state transition, from ZenHub



data StateTransitions =
  STBacklog UTCTime
  |STInProgress UTCTime UTCTime UTCTime
  |STInReview UTCTime UTCTime UTCTime UTCTime
  |STDone UTCTime UTCTime UTCTime UTCTime UTCTime
  |STIllegalStateTransitions
  deriving (Eq, Show)

{-
Given a list of such event, the goal is to find a algo that will produce a set S of
forward-only state transactions: Backlog -> InProgress -> InReview -> Done.
with some conditions:

Backlog time always = creation time

If a Done state is in the set S, it has the highest time .
Beware of backwards transitions from Done to InProgress, Review.
Transition from Done to Backlog are problematic,

-}



