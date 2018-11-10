{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}


{-# LANGUAGE TemplateHaskell #-}

module Meas.Dev.Efficiency
(
  getBlockedDays
)
where

-- import Debug.Trace (trace)

import qualified  Data.List as L
import qualified  Data.Set as S

import            Data.Time.Calendar
import            Data.Time.Clock

import Meas.Misc
import Meas.Dev.Types

{- Compute the number of days an issue is not worked on.
The issue is represented by its state transitions and by all its historical changes.
The number of blocked days makes only sense if the issue is Done.

Everytime a change occurs at a given update time t, we consider the issue has been worked on
during the period [day-n, day] where day corresponds to the update time.

-}
getBlockedDays :: Day -> Integer -> StateTransitions -> [(UTCTime, [ValueChange])] -> Integer
getBlockedDays currentDay nDays stateTransitions changes =
  max nbBlockedDays 0
  where
  (tipDay, tdDay) = getWipPeriod currentDay stateTransitions
  wipDays = diffDays tdDay tipDay
  updateDays = L.map (utctDay . fst) changes
  wipUpdateDays = L.filter (\t -> tipDay <= t && t <= tdDay) updateDays
  touchedDays = S.fromList $ do
    day <- wipUpdateDays
    [addDays (-i) day | i <- [0 .. nDays]]
  nbBlockedDays = wipDays - fromIntegral (S.size touchedDays)

getWipPeriod :: Day -> StateTransitions -> (Day, Day)
getWipPeriod _          (STBacklog _)             = (utctDay defUTCTime, utctDay defUTCTime)
getWipPeriod _          (STSelected _ _)          = (utctDay defUTCTime, utctDay defUTCTime)
getWipPeriod currentDay (STInProgress _ _ tip)    = (utctDay tip, currentDay)
getWipPeriod currentDay (STInReview _ _ tip _)    = (utctDay tip, currentDay)
getWipPeriod _          (STDone _ _ tip _ td)     = (utctDay tip, utctDay td)
getWipPeriod _          STIllegalStateTransitions = (utctDay defUTCTime, utctDay defUTCTime)









