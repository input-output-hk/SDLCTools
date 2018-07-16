{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}


{-# LANGUAGE TemplateHaskell #-}

module Meas.Breakdown

where

-- import Debug.Trace (trace)

import            Data.Csv
import qualified  Data.List as L
import qualified  Data.Map.Strict as M
import qualified  Data.Set as S
import qualified  Data.Text as T
import            Data.Time.Calendar
import            Data.Time.Clock

import            Meas.Extract.Efficiency
import            Meas.Extract.Issue
import            Meas.Extract.Misc
import            Meas.Extract.State
import            Meas.Extract.Types
import            Meas.YouTrack.Queries



data Breakdown = MkBreakdown
  { _bdStartDay   :: Day
  , _bdEndDay     :: Day
  , _bdProject    :: T.Text
  , _bdFraction   :: Float
  }



defaultBreakdownHeader :: Header
defaultBreakdownHeader = header
  [ "Start Date"
  , "End Date"
  , "Project"
  , "Fraction"
  ]

instance ToNamedRecord Breakdown where
    toNamedRecord (MkBreakdown {..}) = namedRecord
        [ "Start Date"    .= dayToStdDateText _bdStartDay
        , "End Date"      .= dayToStdDateText _bdEndDay
        , "Project"       .= _bdProject
        , "Fraction"      .= show (_bdFraction * 100.0)
        ]



instance DefaultOrdered Breakdown where
    headerOrder _ = defaultBreakdownHeader



projectBreakDown :: [YtTask] -> Day -> (Day, Day) -> M.Map T.Text Float
projectBreakDown tasks currentDay (startPeriod, endPeriod) =
  if totalDays == 0
  then M.empty
  else ratioPerProject
  where
  ratioPerProject = M.map (\n -> fromIntegral n / totalDays) touchedDaysPerProject
  totalDays = fromIntegral $ M.foldl' (+) 0 touchedDaysPerProject
  touchedDaysPerProject = L.foldl' go M.empty tasks
  go acc (MkYtTask {..}) = let
    touchedDays = touchedDaysInPeriod currentDay nbTouchedDays (startPeriod, endPeriod) _yttStateTransitions _yttChanges
    in M.insertWith (+) _yttProject (S.size touchedDays) acc

--touchedDaysInPeriod :: Day -> Integer -> (Day, Day) -> StateTransitions -> [(Int, [ValueChange])] -> S.Set Day

-- insertWith :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a

nbTouchedDays :: Integer
nbTouchedDays = 0

