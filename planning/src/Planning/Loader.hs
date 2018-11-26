{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}



module Planning.Loader

where

--import            Control.Monad.Fail
import qualified  Data.List as L
import qualified  Data.Map.Strict as M
import            Data.Maybe (mapMaybe)
import            Data.Random
import            Data.Random.Source
import            Data.Random.Internal.Source
import            Data.Random.Distribution.Beta (Beta, beta)
import            Data.Ratio
import qualified  Data.Text as T

import            Test.QuickCheck.Arbitrary
import            Test.QuickCheck.Gen (Gen, shuffle, frequency, choose, vectorOf, generate)

import Data.Word

import            Gtsim.Types
import            Gtsim.Simulator as S
import            Meas.Dev.Types

import System.IO.Unsafe (unsafePerformIO)

betaGen :: Gen Float
betaGen = do
  frequency $ L.zip density slots
  where
  density =
    [ 6734, 16340, 22042, 24026, 24346, 22830, 19992, 17248, 13956, 10810
      , 7944, 5592, 3750, 2290, 1230, 0562, 234, 70, 2, 2
    ]
  breaks =
    [ 0.00, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50, 0.55, 0.60
    , 0.65, 0.70, 0.75, 0.80, 0.85, 0.90, 0.95, 1.00
    ]
  slots = L.zipWith (\min max -> choose (min, max)) breaks $ L.tail breaks



iohkResources =
--  [ MkResource "Squad 1"          (stdResourceEfficiency 1.2) parTaskGen
--  , MkResource "Squad 2"          (stdResourceEfficiency 1.2) parTaskGen
--  , MkResource "Squad 3"          (stdResourceEfficiency 1.2) parTaskGen
--  , MkResource "Squad 4"          (stdResourceEfficiency 1.2) parTaskGen
--  , MkResource "New Wallet Squad" (stdResourceEfficiency 1.2) parTaskGen
--  , MkResource "Core Squad"       (stdResourceEfficiency 1.2) parTaskGen
--  , MkResource "Network Squad"    (stdResourceEfficiency 1.2) parTaskGen
--  ]
  [ MkResource "Squad 1"          (stdResourceEfficiency 2.5) parTaskGen
  , MkResource "Squad 2"          (stdResourceEfficiency 1.0) parTaskGen
  , MkResource "Squad 3"          (stdResourceEfficiency 2.5) parTaskGen
  , MkResource "Squad 4"          (stdResourceEfficiency 2.5) parTaskGen
  , MkResource "New Wallet Squad" (stdResourceEfficiency 5.0) parTaskGen
  , MkResource "Core Squad"       (stdResourceEfficiency 5.0) parTaskGen
  , MkResource "Network Squad"    (stdResourceEfficiency 3.0) parTaskGen
  , MkResource "Network Squad"    (stdResourceEfficiency 3.0) parTaskGen
  , MkResource "Dev"              (stdResourceEfficiency 1.0) parTaskGen
  ]
  where
  parTaskGen = choose (1, 2)

iohkResourceMap = M.fromList $ map (\r -> (rId r, r)) iohkResources


resourceEfficiency :: [(Int, Float)] -> (Int -> ManDaysPerDay)
resourceEfficiency effs = go
  where
  go 0 = go 1
  go i = (M.fromList $ map (\(i, e) -> (i, toRational e)) effs) M.! i

stdResourceEfficiency :: Float -> (Int -> ManDaysPerDay)
stdResourceEfficiency baseEff =
  resourceEfficiency effs
  where
  effs = zipWith (\i f -> (i, let b = fromIntegral i * (1+f) in baseEff / b)) [1..] switchPercent
  --switchPercent = L.take 10 $ L.repeat 0.0
  switchPercent = [0, 0.05, 0.10, 0.15, 0.30, 0.45, 0.7, 1, 1.5, 2.0, 2.0, 3.0]


initialize :: [Resource] -> [YtIssue] -> (SimState, ProblemDefinition)
initialize resources ytIssues =
  S.initialize tasks resources dependencies runningAssignmentGen
  where
  tasks = mapMaybe mkTask ytIssues
  dependencies = M.fromList $ map (\t -> (tId t, [])) tasks
  runningAssignmentGen = pure True -- frequency [(40, pure True), (60, pure False)]


mkTask :: YtIssue -> Maybe Task
mkTask (MkYtIssue {..}) = do
  romMandays <- _ytiROMManday
  let tId = T.unpack _ytiIssueId
  let tProjectId = T.unpack _ytiProject
  let tManDaysGen = romMandaysToGen romMandays
  let tCanBeDoneBy =
        if L.null _ytiPotentialSquad
          then ["Dev"]
          else map T.unpack _ytiPotentialSquad
  if L.null tCanBeDoneBy
    then fail ""
    else return $ MkTask {..}





romMandaysToGen :: ROMMandaysValue -> Gen ManDays
romMandaysToGen Days      = mandaysGen 1.0  4.0    >>= (return . toRational)
romMandaysToGen Weeks     = mandaysGen 3.0  13.0   >>= (return . toRational)
romMandaysToGen Months    = mandaysGen 12.0 45.0   >>= (return . toRational)
romMandaysToGen Quarters  = mandaysGen 40.0 100.0  >>= (return . toRational)

mandaysGen min max = do
  x <- betaGen
  return $ min + (max - min) * x

