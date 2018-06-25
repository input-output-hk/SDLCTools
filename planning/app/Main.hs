{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}


module Main where

--import Lib

import Data.Ratio

import Data.Decimal
import Debug.Trace(trace)
import Control.Monad.State
import Control.Monad
import Data.Monoid

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.List as L



--import qualified  Data.ByteString.Lazy as LBS
--import            Data.Time.Clock


import            Gtsim.Simulator

import            Meas.Extract.Types
import            Meas.Extractor
import            Meas.Extract.Misc
import            Meas.Extract.Report

import            Gtsim.Types
import qualified  Gtsim.Invariants as I

import            Planning.Loader as L
import            Planning.Processing


main :: IO ()
main = do
  (MkOptions {..}) <- parseCliArgs
  run optAuth


run :: String -> IO ()
run authorization = do
  res <- getAllNoHistory authorization
          [
           ("CBR", "Type:{User Story} #Bug State: Backlog #Selected #Planning sort by: {issue id} asc")
--         ("DDW", "issue id: DDW-10")
--         ("DDW", "Type:Task #{User Story} #Bug sort by: {issue id} asc")

         --   ("DEVOPS", "Type:Task  #Bug sort by: {issue id} asc")
         --   , ("TSD", "Type:Task #{User Story} #Bug sort by: {issue id} asc")
         --   , ("PB", "Type:Task #{User Story} #Bug sort by: {issue id} asc")
         --   , ("DDW", "Type:Task #{User Story} #Bug sort by: {issue id} asc")
         --   , ("CDEC", "Type:Task #{User Story} #Bug sort by: {issue id} asc")
         --   , ("CBR", "Type:Task #{User Story} #Bug sort by: {issue id} asc")
         --   , ("CHW", "Type:Task #{User Story} #Bug sort by: {issue id} asc")
         --   , ("CO", "Type:Task #{User Story} #Bug sort by: {issue id} asc")
          ]

  --
  let issues = concat $ map (\(_, _, issue) -> issue) res

  --mapM print issues


  putStrLn "simulation"
  let (simState, problemDefinition) = L.initialize iohkResources issues

  let ts = (pdTasksPerResource problemDefinition)

--  print("------------------------------------")
--  print ts
  print ("------------------------------------")
  print $ M.size (pdTasks problemDefinition)
  print ("------------------------------------")

  print $ pdTasks problemDefinition
  let simRes = runSimulations
                --I.simStateInv I.finalSimStateAdditionalInv
                (const []) (\_ _ -> [])
                problemDefinition 1 simState
                getFinalEndTime (\xs -> map (\x-> (fromRational x)::Float) xs)
                42 100

  putStrLn "results"
  mapM print simRes
  return ()

