{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ScopedTypeVariables   #-}


module Main where



import            Data.Aeson
import qualified  Data.ByteString.Char8 as B8
import qualified  Data.ByteString.Lazy.Char8 as BL8
import qualified  Data.ByteString.Lazy as LBS

import qualified  Data.List as L
import            Data.Maybe (catMaybes)
import            Data.Monoid ((<>))
import qualified  Data.Text as T
import            Network.HTTP.Simple
import            Control.Monad
import            Data.Time.Calendar
import            Data.Time.Clock
import            Data.Time.Clock.POSIX
import            Data.Time.Format

import            Assignee
import            Config
import            Issue
import            Misc
import            Parser
import            Queries
import            Report.Assignees
import            Types

main :: IO ()
main = do
  (MkCliOptions {..}) <- parseCliArgs
  resp <- runQuery zhToken repoId issueNum
--  print resp
  return ()


main3 :: IO ()
main3 = do
  issues <- getIssues config
--  print config
--  print issues
  --generateAssigneeIssueReport "files/assignees.csv" $ (assigneeMap $ map iGHIssue (onlyInProgressIssues issues))
  return ()
  where
  onlyInProgressIssues issues = L.filter (\i -> let s = zhiState $ iZHIssue $ i in s == InProgress || s == InReview) issues
  config = MkConfig [--("input-output-hk", "cardano-chain", 149791280)
                     ("input-output-hk", "cardano-wallet", 154148239)
--                    ("jcmincke", "zenhub-prj", 152765249)
                    ]
                    "gh-key"
                    "zh-key"





main2 :: IO ()
main2 = do
  json <- getAllIssuesFromGHRepo "GH-key" "jcmincke" "zenhub-prj"
  LBS.writeFile "resp1.json" json
  let (issue :: Either String [GHIssue]) = eitherDecode json
  print issue
  json <- getIssueEventsFromGHRepo "GH-key" "jcmincke" "zenhub-prj" 6
  LBS.writeFile "resp2.json" json
  let (evts :: Either String [Maybe GHIssueEvent]) = eitherDecode json
  print evts


runQuery :: String -> String -> Int -> IO (BL8.ByteString)
runQuery token repo_id issue_number = do
  req' <- parseRequest $ "GET https://api.zenhub.io/p1/repositories/" <> repo_id <> "/issues/" <> show issue_number <> "/events"
  let req = setRequestHeaders [ ("X-Authentication-Token", B8.pack token)
                              ]
          $ req'
  response <- httpLBS req
  putStrLn $ "The status code was: " ++
              show (getResponseStatusCode response)
  let responseBody = getResponseBody response
  return responseBody




