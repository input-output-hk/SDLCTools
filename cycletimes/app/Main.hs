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

import            Network.HTTP.Link.Parser as P

import            GH.Assignee
import            GH.Config
import            GH.Issue
import            GH.Misc
import            GH.Parser
import            GH.Queries
import            GH.Report.Assignees
import            GH.Report.Actionable
import            GH.Types

-- parseLinkHeader' :: Text -> Either String [Link]
l = parseLinkHeader' "<https://api.github.com/repositories/154148239/issues?state=all&page=4>; rel=\"prev\", <https://api.github.com/repositories/154148239/issues?state=all&page=4>; rel=\"last\", <https://api.github.com/repositories/154148239/issues?state=all&page=1>; rel=\"first\""


main1 :: IO ()
main1 = do
  (MkCliOptions {..}) <- parseCliArgs
  resp <- runQuery zhToken repoId issueNum
--  print resp
  return ()


main :: IO ()
main = do
  issues <- getIssues config
--  print config
  print issues
  generateAssigneeIssueReport "files/assignements.csv" $ (assigneeMap $ map iGHIssue (onlyInProgressIssues issues))
  generateIssueAssigneeReport "files/assignees.csv" $ (issueMap $ map iGHIssue (onlyInProgressIssues issues))
  generateActionableForIssues "files/actionable.csv" issues
  return ()
  where
  onlyInProgressIssues issues = L.filter (\i -> let
                  s = (zhiState $ iZHIssue i)
                  isPR = ghiIsPR $ iGHIssue i
                  in (s == InProgress || s == InReview) && not isPR) issues
  config = MkConfig [ -- ("input-output-hk", "cardano-wallet", 154148239)
  --                   ("input-output-hk", "ouroboros-network", 149481615)
                     ("input-output-hk", "cardano-chain", 149791280)
   --                  ("input-output-hk", "fm-ledger-rules", 150113380)
  --                   ("input-output-hk", "cardano-shell", 154114906)

--                    ("jcmincke", "zenhub-prj", 152765249)
                    ]
                    "key"
                    "key"


main4 :: IO ()
main4 = do
  mapM_ (\i -> do
            json <- getSingleIssueFromGHRepo "gh-key" "input-output-hk"  "cardano-wallet" 24
            print "========="
            print i
            print "--------"
            print json
            ) [1..62]


--main2 :: IO ()
--main2 = do
--  json <- getAllIssuesFromGHRepo "GH-key" "jcmincke" "zenhub-prj"
--  LBS.writeFile "resp1.json" json
--  let (issue :: Either String [GHIssue]) = eitherDecode json
--  print issue
--  json <- getIssueEventsFromGHRepo "GH-key" "jcmincke" "zenhub-prj" 6
--  LBS.writeFile "resp2.json" json
--  let (evts :: Either String [Maybe GHIssueEvent]) = eitherDecode json
--  print evts


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




