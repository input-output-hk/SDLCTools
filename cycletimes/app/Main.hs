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

import            Data.Maybe (catMaybes)
import            Data.Monoid ((<>))
import qualified  Data.Text as T
import            Network.HTTP.Simple
import            Control.Monad
import            Data.Time.Calendar
import            Data.Time.Clock
import            Data.Time.Clock.POSIX
import            Data.Time.Format


import            Misc
import            Parser
import            Queries
import            Types

main :: IO ()
main = do
  (MkCliOptions {..}) <- parseCliArgs
  resp <- runQuery zhToken repoId issueNum
  print resp

-- curl -H 'X-Authentication-Token: 99e0c835ae8b1f111226d1f88dc130608bf36891'
-- https://api.github.com/repos/jcmincke/zenhub-prj/issues/6 > gh-one-issue-jc.json

-- main2
d::UTCTime
d = parseTimeOrError  True defaultTimeLocale  "%Y-%m-%dT%TZ" "2018-11-22T22:34:14Z"

-- "2018-11-22T10:32:29Z"

main2 :: IO ()
main2 = do
  json <- getSingleIssueFromGH "99e0c835ae8b1f111226d1f88dc130608bf36891" "jcmincke" "zenhub-prj" 6
  LBS.writeFile "resp.json" json
  let (issue :: Either String GHIssue) = eitherDecode json
  print issue

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


key =" 0224a838e6110e6971df72feb78200ab68b1924406ec70b1b14159c86de186fc95b3961f155fa506"

repo = "149791280"


