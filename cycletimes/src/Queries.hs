{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}



module Queries
where

-- import Debug.Trace (trace)


import qualified  Data.ByteString as BS
import qualified  Data.ByteString.Char8 as BS8
import qualified  Data.ByteString.Lazy as LBS
import qualified  Data.List as L
import            Data.Monoid ((<>))
import qualified  Data.Text as T

import            Network.HTTP.Simple as HTTP


{-
curl -H 'X-Authentication-Token: 99e0c835ae8b1f111226d1f88dc130608bf36891'
https://api.github.com/repos/jcmincke/zenhub-prj/issues/6 > gh-one-issue-jc.json
-}


getSingleIssueFromGHRepo :: String -> String -> String -> Int -> IO LBS.ByteString
getSingleIssueFromGHRepo token user repo issue_number = do
  req' <- parseRequest $ "GET https://api.github.com/repos/" <> user <> "/" <> repo <> "/issues/"
                         <> show issue_number
  let req = setRequestHeaders [ ("X-Authentication-Token", BS8.pack token)
                              , ("User-Agent", BS8.pack user)
                              ]
          $ req'
  response <- httpLBS req
  let responseBody = getResponseBody response
  return responseBody

getAllIssuesFromGHRepo :: String -> String -> String -> IO LBS.ByteString
getAllIssuesFromGHRepo token user repo = do
  req' <- parseRequest $ "GET https://api.github.com/repos/" <> user <> "/" <> repo <> "/issues"
  let req = setRequestHeaders [ ("X-Authentication-Token", BS8.pack token)
                              , ("User-Agent", BS8.pack user)
                              ]
          $ req'
  response <- httpLBS req
  let responseBody = getResponseBody response
  return responseBody



getIssueEventsFromGHRepo :: String -> String -> String -> Int -> IO LBS.ByteString
getIssueEventsFromGHRepo token user repo issue_number = do
  req' <- parseRequest $ "GET https://api.github.com/repos/" <> user <> "/" <> repo <> "/issues/"
                         <> show issue_number <> "/events"
  let req = setRequestHeaders [ ("X-Authentication-Token", BS8.pack token)
                              , ("User-Agent", BS8.pack user)
                              ]
          $ req'
  response <- httpLBS req
  let responseBody = getResponseBody response
  return responseBody


getSingleIssueFromZHRepo :: String  -> Int -> Int -> IO LBS.ByteString
getSingleIssueFromZHRepo token repoId issueNumber = do
  req' <- parseRequest $ "GET https://api.zenhub.io/p1/repositories/" <> show repoId <> "/issues/"  <> show issueNumber
  let req = setRequestHeaders [ ("X-Authentication-Token", BS8.pack token)
                              ]
          $ req'
  response <- httpLBS req
  let responseBody = getResponseBody response
  print (repoId, issueNumber, responseBody)
  return responseBody

getSingleIssueEventsFromZHRepo :: String  -> Int -> Int -> IO LBS.ByteString
getSingleIssueEventsFromZHRepo token repoId issueNumber = do
  req' <- parseRequest $ "GET https://api.zenhub.io/p1/repositories/" <> show repoId <> "/issues/"  <> show issueNumber <> "/events/"
  let req = setRequestHeaders [ ("X-Authentication-Token", BS8.pack token)
                              ]
          $ req'
  response <- httpLBS req
  let responseBody = getResponseBody response
  return responseBody

--https://api.github.com/repos/jcmincke/zenhub-prj/issues/6/events > gh-events-jc.json

{-
allIssuesForProjectJson :: String -> String -> String -> IO LBS.ByteString
allIssuesForProjectJson authorization projectName query = do
  req <- HTTP.parseRequest ("https://iohk.myjetbrains.com/youtrack/rest/issue/byproject/" ++ projectName)
  let req' =  ((HTTP.setRequestHeaders
                [("Authorization", BS8.pack authorization)])
              . (HTTP.setRequestQueryString
                  [ ("max", Just "4000")
                  , ("filter", Just $ BS8.pack query)
                  ])
              ) req
  resp <- httpBS req'
  let xmlBs = getResponseBody resp
  let st = L.concat $ xmlStreamToJSON (BS8.unpack xmlBs)
  let jsonBs = LBS.fromStrict $ BS8.pack $ st
  return jsonBs


-}





