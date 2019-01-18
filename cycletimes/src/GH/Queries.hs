{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module GH.Queries
where

-- import Debug.Trace (trace)

import            Control.Concurrent

import qualified  Data.ByteString.Char8 as BS8
import qualified  Data.ByteString.Lazy as LBS
import qualified  Data.Map.Strict as M
import            Data.Monoid ((<>))
import qualified  Data.Text as T

import            Network.HTTP.Link.Parser
import            Network.HTTP.Link.Types
import            Network.HTTP.Simple as HTTP
import            Network.HTTP.Types.Header


deriving instance Ord LinkParam

getNextPage :: ResponseHeaders -> Maybe String
getNextPage headers = do
  header <- M.lookup hLink (M.fromList headers)
  links <- parseLinkHeaderBS header
  let uriMap = M.fromList $ do
                Link uri params <- links
                param <- params
                return (param, uri)
  uri <- M.lookup (Rel, "next") uriMap
  return $ show uri
  where
  hLink = "Link"

getSingleIssueFromGHRepo :: String -> String -> String -> Int -> IO LBS.ByteString
getSingleIssueFromGHRepo token user repo issue_number = do
  req' <- parseRequest $ "GET https://api.github.com/repos/" <> user <> "/" <> repo <> "/issues/"
                         <> show issue_number
  let authorization = "token " ++ token
  let req = setRequestHeaders [ ("Authorization", BS8.pack authorization)
                              , ("User-Agent", BS8.pack user)
                              ]
          $ req'
  response <- httpLBS req
  let responseBody = getResponseBody response
  return responseBody


getAllIssuesFromGHRepo :: String -> String -> String -> IO [LBS.ByteString]
getAllIssuesFromGHRepo token user repo = do
  go [] initialRequest
  where
  go jsons url = do
--    print url
    req' <- parseRequest url
    let authorization = "token " ++ token
    let req = setRequestHeaders [ ("Authorization", BS8.pack authorization)
                                , ("User-Agent", BS8.pack user)
                                ]
              $ req'
    response <- httpLBS req
    let responseBody = getResponseBody response
    case getNextPage $ getResponseHeaders response of
      Just nextUrl -> go (responseBody:jsons) nextUrl
      Nothing -> return (responseBody:jsons)

  initialRequest = "GET https://api.github.com/repos/" <> user <> "/" <> repo <> "/issues?state=all"



getIssueEventsFromGHRepo :: String -> String -> String -> Int -> IO LBS.ByteString
getIssueEventsFromGHRepo token user repo issue_number = do
  req' <- parseRequest $ "GET https://api.github.com/repos/" <> user <> "/" <> repo <> "/issues/"
                         <> show issue_number <> "/events"
  let authorization = "token " ++ token
  let req = setRequestHeaders [ ("Authorization", BS8.pack authorization)
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
--  print (repoId, issueNumber, responseBody)
  return responseBody

getSingleIssueEventsFromZHRepo :: String  -> Int -> Int -> IO LBS.ByteString
getSingleIssueEventsFromZHRepo token repoId issueNumber = do
  req' <- parseRequest $ "GET https://api.zenhub.io/p1/repositories/" <> show repoId <> "/issues/"  <> show issueNumber <> "/events/"
  let req = setRequestHeaders [ ("X-Authentication-Token", BS8.pack token)
                              , ("User-Agent", BS8.pack "jcmincke")
                              ]
          $ req'
  response <- httpLBS req
  let responseBody = getResponseBody response
--  print (issueNumber, responseBody)
  return responseBody


getSingleEpicFromZHRepo :: String -> Int -> Int -> IO LBS.ByteString
getSingleEpicFromZHRepo token repoId epicId = do
  threadDelay 1000000

  req' <- parseRequest $ "GET https://api.zenhub.io/p1/repositories/" <> show repoId <> "/epics/"  <> show epicId
  let req = setRequestHeaders [ ("X-Authentication-Token", BS8.pack token)
                              ]
          $ req'
  response <- httpLBS req
  let responseBody = getResponseBody response
--  print (epicId, responseBody)
  return responseBody

getAllEpicsFromZHRepo :: String -> Int -> IO LBS.ByteString
getAllEpicsFromZHRepo token repoId = do
  req' <- parseRequest $ "GET https://api.zenhub.io/p1/repositories/" <> show repoId <> "/epics"
  let req = setRequestHeaders [ ("X-Authentication-Token", BS8.pack token)
                              ]
          $ req'
  response <- httpLBS req
  let responseBody = getResponseBody response
--  print (repoId, responseBody)
  return responseBody


getReleaseFromZHRepo :: String -> Int -> IO LBS.ByteString
getReleaseFromZHRepo token repoId = do
  req' <- parseRequest $ "GET https://api.zenhub.io/p1/repositories/" <> show repoId <> "/reports/releases"
  let req = setRequestHeaders [ ("X-Authentication-Token", BS8.pack token)
                              ]
          $ req'
  response <- httpLBS req
  let responseBody = getResponseBody response
  return responseBody


getIssuesInReleaseFromZH :: String -> T.Text -> IO LBS.ByteString
getIssuesInReleaseFromZH token releaseId = do
  req' <- parseRequest $ "GET https://api.zenhub.io/p1/reports/release/" <> T.unpack releaseId <> "/issues"
  let req = setRequestHeaders [ ("X-Authentication-Token", BS8.pack token)
                              ]
          $ req'
  response <- httpLBS req
  let responseBody = getResponseBody response
  return responseBody




