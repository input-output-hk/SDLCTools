{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}


{-# LANGUAGE TemplateHaskell #-}

module Meas.YouTrack.Queries
where

-- import Debug.Trace (trace)


import qualified  Data.ByteString as BS
import qualified  Data.ByteString.Char8 as BS8
import qualified  Data.ByteString.Lazy as LBS
import qualified  Data.List as L
import qualified  Data.Text as T

import            Network.HTTP.Simple as HTTP

import            Text.XML.JSON.StreamingXmlToJson



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


changesForIssueJson :: String -> T.Text -> IO LBS.ByteString
changesForIssueJson authorization issueId = do
  req <- HTTP.parseRequest ("https://iohk.myjetbrains.com/youtrack/rest/issue/"++T.unpack issueId++"/changes")
  let req' = HTTP.setRequestHeaders
                [("Authorization", BS8.pack authorization)]
                req
  resp <- httpBS req'
  let xmlBs = getResponseBody resp
  let st = L.concat $ xmlStreamToJSON (BS8.unpack xmlBs)
  let jsonBs = LBS.fromStrict $ BS8.pack $ st

  return jsonBs



singleIssueJson :: String -> String -> IO LBS.ByteString
singleIssueJson authorization issueId = do
  req <- HTTP.parseRequest ("https://iohk.myjetbrains.com/youtrack/rest/issue/" ++ issueId)
  let req' =  ((HTTP.setRequestHeaders
                [("Authorization", BS8.pack authorization)])
              ) req
  resp <- httpBS req'
  let xmlBs = getResponseBody resp
  BS.writeFile "tall" xmlBs
  let st = L.concat $ xmlStreamToJSON (BS8.unpack xmlBs)
  let jsonBs = LBS.fromStrict $ BS8.pack $ st
  return jsonBs