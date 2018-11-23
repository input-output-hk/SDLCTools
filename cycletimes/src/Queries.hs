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


getSingleIssueFromGH :: String -> String -> String -> Int -> IO LBS.ByteString
getSingleIssueFromGH token user repo issue_number = do
  req' <- parseRequest $ "GET https://api.github.com/repos/" <> user <> "/" <> repo <> "/issues/"
                         <> show issue_number
  let req = setRequestHeaders [ ("X-Authentication-Token", BS8.pack token)
                              , ("User-Agent", BS8.pack user)
                              ]
          $ req'
  response <- httpLBS req
  let responseBody = getResponseBody response
  return responseBody



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





