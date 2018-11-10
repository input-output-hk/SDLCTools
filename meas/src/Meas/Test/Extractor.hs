{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

module Meas.Test.Extractor
(
  getAllTestIssues
  , allTestIssuesForProjectJson

)
where

--import Debug.Trace (trace)

import            Data.Aeson
import qualified  Data.ByteString.Char8 as BS8
import            Network.HTTP.Simple as HTTP

import            Meas.Test.Parser ()
import            Meas.Test.Types


getAllTestIssues :: String -> [(String, String)] -> IO [(String, [YttpIssue])]
getAllTestIssues authorization projects = do
  mapM getOneproject projects
  where
  getOneproject (projectId, query) = do
    print $ "Extracting Test project: "++ projectId
    issues <- allTestIssuesForProjectJson authorization projectId query
    print ("Found # issues: " ++ (show $ length issues))
    return (projectId, issues)

allTestIssuesForProjectJson :: String -> String -> String -> IO [YttpIssue]
allTestIssuesForProjectJson authorization projectName query = do
  req <- HTTP.parseRequest ("https://iohk.myjetbrains.com/youtrack/rest/issue/byproject/" ++ projectName)
  let req' =  (HTTP.setRequestHeaders
                [ ("Authorization", (BS8.pack authorization))
                , ("Accept", "application/json")
                ])

              . (HTTP.setRequestQueryString
                  [ ("max", Just "4000")
                  , ("filter", Just $ BS8.pack query)
                  ])

              $ req
  resp <- httpBS req'
  let !jsonBs = getResponseBody resp
  let resE = eitherDecodeStrict jsonBs :: Either String [YttpIssue]
  case resE of
    Right issues -> return issues
    Left e -> error e
