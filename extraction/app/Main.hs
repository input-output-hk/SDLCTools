{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as L
import qualified Data.Text as T
import           Options.Applicative
import           Data.Monoid ((<>))
import           Database.PostgreSQL.Simple

import           Network.HTTP.Simple as HTTP
import           Data.Aeson

import           Database
import           Misc
import           Types

main :: IO ()
main = do
  (MkCliOptions {..}) <- parseCliArgs
  run ytAuthorization projectName

run :: String -> String -> IO ()
run auth pName = do
  issues <- allIssuesForProjectJson auth pName ""
  print issues

doit =  do
  issues <- allIssuesForProjectJson  "your key"
              "CST" -- "DT"
              "sort by: {issue id}"
              --"issue id: DT-278  sort by: {issue id} "
  conn <- connectPostgreSQL "host=localhost port=5432 dbname=sdlc_db user=postgres"
  mapM_ (saveOne conn) issues

  print ("size=", length issues)

  where
  saveOne conn issue = do
    print ("saving", _yttpiIssueId issue)
    print issue
    saveTestProjectIssue conn issue

allIssuesForProjectJson :: String -> String -> String -> IO [YttpIssue] -- LBS.ByteString
allIssuesForProjectJson authorization1 projectName query = do
  req <- HTTP.parseRequest ("https://iohk.myjetbrains.com/youtrack/rest/issue/byproject/" ++ projectName)
  let req' =  (HTTP.setRequestHeaders
                [ ("Authorization", "Bearer " <> (BS8.pack authorization1))
                , ("Accept", "application/json")
                ])

              . (HTTP.setRequestQueryString
                  [ ("max", Just "4000")
                  , ("filter", Just $ BS8.pack query)
                  ])

              $ req
  resp <- httpBS req'
  let !jsonBs = getResponseBody resp
  --BS.writeFile "res.json" jsonBs
  --print xmlBs
  let resE = eitherDecodeStrict jsonBs :: Either String [YttpIssue]
  case resE of
    Right issues -> return issues
    Left e -> error e


