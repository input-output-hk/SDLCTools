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

import           Network.HTTP.Simple as HTTP
import           Types
import           Data.Aeson
import           Misc

main :: IO ()
main = do
  (MkCliOptions {..}) <- parseCliArgs
  run ytAuthorization projectName

run :: String -> String -> IO ()
run auth pName = allIssuesForProjectJson auth pName ""

allIssuesForProjectJson :: String -> String -> String -> IO () -- LBS.ByteString
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
  print (eitherDecodeStrict jsonBs :: Either String [YttpIssue])
  return ()


