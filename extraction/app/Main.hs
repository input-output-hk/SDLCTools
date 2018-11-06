{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified  Data.ByteString as BS
import qualified  Data.ByteString.Char8 as BS8
import qualified  Data.ByteString.Lazy as LBS
import qualified  Data.List as L
import qualified  Data.Text as T

import            Network.HTTP.Simple as HTTP
import            Types
import            Data.Aeson

main :: IO ()
main = print 3 -- dummy

allIssuesForProjectJson :: String -> String -> String -> IO () -- LBS.ByteString
allIssuesForProjectJson authorization1 projectName query = do
  req <- HTTP.parseRequest ("https://iohk.myjetbrains.com/youtrack/rest/issue/byproject/" ++ projectName)
  let req' =  HTTP.setRequestHeaders
                [ ("Authorization", BS8.pack authorization1)
                , ("Accept", "application/json")
                ]
              $ req
  resp <- httpBS req'
  let !xmlBs = getResponseBody resp
  --print xmlBs
  print (eitherDecodeStrict xmlBs :: Either String [YttpIssue])
  return ()


