{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Network.HTTP.Simple
import           System.Environment (getArgs)
import           GHC.Generics
import           Data.Aeson
import           Data.Aeson.Types
import           Types

main = do
  respPr <- runQuery prQueryFilePath
  let parsedPrJSON  = eitherDecode respPr  :: Either String PullRequest
  print parsedPrJSON

runQuery :: FilePath -> IO (BL8.ByteString)
runQuery queryFilePath = do
  authorization <- (("token " ++) . init) <$> readFile tokenFilePath

  req' <- parseRequest "POST https://api.github.com/graphql"
  let req = setRequestHeaders [ ("User-Agent", "Firefox")
                              , ("Authorization", B8.pack authorization)
                              , ("Content-Type", "application/json")
                              ]
          . setRequestBodyFile queryFilePath $ req'

  response <- httpLBS req
  putStrLn $ "The status code was: " ++
              show (getResponseStatusCode response)
  let responseBody = getResponseBody response
  BL8.putStrLn responseBody
  return responseBody

-- set to your github personnel access token file path
tokenFilePath :: FilePath
tokenFilePath = "/home/deepak/IOHK-work/github/copytoken"

-- set it your sample graphql query file path for PullRequest
prQueryFilePath :: FilePath
prQueryFilePath = "/home/deepak/IOHK-work/SDLCTools/GHStats/extraData/queryPullRequest"

