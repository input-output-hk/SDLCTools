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
import           Extract
import           Report

main = do
  respPrFstCommit  <- runQuery prQueryFilePathFC
  respPrLastCommit <- runQuery prQueryFilePathLC
  let parsedPrJSON1  = eitherDecode respPrFstCommit  :: Either String PullRequestList
      parsedPrJSON2  = eitherDecode respPrLastCommit :: Either String PullRequestList
  case (parsedPrJSON1, parsedPrJSON2) of
    (Right (PullRequestList prs1), Right (PullRequestList prs2)) |
       (prNumber <$> prs1) == (prNumber <$> prs2)-> do
      let latestCommitTimes = getCommitTime <$> prs2
      makeReport "PRAnalysis.csv" $ zipWith mkPRAnalysis latestCommitTimes prs1
      print "OK"

    _ -> print $ "oops error occured "

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
  return responseBody

-- set to your github personnel access token file path
tokenFilePath :: FilePath
tokenFilePath = "/home/deepak/IOHK-work/github/copytoken"

-- set it your sample graphql query file path for PullRequest
prQueryFilePathLC :: FilePath
prQueryFilePathLC = "/home/deepak/IOHK-work/SDLCTools/GHStats/extraData/queryPullRequestLC"

prQueryFilePathFC :: FilePath
prQueryFilePathFC = "/home/deepak/IOHK-work/SDLCTools/GHStats/extraData/queryPullRequestFC"
