{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE RecordWildCards    #-}
module Main where

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Network.HTTP.Simple
import           System.Environment (getArgs)
import           Control.Monad
import           GHC.Generics
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM


data PullRequest = PullRequest {
                   title       :: Maybe Title
                 , isMerged    :: Maybe Bool
                 , isMergeable :: Maybe MergeableStatus
                 } deriving ( Show, Eq, Generic)

data MergeableStatus = MERGEABLE
                     | CONFLICTING
                     | UNKNOWN
 deriving (Show, Eq, Generic,FromJSON)

type Title = T.Text

instance FromJSON PullRequest where
 parseJSON = parseResponse

parseResponse :: Value -> Parser PullRequest
parseResponse = withObject "PullRequest" $ \o -> do
  data_        <- o            .: "data"
  organisation <- data_        .: "organization"
  repository   <- organisation .: "repository"
  pullRequests <- repository   .: "pullRequests"
  edges        <- pullRequests .: "edges"
  pr <- forM edges parsePullrequest
  case pr of
    []      -> mzero
    p : prs -> pure p


parsePullrequest :: Value -> Parser PullRequest
parsePullrequest v = do
    node         <- withObject "PullRequest"  (.: "node") v
    title       <- node .: "title"
    isMerged    <- node .: "merged"
    isMergeable <- node .: "mergeable"
    return $ PullRequest{..}

main = do
  respJson <- pure $ eitherDecode resp :: IO (Either String PullRequest)
  print respJson

--query response I'm trying to parse but not successful yet
resp :: BL8.ByteString
resp = "{\"data\":{\"organization\":{\"repository\":{\"pullRequests\":{\"edges\":[{\"node\":{\"title\":\"[CDEC-432] Switch `JsonLog` & `Mockable` imports from networking to core\",\"merged\":false,\"createdAt\":\"2018-07-18T14:18:59Z\",\"mergeable\":\"MERGEABLE\"}}]}}}}}\n"

runQuery :: IO (BL8.ByteString)
runQuery = do
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

-- set it your sample graphql query file path
queryFilePath :: FilePath
queryFilePath = "/home/deepak/IOHK-work/SDLCTools/GHStats/app/query"

