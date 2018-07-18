{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}


module Main where

import qualified Data.ByteString.Char8 as L8
import qualified Data.ByteString.Lazy.Char8 as LL8
import           Network.HTTP.Simple
import           System.Environment (getArgs)
import           Control.Monad
import           GHC.Generics
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM



data PullRequest = PullRequest {
                   title       :: Maybe Title    -- J-C are you sure Maybe is needed ? try with resp1 and see what happens
                 , isMerged    :: Maybe Bool
                 , isMergeable :: Maybe MergeableStatus
                 } deriving ( Show, Eq, Generic)

data MergeableStatus = Mergeable | NotMergeable
 deriving (Show, Eq, Generic)


type Title = T.Text

samplePullReq = PullRequest (Just "someTitle") (Just True) (Just Mergeable)

main = do
  let (d :: Either String PullRequest) = eitherDecode resp1
  print d


instance FromJSON MergeableStatus where
 parseJSON = withText "MergeableStatus" $ \t ->
  case t of
    "MERGEABLE" -> return Mergeable
    -- Check whether NOTMERGEABLE is a legal value
    "NOTMERGEABLE" -> return NotMergeable
    _ -> fail ("Bad Mergeable Status: " ++ T.unpack t)

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
  return (head pr)


parsePullrequest :: Value -> Parser PullRequest
parsePullrequest v = do
    node         <- withObject "PullRequest"  (.: "node") v
    title_       <- node .: "title"
    isMerged_    <- node .: "merged"
    isMergeable_ <- node .: "mergeable"
    return $ PullRequest title_ isMerged_ isMergeable_


--query response I'm trying to parse but not successful yet
resp :: LL8.ByteString
resp = "{\"data\":{\"organization\":{\"repository\":{\"pullRequests\":{\"edges\":[{\"node\":{\"title\":\"[CDEC-432] Switch `JsonLog` & `Mockable` imports from networking to core\",\"merged\":false,\"createdAt\":\"2018-07-18T14:18:59Z\",\"mergeable\":\"MERGEABLE\"}}]}}}}}\n"

resp1 :: LL8.ByteString
resp1 = "{\"data\":{\"organization\":{\"repository\":{\"pullRequests\":{\"edges\":[{\"node\":{\"title\":true,\"merged\":false,\"createdAt\":\"2018-07-18T14:18:59Z\",\"mergeable\":\"MERGEABLE\"}}]}}}}}\n"


runQuery :: IO (LL8.ByteString)
runQuery = do
  -- tokenFileName <- head <$> getArgs
  authorization <- (("token " ++) . init) <$> readFile tokenFilePath

  req' <- parseRequest "POST https://api.github.com/graphql"
  let req = setRequestHeaders [ ("User-Agent", "Firefox")
                              , ("Authorization", L8.pack authorization)
                              , ("Content-Type", "application/json")
                              ]
          . setRequestBodyFile queryFilePath $ req'

  response <- httpLBS req
  putStrLn $ "The status code was: " ++
              show (getResponseStatusCode response)
  let responseBody = getResponseBody response
  --LL8.putStrLn responseBody
  return responseBody

query :: String
query = "{ \"query\" : \"{ viewer { login }}\"}"

-- set to your github personnel access token file path
tokenFilePath :: FilePath
tokenFilePath = "/home/deepak/IOHK-work/github/copytoken"

-- set it your sample graphql query file path
queryFilePath :: FilePath
queryFilePath = "/home/deepak/IOHK-work/SDLCTools/GHStats/app/query"





