{-# Language BangPatterns         #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}


module PR.Pr
where



import Debug.Trace (trace)

import           Data.Aeson
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Maybe (catMaybes)
import           Data.Monoid ((<>))
import           Data.List.Utils (replace)
import qualified Data.Text as T
import           Network.HTTP.Simple
--import           System.FilePath.Posix
import           Control.Monad

import           Paths_GHStats
import           PR.Types
import           PR.Extract
import           PR.Report
import           PR.Misc
import           PR.YtIntegration



getAllPRs :: String -> Int -> [String] -> IO  [(String, [PullRequest])]
getAllPRs ghKey maxRequestCount repoNames =
  mapM (\repoName -> do
    prs <- getAllPRsForRepo ghKey maxRequestCount repoName
    return (repoName, prs)
    ) repoNames


getAllPRsForRepo :: String -> Int -> String -> IO  [PullRequest]
getAllPRsForRepo ghKey maxRequestCount repoName = do
  queryFile <- getDataFileName "extraData/queryPullRequestAll"
  queryTemplate <- (replace "#repoName#" repoName . filter (\c -> c /= '\n')) <$> readFile queryFile
  pulRequests <- loop queryTemplate 0 "" []
  return pulRequests

  where
  loop :: String -> Int -> T.Text -> [PullRequest] -> IO [PullRequest]
  loop queryTemplate n cursor acc = do
    putStrLn ("n = " ++ show n)
    let query = replace "###" (T.unpack cursor) $ queryTemplate
    putStrLn query
    respAllCommits  <- runQuery ghKey query
    BL8.appendFile "resp.json" respAllCommits
    let parserPrs = eitherDecode respAllCommits :: Either String GHResponse
    case parserPrs of
      Right (GHResponse (PageInfo{..}, prs)) -> do
        if (hasNextPage && n < maxRequestCount)
          then loop queryTemplate (n+1) ("\\\"" <> endCursor <> "\\\"" ) (prs <> acc)
          else return (prs <> acc)

      Left e  -> do
        putStrLn $ "oops error occured" <> e
        return acc



runQuery :: String -> String -> IO (BL8.ByteString)
runQuery ghKey query = do
  let authorization = "token " ++ ghKey
  req' <- parseRequest "POST https://api.github.com/graphql"
  let req = setRequestHeaders [ ("User-Agent", "Firefox")
                              , ("Authorization", B8.pack authorization)
                              , ("Content-Type", "application/json")
                              ]
          . setRequestBodyLBS (BL8.pack query) $ req'

  response <- httpLBS req
  putStrLn $ "The status code was: " ++
              show (getResponseStatusCode response)
  let responseBody = getResponseBody response
  return responseBody








