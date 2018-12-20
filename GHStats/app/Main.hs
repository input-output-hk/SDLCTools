{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where


import Debug.Trace (trace)

import           Data.Aeson
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Maybe (catMaybes)
import           Data.Monoid ((<>))
import           Data.List.Utils (replace)
import qualified Data.Text as T
import           Network.HTTP.Simple
import           Control.Monad

import           Paths_GHStats
import           PR.Types
import           PR.Extract
import           PR.Report
import           PR.Misc
import           PR.YtIntegration


main :: IO ()
main = do
  (MkCliOptions {..}) <- parseCliArgs
  queryFile <- getDataFileName "extraData/queryPullRequestAll"
  print queryFile
  queryTemplate <- (replace "#repoName#" repoName . filter (\c -> c /= '\n')) <$> readFile queryFile
  let loop :: Int -> T.Text -> [PullRequest] -> IO ()
      loop n cursor acc = do
        putStrLn ("n = " ++ show n)
        let query = replace "###" (T.unpack cursor) $ queryTemplate
        putStrLn query
        respAllCommits  <- runQuery apiToken query
        BL8.appendFile "resp.json" respAllCommits
        let parserPrs = eitherDecode respAllCommits :: Either String GHResponse
        case parserPrs of
          Right (GHResponse (PageInfo{..}, prs)) -> do
            if (hasNextPage && n < maxRequestCount)
              then
                loop (n+1) ("\\\"" <> endCursor <> "\\\"" ) (prs <> acc)
              else
                do
                  let allprs = prs <> acc
                  maybeYtInfos <- forM allprs $ \ PullRequest{..} -> trace ("YT: "++ show (extractIssueId prTitle)) $ (getYtInfo ytAuthorization (either (const Nothing) Just $ extractIssueId prTitle))
                  makeReport "PRAnalysis.csv" $ (catMaybes $ zipWith mkPRAnalysis allprs maybeYtInfos)
                  makeReport "PRCDetails.csv" $ (concat . catMaybes $ mkPRCDetails <$> allprs)
                  putStrLn $ "OK : made " <> show (n+1) <> " calls to Github"
          Left e  -> do
            maybeYtInfos <- forM acc $ \PullRequest{..} -> (getYtInfo ytAuthorization (either (const Nothing) Just $ extractIssueId prTitle) )
            makeReport "PRAnalysis.csv" $ (catMaybes $ zipWith mkPRAnalysis acc maybeYtInfos)
            makeReport "PRCDetails.csv" $ (concat . catMaybes $ mkPRCDetails <$> acc)
            putStrLn $ "oops error occured" <> e

  loop 0 "" []

runQuery :: String -> String -> IO (BL8.ByteString)
runQuery token query = do
  let authorization = "token " ++ token
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

