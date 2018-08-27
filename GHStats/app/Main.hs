{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import           Data.Aeson
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Network.HTTP.Simple
import           Options.Applicative as OA
import           System.FilePath.Posix

import           Types
import           Extract
import           Report



data CliOptions = MkCliOptions {
  relPath :: String
  , apiToken :: String
  }
  deriving (Show)

optionParser :: OA.Parser CliOptions
optionParser =
  MkCliOptions
        <$> strOption (
              long "relativePath"
              <> short 'p'
              <> (help "Relative Path to Query and Token Directory"))
        <*> strOption (
              long "Api Token"
              <> short 't'
              <> (help "GitHub APi Token"))

-- | Parse command line options
parseCliArgs :: IO CliOptions
parseCliArgs = customExecParser (prefs showHelpOnError) (info optionParser fullDesc)


main :: IO ()
main = do
  (MkCliOptions {..}) <- parseCliArgs

  respAllCommits  <- runQuery apiToken $ (relPath </> "queryPullRequestAll")
  BL8.writeFile "resp.json" respAllCommits
  let parserPrs      = eitherDecode respAllCommits :: Either String PullRequestList
  case parserPrs of
    Right (PullRequestList prs) -> do
      makeReport "PRAnalysis.csv" $ map mkPRAnalysis prs
      putStrLn "OK"

    _ -> putStrLn "oops error occured"

runQuery :: String -> FilePath -> IO (BL8.ByteString)
runQuery token queryFilePath = do
--  authorization <- (("token " ++) . init) <$> readFile tokenFilePath
  let authorization = "token " ++ token

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
--prQueryFilePathLC = "/home/deepak/IOHK-work/SDLCTools/GHStats/extraData/queryPullRequestLC"

prQueryFilePathFC :: FilePath
prQueryFilePathFC = "/home/deepak/IOHK-work/SDLCTools/GHStats/extraData/queryPullRequestFC"
