{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import           Data.Aeson
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as T

import           Network.HTTP.Simple
import           Options.Applicative as OA
import           System.FilePath.Posix
import           Data.Monoid ((<>))
import           Types
import           Extract
import           Report

data CliOptions = MkCliOptions {
                  relPath :: String
                , apiToken :: String
                } deriving (Show)

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
  queryPart1 <- BL8.readFile (relPath </> "queryPart1")
  queryPart2 <- BL8.readFile (relPath </> "queryPart2")
  let
    loop n cursor acc =  do
      respAllCommits  <- runQuery apiToken (BL8.init queryPart1 <> cursor <> queryPart2)
      BL8.appendFile "resp.json" respAllCommits
      let parserPrs      = eitherDecode respAllCommits :: Either String GHResponse
      case parserPrs of
        Right (GHResponse (PageInfo{..}, prs)) -> do
          if hasPreviousPage
            then
              loop (n+1) (" , before : \\\"" <> (BL8.pack . T.unpack $ endCursor) <> "\\\" ") ((mkPRAnalysis <$> prs) <> acc)
            else
              do
                makeReport "PRAnalysis.csv" $ (mkPRAnalysis <$> prs) <> acc
                putStrLn $ "OK : made " <> show n <> " calls to Github"
        Left e  -> do
          makeReport "PRAnalysis.csv" acc
          putStrLn $ "oops error occured" <> e
  loop 0 "" []

runQuery :: String -> BL8.ByteString -> IO (BL8.ByteString)
runQuery token query = do
  let authorization = "token " ++ token
  req' <- parseRequest "POST https://api.github.com/graphql"
  let req = setRequestHeaders [ ("User-Agent", "Firefox")
                              , ("Authorization", B8.pack authorization)
                              , ("Content-Type", "application/json")
                              ]
          . setRequestBodyLBS query $ req'

  response <- httpLBS req
  putStrLn $ "The status code was: " ++
              show (getResponseStatusCode response)
  let responseBody = getResponseBody response
  return responseBody

