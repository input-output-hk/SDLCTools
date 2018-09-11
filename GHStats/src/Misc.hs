module Misc where

import           Options.Applicative
import           Data.Monoid ((<>))

data CliOptions = MkCliOptions {
                  relPath         :: String
                , apiToken        :: String
                , repoName        :: String
                , maxRequestCount :: Int
                , ytAuthorization :: String
                } deriving (Show)

optionParser :: Parser CliOptions
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
        <*> strOption (
              long "Repository Name"
              <> short 'r'
              <> (help "Name of Repository to query"))
        <*> option auto (
              long "MaxRequestCount"
              <> showDefault
              <> short 'm'
              <> (help "Maximum Count of Github Requests to be made"))
        <*> strOption (
              long "yt authorization"
              <> short 'a'
              <> (help "YouTrack authorization"))


-- | Parse command line options
parseCliArgs :: IO CliOptions
parseCliArgs = customExecParser (prefs showHelpOnError) (info optionParser fullDesc)

