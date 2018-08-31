{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE RecordWildCards   #-}

module Misc where


import           Data.Monoid ((<>))
import           Options.Applicative as OA

data CliOptions = MkCliOptions {
                  relPath  :: String
                , apiToken :: String
                , repoName :: String
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
        <*> strOption (
              long "Repository Name"
              <> short 'r'
              <> (help "Name of Repository to query"))

-- | Parse command line options
parseCliArgs :: IO CliOptions
parseCliArgs = customExecParser (prefs showHelpOnError) (info optionParser fullDesc)
