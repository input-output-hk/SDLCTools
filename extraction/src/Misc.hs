module Misc where

import           Options.Applicative
import           Data.Monoid ((<>))

data CliOptions = MkCliOptions {
                  ytAuthorization :: String
                , projectName     :: String
                } deriving (Show)

optionParser :: Parser CliOptions
optionParser =
  MkCliOptions
        <$> strOption (
              long "yt authorization"
              <> short 'a'
              <> (help "YouTrack authorization"))
        <*> strOption (
              long "Project Name"
              <> short 'p'
              <> (help "Name of Project to query from YouTrack"))


-- | Parse command line options
parseCliArgs :: IO CliOptions
parseCliArgs = customExecParser (prefs showHelpOnError) (info optionParser fullDesc)

