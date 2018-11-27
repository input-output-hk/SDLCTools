module GH.Misc where

import           Options.Applicative
import           Data.Monoid ((<>))

data CliOptions = MkCliOptions {
                  zhToken  :: String
                , repoId   :: String
                , issueNum :: Int
                } deriving (Show)

optionParser :: Parser CliOptions
optionParser =
  MkCliOptions
        <$> strOption (
              long "Api Token"
              <> short 't'
              <> (help "ZenHub APi Token"))
        <*> strOption (
              long "Repository Id"
              <> short 'r'
              <> (help "Id of Repository to query"))
        <*> option auto (
              long "Issue Number"
              <> showDefault
              <> short 'i'
              <> (help "Issue Number of repo to query"))


-- | Parse command line options
parseCliArgs :: IO CliOptions
parseCliArgs = customExecParser (prefs showHelpOnError) (info optionParser fullDesc)

