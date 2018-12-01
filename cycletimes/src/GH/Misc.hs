module GH.Misc where

import           Options.Applicative
import           Data.Monoid ((<>))


data Options = MkOptions {
  optConfigFile :: String
  }
  deriving (Show)

optionParser :: Parser Options
optionParser =
  MkOptions
        <$> strOption (
              long "config"
              <> value "config.yml"
              <> short 'c'
              <> (help "Config file name"))

-- | Parse command line options
parseCliArgs :: IO Options
parseCliArgs = customExecParser (prefs showHelpOnError) (info optionParser fullDesc)
