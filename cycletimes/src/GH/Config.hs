{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}



module GH.Config
(
  Config (..)

  , readConfig

)
where

-- import Debug.Trace (trace)

import            Control.Monad
import qualified  Data.ByteString.Char8 as BS
import            Data.Yaml



data Config = MkConfig
  { cfg_Repos   :: [(String, String, Int)]
  , cfg_gh_key  :: String
  , cfg_zh_key  :: String
  , cfg_pr      :: Bool     -- ^ Do Pull Request analysis
  }
  deriving Show

instance FromJSON Config where
    parseJSON (Object o) = do
        cfg_Repos   <- o .: "repos"
        cfg_gh_key  <- o .: "gh_key"
        cfg_zh_key  <- o .: "zh_key"
        cfg_pr      <- o .: "pr"

        return MkConfig{..}
    parseJSON _ = mzero

readConfig :: String -> IO Config
readConfig filename = do
  ymlData <- BS.readFile filename
  let mconfig = Data.Yaml.decode ymlData :: Maybe Config
  case mconfig of
    Just cfg -> return cfg
    Nothing -> error "Can't parse Config from YAML/JSON"





