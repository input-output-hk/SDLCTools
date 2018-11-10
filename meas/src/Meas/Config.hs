{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}



module Meas.Config
where

-- import Debug.Trace (trace)

import            Control.Applicative
import            Control.Monad
import qualified  Data.ByteString.Char8 as BS
import            Data.Maybe (fromJust)
import            Data.Yaml


data Config = MkConfig
  { cfgDevCleanupDB  :: Bool
  , cfgDevQueries     :: [(String, String)]
  , cfgTestCleanupDB  :: Bool
  , cfgTestQueries  :: [(String, String)]
  , cfg_db_host :: String
  , cfg_db_port :: Int
  , cfg_db_name :: String
  , cfg_db_user :: String
  , cfg_db_pwd  :: String
  , cfg_yt_key  :: String
  }
  deriving Show

instance FromJSON Config where
    parseJSON (Object o) = do
        devPrj <- o .: "dev"
        cfgDevCleanupDB <- devPrj .: "cleanup_db"
        cfgDevQueries <- devPrj .: "queries"

        testPrj <- o .: "test"
        cfgTestCleanupDB <- testPrj .: "cleanup_db"
        cfgTestQueries <- testPrj .: "queries"

        cfg_db_host <- o .: "db_host"
        cfg_db_port <- o .: "db_port"
        cfg_db_name <- o .: "db_name"
        cfg_db_user <- o .: "db_user"
        cfg_db_pwd <- o .: "db_pwd"
        cfg_yt_key <- o .: "yt_key"

        return MkConfig{..}
    parseJSON _ = mzero

readConfig :: String -> IO Config
readConfig filename = do
  ymlData <- BS.readFile filename
  let mconfig = Data.Yaml.decode ymlData :: Maybe Config
  case mconfig of
    Just cfg -> return cfg
    Nothing -> error "Can't parse Config from YAML/JSON"





