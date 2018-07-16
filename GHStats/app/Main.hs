{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
module Main where

import qualified Data.ByteString.Char8 as L8
import qualified Data.ByteString.Lazy.Char8 as LL8
import           Network.HTTP.Simple
import           System.Environment (getArgs)

main :: IO ()
main = do
  -- tokenFileName <- head <$> getArgs
  authorization <- (("token " ++) . init) <$> readFile tokenFilePath
  print  authorization

  req' <- parseRequest "https://api.github.com/graphql"
  let req = setRequestHeaders [ ("User-Agent", "Firefox")
                              , ("Authorization", L8.pack authorization)
                              , ("Content-Type", "application/json") ]
          . setRequestMethod "POST"
          . setRequestBodyLBS ( LL8.pack query) $ req'  --if you want to see a positive feedback uncomment this line and comment next line
  --        . setRequestBodyFile queryFilePath $ req'  -- if you want to try query from file commentout this and add comment above

  response <- httpLBS req
  putStrLn $ "The status code was: " ++
              show (getResponseStatusCode response)
  LL8.putStrLn $ getResponseBody response

query :: String
query = "{ \"query\" : \"{ viewer { login }}\"}"

-- set to your github personnel access token file path
tokenFilePath :: FilePath
tokenFilePath = "/home/deepak/IOHK-work/github/copytoken"

-- set it your sample graphql query file path
queryFilePath :: FilePath
queryFilePath = "/home/deepak/IOHK-work/SDLCTools/GHStats/app/query"
