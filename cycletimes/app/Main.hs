{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where



import           Data.Aeson
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Maybe (catMaybes)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Network.HTTP.Simple
import           Control.Monad


import           Misc
import           Types

main :: IO ()
main = do
  (MkCliOptions {..}) <- parseCliArgs
  resp <- runQuery zhToken repoId issueNum
  print resp

runQuery :: String -> String -> Int -> IO (BL8.ByteString)
runQuery token repo_id issue_number = do
  req' <- parseRequest $ "GET https://api.zenhub.io/p1/repositories/" <> repo_id <> "/issues/" <> show issue_number <> "/events"
  let req = setRequestHeaders [ ("X-Authentication-Token", B8.pack token)
                              ]
          $ req'
  response <- httpLBS req
  putStrLn $ "The status code was: " ++
              show (getResponseStatusCode response)
  let responseBody = getResponseBody response
  return responseBody


key =" 0224a838e6110e6971df72feb78200ab68b1924406ec70b1b14159c86de186fc95b3961f155fa506"

repo = "149791280"


