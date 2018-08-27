{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE RecordWildCards    #-}
module Report where

import Data.Csv
import qualified Data.ByteString.Lazy as BS

import Types
import qualified Data.Text as T

defaultPRCSVHeader :: Header
defaultPRCSVHeader = header
  [ "PullRequest Number ID"
  , "Development Start Time"
  , "Review Start Time"
  , "Closing Time"
  ]

instance ToNamedRecord PRCSVData where
  toNamedRecord (PRCSVData ( prnumber, devStartTime, reviewStartTime, closingTime)) =
    namedRecord [ "PullRequest Number ID"  .= prnumber
                , "Development Start Time" .= devStartTime
                , "Review Start Time"      .= reviewStartTime
                , "Closing Time"           .= closingTime
                ]

instance DefaultOrdered PRCSVData where
  headerOrder _ = defaultPRCSVHeader

makeReport :: (ToNamedRecord a, DefaultOrdered a) => FilePath -> [a] -> IO ()
makeReport filePath values = BS.writeFile filePath $ content
  where
    content = encodeDefaultOrderedByName values

defaultPRACSVHeader :: Header
defaultPRACSVHeader = header
  [ "PullRequest Number ID"
  , "First Commit Time"
  , "PullRequest Creation Time"
  , "Latest Commit Time"
  , "Closing Time"
  ]

instance ToNamedRecord PRAnalysis where
  toNamedRecord PRAnalysis{..} =
    namedRecord [ "PullRequest Number ID"     .= (T.pack . show $ paPRNumber)
                , "First Commit Time"         .= paFirstCommitTime
                , "PullRequest Creation Time" .= paPRCreationTime
                , "Latest Commit Time"        .= paLatestCommitTime
                , "Closing Time"              .= paPRClosingTime
                ]

instance DefaultOrdered PRAnalysis where
  headerOrder _ = defaultPRACSVHeader
