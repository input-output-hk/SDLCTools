{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE RecordWildCards    #-}

module Report where

import qualified Data.ByteString.Lazy as BS
import           Data.Csv
import qualified Data.Text as T
import           Data.Time.Clock
import           Data.Time.Format

import           Types

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
                , "Development Start Time" .= formatDate devStartTime
                , "Review Start Time"      .= formatDate reviewStartTime
                , "Closing Time"           .= maybe "" formatDate closingTime
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
  -- we don't need this in the csv, makes no sense.
 -- , "Latest Commit Time"
  , "Closing Time"
  , "State"
  , "Dev Commits"
  , "Review Commits"
  , "Comments"
  ]

instance ToNamedRecord PRAnalysis where
  toNamedRecord PRAnalysis{..} =
    namedRecord [ "PullRequest Number ID"     .= (T.pack . show $ paPRNumber)
                , "First Commit Time"         .= formatDate paFirstCommitTime
                , "PullRequest Creation Time" .= formatDate paPRCreationTime
               -- , "Latest Commit Time"        .= formatDate paLatestCommitTime
                , "Closing Time"              .= maybe "" formatDate paPRClosingTime
                , "State"                     .= maybe ("Review"::String) (const "Closed") paPRClosingTime
                , "Dev Commits"               .= (length $ fst paDevReviewCommits)
                , "Review Commits"            .= (length $ snd paDevReviewCommits)
                , "Comments"                  .= length paComments
                ]

instance DefaultOrdered PRAnalysis where
  headerOrder _ = defaultPRACSVHeader


formatDate = formatTime defaultTimeLocale "%Y%m%d"
