{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}



module Report where

import qualified Data.ByteString.Lazy as BS
import           Data.Csv
import qualified Data.Text as T
import           Data.Time.Format

import           Types


makeReport :: (ToNamedRecord a, DefaultOrdered a) => FilePath -> [a] -> IO ()
makeReport filePath values = BS.writeFile filePath $ content
  where
    content = encodeDefaultOrderedByName values

defaultPRACSVHeader :: Header
defaultPRACSVHeader = header
  [ "PullRequest Number ID"
  , "YouTrack Issue ID"
  , "First Commit Time"
  , "PullRequest Creation Time"
  , "Closing Time"
  , "State"
  , "Was Merged"
  , "Author"
  , "Dev Commits"
  , "Review Commits"
  , "Comments"
  , "Source Branch Name"
  , "Target Branch Name"
  ]

instance ToNamedRecord PRAnalysis where
  toNamedRecord PRAnalysis{..} =
    namedRecord [ "PullRequest Number ID"     .= (T.pack . show $ paPRNumber)
                , "YouTrack Issue ID"         .= maybe T.empty id paYTIssueId
                , "First Commit Time"         .= formatDate paFirstCommitTime
                , "PullRequest Creation Time" .= formatDate paPRCreationTime
                , "Closing Time"              .= maybe "" formatDate paPRClosingTime
                , "State"                     .= maybe ("Review"::String) (const "Closed") paPRClosingTime
                , "Was Merged"                .= wasMergedSt
                , "Author"                    .= maybe T.empty id paPrAuthor
                , "Dev Commits"               .= (length $ fst paDevReviewCommits)
                , "Review Commits"            .= (length $ snd paDevReviewCommits)
                , "Comments"                  .= length paComments
                , "Source Branch Name"        .= paSourceBranch
                , "Target Branch Name"        .= paTargetBranch
                ]
    where
    wasMergedSt :: String =
      case (paPRClosingTime, paWasMerged) of
      (Nothing, _)    -> ""
      (Just _, True)  -> "true"
      (Just _, False) -> "false"

instance DefaultOrdered PRAnalysis where
  headerOrder _ = defaultPRACSVHeader

formatDate :: Date -> String
formatDate = formatTime defaultTimeLocale "%Y%m%d"

