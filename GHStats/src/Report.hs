{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module Report where

import           Data.Char (toLower)
import qualified Data.ByteString.Lazy as BS
import           Data.Csv
import qualified Data.Text as T
import           Data.Time.Format

import           Types


makeReport :: (ToNamedRecord a, DefaultOrdered a) => FilePath -> [a] -> IO ()
makeReport filePath values = BS.writeFile filePath $ content
  where
    content = encodeDefaultOrderedByName values

defaultPRCDCSVHeader :: Header
defaultPRCDCSVHeader = header
  [ "PullRequest Number ID"
  , "PullRequest Author"
  , "Authored Date"
  , "Commit Author"
  , "Commit Author Email"
  , "Yt Issue ID in PR Title"
  , "Yt Issue ID in Commit"
  , "Commit ID"
  ]

instance ToNamedRecord PRCDetails where
  toNamedRecord PRCDetails{..} =
    namedRecord [ "PullRequest Number ID"   .= (T.pack . show $ pcdPRNumber)
                , "PullRequest Author"      .= maybe T.empty id pcdPRAuthor
                , "Authored Date"           .= formatDate pcdCoAuthoredDate
                , "Commit Author"           .= maybe T.empty id pcdCoAuthor
                , "Commit Author Email"     .= maybe T.empty id pcdCoAuthorEmail
                , "Yt Issue ID in PR Title" .= maybe T.empty id pcdPRYTIssueId
                , "Yt Issue ID in Commit"   .= maybe T.empty id pcdCommitYTId
                , "Commit ID"               .= pcdCommitId
                ]

instance DefaultOrdered PRCDetails where
  headerOrder _ = defaultPRCDCSVHeader

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
  , "Yt Issue Id Presence"
  , "Yt Type"
  , "Yt State Status"
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
                , "Yt Issue Id Presence"      .= (toLower <$> show paYtIssueIdPresence)
                , "Yt Type"                   .= show paYtType
                , "Yt State Status"           .= show paYtState
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

