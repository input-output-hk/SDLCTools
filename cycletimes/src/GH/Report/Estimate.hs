{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}


{-# LANGUAGE TemplateHaskell #-}

module GH.Report.Estimate
(
  generateEstimateForIssues
)
where

-- import            Debug.Trace (trace)

import qualified  Data.ByteString.Lazy as LBS
import            Data.Csv as CSV
import            Data.Maybe (mapMaybe)
import qualified  Data.Text as T
import            Data.Time.Clock


import            GH.Types


generateEstimateForIssues :: String -> [Issue] -> IO ()
generateEstimateForIssues filename issues = do
  return ()
  let csvIssuesLBS = CSV.encodeByName defaultEstimateHeader $ mapMaybe getEstimateAndCycleTime issues
  LBS.writeFile filename LBS.empty
  LBS.appendFile filename csvIssuesLBS
  where
  getEstimateAndCycleTime :: Issue -> Maybe EstimateReport
  getEstimateAndCycleTime issue@MkIssue{..} = do
    est <- getEstimate issue
    cy <- getCycleTime issue
    return $ EstimateReport iRepoName (ghiNumber iGHIssue) est cy

  getEstimate MkIssue{..} = zhiEstimate iZHIssue
  getCycleTime MkIssue{..} =
    case iStateTransitions of
      STDone _ ts _ te -> let
        (c :: Float) = fromIntegral $ fromEnum $ diffUTCTime te ts
        (d :: Float) = fromIntegral $ fromEnum nominalDay
        in Just (c / d)
      _ -> Nothing

data EstimateReport = EstimateReport T.Text Int Int Float


defaultEstimateHeader :: Header
defaultEstimateHeader = header
  [ "Issue"
  , "Estimate"
  , "CycleTime"
  ]


instance ToNamedRecord EstimateReport where
    toNamedRecord (EstimateReport repo nb est ct) = namedRecord
        [ "Issue"          .= (T.unpack repo ++ "-" ++ show nb)
        , "Estimate"      .= show est
        , "CycleTime"     .= show ct
        ]




instance CSV.DefaultOrdered EstimateReport where
    headerOrder _ = defaultEstimateHeader


{-}
data StateTransitions =
  STBacklog UTCTime
  |STInProgress UTCTime UTCTime
  |STInReview UTCTime UTCTime UTCTime
  |STDone UTCTime UTCTime UTCTime UTCTime
  |STIllegalStateTransitions
  deriving (Eq, Show, Ord)
-}

{-}

data ActionableIssueReport = ActionableIssueReport Issue



instance ToNamedRecord ActionableIssueReport where
    toNamedRecord (ActionableIssueReport (MkIssue MkGHIssue{..} MkZHIssue{..} _ _ repo transitions)) = namedRecord
        [ "IssueId"         .= (T.unpack repo ++ "-" ++ show ghiNumber)
        , "Backlog"         .= maybe T.empty intToDateText (getBacklogTime transitions)
        , "InProgress"      .= maybe T.empty intToDateText (getInProgressTime transitions)
        , "Review"          .= maybe T.empty intToDateText (getReviewTime transitions)
        , "Done"            .= maybe T.empty intToDateText (getDoneTime transitions)
        , "Repo"            .= repo
        , "Is Epic"         .= show zhiIsEpic
        , "Parent Epic"     .= maybe "" (\n -> T.unpack repo ++ "-" ++ show n) zhiParentEpic
        , "Milestone"       .= maybe "" (\MkGHMilestone{..} -> T.unpack repo ++ "-" ++ show ghmNumber ++ " (" ++ utcTimeToDateString ghmDueDate ++ "):" ++ T.unpack ghmTitle) ghiMilestone
        , "Release"         .= maybe "" (\MkZHRelease{..} -> T.unpack zhrTitle ++ " (" ++ utcTimeToDateString zhrEndDate ++ "): " ++ T.unpack repo) releaseM
        , "Assignee-1"      .= a0
        , "Assignee-2"      .= a1
        , "Assignee-3"      .= a2
        ]
        where
        (a0:a1:a2:_) = (L.map (toRealName . ghuUser) ghiAssignees ++ L.repeat "")
        utcTimeToDateString t = formatTime defaultTimeLocale  "%d-%m-%Y" t
        releaseM =
          case (zhiRelease, zhiRelease) of
            (Just r, _)  -> Just r
            (_, Just r)  -> Just r
            _ -> Nothing




instance CSV.DefaultOrdered ActionableIssueReport where
    headerOrder _ = defaultActionableIssueHeader

-}
