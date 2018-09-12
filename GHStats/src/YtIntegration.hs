{-# Language BangPatterns         #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module YtIntegration where

import qualified Data.Text as T
import Types

import Meas


getYtInfo :: YtAuthorization -> Maybe YtIssueId -> IO (Maybe YtInfo)
getYtInfo _ Nothing = return Nothing
getYtInfo auth (Just issueId) = do
  putStrLn $ T.unpack issueId
  (tasks, issues) <- getSingleIssue auth (T.unpack issueId)
  case (tasks, issues) of
    ([task], [])  -> return . pure $ YtInfo TaskType  (_yttState task)
    ([], [issue]) -> return . pure $ YtInfo IssueType (_ytiState issue)
    _             -> do putStrLn ("Issue: " ++ T.unpack issueId ++ " not found")
                        return Nothing
