{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}


{-# LANGUAGE TemplateHaskell #-}

module GH.Report.Release
(
  generateReleaseReport
)
where

import qualified  Data.Text as T

import            GH.Types

generateReleaseReport :: String -> [Issue] -> IO ()
generateReleaseReport releaseDiscrepancyFilename  issues = do
  writeFile releaseDiscrepancyFilename "Wrong release\n"
  mapM_ (go releaseDiscrepancyFilename) issues

  where
  go filename MkIssue{..} = do
    let MkZHIssue{..} = iZHIssue
    let MkGHIssue{..} = iGHIssue

    case (zhiRelease, zhiInheritedRelease) of
      (Just r, Just ir) | r /= ir -> do
        appendFile filename "\n"
        mapM_ (\s -> do
            appendFile filename s
            appendFile filename "\n"
            ) $ ["Issue: " ++ show ghiNumber ++ " " ++ T.unpack ghiTitle
                , T.unpack $ zhrTitle r
                , T.unpack $ zhrTitle ir
                ]
        appendFile filename "-------------------------------\n"
      _ -> return ()




