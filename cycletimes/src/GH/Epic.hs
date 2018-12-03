{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE RecordWildCards   #-}

module GH.Epic where


import qualified  Data.ByteString as BS
import qualified  Data.ByteString.Char8 as BS8
import qualified  Data.ByteString.Lazy as LBS
import qualified  Data.List as L
import qualified  Data.Map.Strict as M
import            Control.Monad (forM)
import            Data.Aeson

import           GH.Types
import           GH.Parser
import           GH.Queries



-- | Creates a Map of Child -> Parent for a given RepoId
makeEpicMap :: String -> Int -> IO (M.Map Int Int)
makeEpicMap token repoId = do
  epics <- getAllEpicsFromZHRepo token repoId
  let eIssues = eitherDecode epics :: Either String AllEpics
  case eIssues of
    Left _      -> error "cannot parse All Epic response"
    Right (AllEpics epics) -> do
      childIssues <- forM epics $ \eId -> do
        resp <- getSingleEpicFromZHRepo token repoId eId
        let epicChildren = eitherDecode resp :: Either String EpicChildren
        case epicChildren of
          Left _ -> error "Cannot Parse Epic Data Response"
          Right (EpicChildren childs) -> return childs
      let children = concat childIssues
          parents  = concat $ zipWith (\ c e-> replicate (length c) e) childIssues epics
      return . M.fromList $ zip children parents

