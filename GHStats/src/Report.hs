{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE RecordWildCards    #-}
module Report where

import Data.Csv
import qualified Data.ByteString.Lazy as BS

import Types

defaultPRCSVHeader :: Header
defaultPRCSVHeader = header
  [ "Development Start Time"
  , "Review Start Time"
  , "Closing Time"
  ]

instance ToNamedRecord PRCSVData where
  toNamedRecord (PRCSVData ( devStartTime, reviewStartTime, closingTime)) =
    namedRecord [ "Development Start Time" .= devStartTime
                , "Review Start Time"      .= reviewStartTime
                , "Closing Time"           .= closingTime
                ]

instance DefaultOrdered PRCSVData where
  headerOrder _ = defaultPRCSVHeader

makeReport :: (ToNamedRecord a, DefaultOrdered a) => FilePath -> [a] -> IO ()
makeReport filePath values = BS.writeFile filePath $ content
  where
    content = encodeDefaultOrderedByName values
