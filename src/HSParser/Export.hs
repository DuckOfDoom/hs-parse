{-# LANGUAGE OverloadedStrings #-}

module HSParser.Export
  ( getRawEvents
  , getEventsAsJSON
  , getEventsAsJSONPretty
  , getEventsAsCSV
  )
  where

import           Data.Aeson
import qualified Data.Aeson                        as JSON (encode)
import           Data.Aeson.Encode.Pretty
import           Data.ByteString
import           Data.ByteString.Lazy              as LBS (toStrict)
import qualified Data.Csv                          as CSV (encode)
import           HSParser.Internal.EventProcessing (getAllEvents)
import           HSParser.Types                    (Event)

getRawEvents :: [String] -> IO [Event]
getRawEvents = foldMap getAllEvents

getEventsAsJSONPretty :: [String] -> IO ByteString
getEventsAsJSONPretty regions = do
  events <- getRawEvents regions
  return $ LBS.toStrict $ encodePretty' conf (toJSON events)
      where sorting = keyOrder ["id", "link", "name", "location", "date"]
            conf = Config { confIndent = 4
                          , confCompare = sorting
                          }

getEventsAsJSON :: [String] -> IO ByteString
getEventsAsJSON regions = do
  events <- getRawEvents regions
  return $ LBS.toStrict $ JSON.encode (toJSON events)

getEventsAsCSV :: [String] -> IO ByteString
getEventsAsCSV regions = do
  events <- getRawEvents regions
  return $ LBS.toStrict $ CSV.encode events
