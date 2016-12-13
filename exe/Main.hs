{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson               (toJSON)
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString          as BS (writeFile)
import qualified Data.ByteString.Lazy     as LBS (toStrict)
import           Data.Csv                 (encode)
import           Debug.Trace              (traceIO)
import           HSParser.Internal.EventProcessing (getAllEvents)
import           HSParser.Types           (Event)
import           System.Exit              (exitFailure)


eventsToCSV :: [Event] -> String -> IO ()
eventsToCSV events filename = do
  traceIO $ "Writing CSV to '" ++ filename ++ "'..."
  BS.writeFile filename (LBS.toStrict $ encode events)

eventsToJSON :: [Event] -> String -> IO ()
eventsToJSON events filename = do
  traceIO $ "Writing JSON to '" ++ filename ++ "'..."
  BS.writeFile filename (LBS.toStrict $ encodePretty' conf (toJSON events))
      where sorting = keyOrder ["id", "link", "name", "location", "date"]
            conf = Config { confIndent = Spaces 4
                          , confCompare = sorting
                          , confNumFormat = Generic
                          }

main :: IO ()
main = do
  regions <- askRegion
  fileName <- askFilename
  events <- foldMap getAllEvents regions
  eventsToCSV events fileName
  putStrLn $ "Done! Processed " ++ show (length events) ++ " events!"
  _ <- getLine
  return ()

  where askRegion = do
          putStrLn ("Oh shit whaddup! Please select region:\n" ++
                    "1. RU\n" ++
                    "2. CIS\n" ++
                    "3. ALL")
          region <- getLine
          case region of
              "1" -> return (["RU"] :: [String])
              "2" -> return (["RU", "BY", "UA"] :: [String])
              "3" -> return (["ALL"] :: [String])
              _ -> do putStrLn "'1', '2' or '3' please."
                      _ <- getLine
                      exitFailure

        askFilename = do
          putStrLn "Choose filename for results (empty for 'resultsCSV.txt')"
          fileName <- getLine
          case fileName of
               "" -> return "resultsCSV.txt"
               _ -> return fileName
