{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString                   as BS (writeFile)
import           HSParser.Export                   (getEventsAsCSV)
import           System.Exit                       (exitFailure)

main :: IO ()
main = do
  regions <- askRegion
  filename <- askFilename
  csv <- getEventsAsCSV regions
  BS.writeFile filename csv
  putStrLn $ "Done! Written results to " ++ filename ++ "!"
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
