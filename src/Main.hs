{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens             ((^.))
import           Prelude                  hiding (id)

import qualified Data.ByteString          as BS (ByteString, writeFile)
import qualified Data.ByteString.Lazy     as LBS (toStrict)
import           Debug.Trace              (trace, traceIO)

import           Data.Aeson               (toJSON)
import           Data.Aeson.Encode.Pretty

import           Data.Csv                 (encode)

import qualified Data.Text                as T
import qualified Data.Text.Encoding       as E (encodeUtf8)

import qualified Network                  as Net (maybeGetWith)

import           Parse                    (parseEventsPage, updateEvent)
import           Types

toByteString :: String -> BS.ByteString
toByteString = E.encodeUtf8 . T.pack

baseUrl :: String
baseUrl = "http://eu.battle.net/hearthstone/ru/fireside-gatherings"

getPage :: String -> Int -> IO (Maybe String)
getPage region pageNumber = Net.maybeGetWith baseUrl opts
  where opts = [ ("country", region)
               , ("page", show pageNumber)
               ]

updateAllEvents :: [Event] -> IO [Event]
updateAllEvents = mapM update
  where update e = case (e ^. link) of
                        Nothing -> trace ("Event #" ++ show (e ^. id) ++ "has no link!")
                                         (return e)
                        Just eventLink -> do
                          maybeEventPage <- Net.maybeGetWith eventLink []
                          traceIO ("   Processing event #" ++ (drop 5 $ show (e ^. id)) ++ "...")
                          case maybeEventPage of
                               Nothing -> return e
                               Just p -> return $ Parse.updateEvent e p

getAllEvents :: String -> IO [Event]
getAllEvents locale =
  getAllEvents' [] [1..] >>= updateAllEvents
  where getAllEvents' evts [] = return evts
        -- Downloading pages until there are no more pages on site
        getAllEvents' evts (x:xs) = do
          page <- getPage locale x
          case page of
            Just p -> trace ("Loaded page " ++ show x ++ "...")
                            (getAllEvents' (evts ++ getEventsFromPage x p) xs)
            Nothing -> return evts

        -- Parsing all pages
        getEventsFromPage pageNumber page = trace ("Found " ++ show (length events) ++ " events on page " ++ show pageNumber ++ ":") events
          where events = (Parse.parseEventsPage page)


testJSON :: IO ()
testJSON = do
  evts <- getAllEvents "RU"
  BS.writeFile "test_json.txt" $ (LBS.toStrict $ encodePretty' conf (toJSON evts))
    where sorting = keyOrder ["id", "link", "name", "location", "date"]
          conf = Config { confIndent = Spaces 4
                        , confCompare = sorting
                        , confNumFormat = Generic
                        }

testCSV :: IO ()
testCSV = do
  evts <- getAllEvents "RU"
  BS.writeFile "test_csv.txt" $ (LBS.toStrict $ encode evts)

main :: IO ()
main = return ()
