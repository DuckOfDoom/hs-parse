{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens             ((.~), (^.))
import           Prelude                  hiding (id)

import           Control.Exception        (SomeException, try)
import qualified Data.ByteString          as BS (ByteString, writeFile)
import qualified Data.ByteString.Lazy     as LBS (ByteString, toStrict)
import           Data.Function            ((&))
import           Debug.Trace              (trace)

import           Data.Aeson               (toJSON)
import           Data.Aeson.Encode.Pretty

import           Data.Csv                 (encode)

import           Data.String              (fromString)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as E (decodeUtf8, encodeUtf8)
import qualified Network.Wreq             as Wreq (Response, defaults, get,
                                                   getWith, param, responseBody)

import           Parse                    (parseEventsPage, updateEvent)
import           Types
import Text.HTML.TagSoup 

toByteString :: String -> BS.ByteString
toByteString = E.encodeUtf8 . T.pack

baseUrl :: String
baseUrl = "http://eu.battle.net/hearthstone/ru/fireside-gatherings"

responseToString :: Wreq.Response LBS.ByteString -> String
responseToString r = (T.unpack . E.decodeUtf8 . LBS.toStrict) (r ^. Wreq.responseBody)

getPage :: String -> Int -> IO (Maybe String)
getPage country page = do
  response <- try (Wreq.getWith opts baseUrl) :: IO (Either SomeException (Wreq.Response LBS.ByteString))
  case response of
       Left _ -> return Nothing
       Right r -> return $ Just (responseToString r)
  where opts = Wreq.defaults & Wreq.param "country" .~ [fromString country]
                             & Wreq.param "page" .~ [fromString $ show page]

updateAllEvents :: [Event] -> IO [Event]
updateAllEvents = mapM update
  where update e = case (e ^. link) of
                        Nothing -> trace ("Event #" ++ show (e ^. id) ++ "has no link!")
                                         (return e)
                        Just eventLink -> do
                          putStrLn ("   Updating event #" ++ show (e ^. id) ++ "...")
                          response <- try (Wreq.get eventLink) :: IO (Either SomeException (Wreq.Response LBS.ByteString))
                          case response of
                               Left _ -> return e
                               Right r -> return $ Parse.updateEvent e (responseToString r)

getAllEvents :: String -> IO [Event]
getAllEvents locale =
  getAllEvents' [] [1..] >>= updateAllEvents
  where getAllEvents' evts [] = return evts
        -- Downloading pages until there are no more pages on site
        getAllEvents' evts (x:xs) = do
          page <- getPage locale x
          case page of
            Just p -> trace ("Loaded page " ++ show x ++ "...")
                            (getAllEvents' (evts ++ getEvents p) xs)
            Nothing -> return evts

        -- Parsing all pages
        getEvents page = trace ("Found " ++ show (length events) ++ " events!") events
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
