{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens         ((^.), (.~))

import qualified Data.ByteString      as BS (ByteString, writeFile)
import qualified Data.ByteString.Lazy as LBS (ByteString, toStrict)
import           Data.Function        ((&))
import           Control.Exception    (SomeException, try)

import Data.String (fromString)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as E (decodeUtf8, encodeUtf8)
import qualified Network.Wreq         as Wreq (getWith, param, responseBody, defaults, Response)

import Parse (getEvents)
import Types (Event, Location)

toByteString :: String -> BS.ByteString
toByteString = E.encodeUtf8 . T.pack

baseUrl :: String
baseUrl = "http://eu.battle.net/hearthstone/ru/fireside-gatherings"

url :: String
url = "http://us.battle.net/hearthstone/en/fireside-gatherings?country=RU#fireside-gatherings"

responseToString :: Wreq.Response LBS.ByteString -> String
responseToString r = (T.unpack . E.decodeUtf8 . LBS.toStrict) (r ^. Wreq.responseBody)

getTablePage :: String -> Int -> IO (Maybe String)
getTablePage country page = do
  response <- try (Wreq.getWith opts baseUrl)
  case response of 
       Left _ -> return Nothing
       Right r -> return $ Just (responseToString <$> r)
  where opts = Wreq.defaults & Wreq.param "country" .~ [fromString country]
                             & Wreq.param "page" .~ [fromString $ show page]

--test :: IO ()
--test = do
--  eventTags <- (splitEvents . findEventsTable) <$> getHtml
--  BS.writeFile "test_split.html" $ toByteString $ renderTags $ concat tags
--  BS.writeFile "results.txt" (toByteString (intercalate "\n\n" $ map show $ mapMaybe parseEvent eventTags))
----  print $ tags

--downloadPage :: IO ()
--downloadPage = do
--  tags <- (splitEvents . findEventsTable) <$> getHtml
--  BS.writeFile "test.html" $ toByteString $ renderTags $ concat tags

downloadPage :: String -> Int -> IO ()
downloadPage country page = 
  toByteString <$> getTablePage country page >>= BS.writeFile fName
    where fName = "testPage_" ++ country ++ "_" ++ show page ++ ".html" 

--
--downloadHtml :: IO ()
--downloadHtml = toByteString <$> getHtml >>= BS.writeFile "downloaded.html"

main :: IO ()
main = return ()
