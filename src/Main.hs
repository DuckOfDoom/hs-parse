{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens         (makeLenses, (.~), (^.))
import           Data.List.Split      (chunksOf)

import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS (writeFile, toStrict)
import           Data.Text            (unpack)
import           Data.Text.Encoding   (decodeUtf8)
import qualified Network.Wreq         as Wreq (get, responseBody)
import           Text.HTML.TagSoup    (Tag, fromAttrib, isTagCloseName,
                                       parseTags, renderTags, (~/=))

data Event = Event
           { name     :: String
           , location :: String
           , date     :: String
           , link     :: String
           }
           deriving Show

--makeLenses ''Event

url :: String
url = "http://us.battle.net/hearthstone/en/fireside-gatherings?country=RU#fireside-gatherings"

getHtml :: IO ByteString
getHtml = do
  response <- Wreq.get url
  return (response ^. Wreq.responseBody)

findEventsTable :: ByteString -> [Tag ByteString]
findEventsTable = takeWhile (~/= "</div>") . drop 1 . dropWhile (~/= "</div>") . dropWhile (~/= "<div class=meetups-event-table>") . parseTags 

splitEvents :: [Tag ByteString] -> [[Tag ByteString]]
splitEvents = chunksOf 32

parseEvent :: [Tag ByteString] -> Event
parseEvent tags = Event name location date link
  where link = "http://us.battle.net/" ++ fromAttrib "href" (head tags)
        name = ""
        location = ""
        date = ""

test :: IO ()
test = do
  tags <- (splitEvents . findEventsTable) <$> getHtml
  LBS.writeFile "test.html" $ renderTags $ head tags

downloadHtml :: IO ()
downloadHtml = getHtml >>= LBS.writeFile "downloaded.html"

main :: IO ()
main = return ()
