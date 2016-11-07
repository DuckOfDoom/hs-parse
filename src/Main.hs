{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Lens         (makeLenses, (^.))
import           Data.List            (intercalate)
import           Data.List.Split      (chunksOf)

import qualified Data.ByteString      as BS (ByteString, writeFile)
import qualified Data.ByteString.Lazy as LBS (toStrict)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as E (decodeUtf8, encodeUtf8)
import qualified Network.Wreq         as Wreq (get, responseBody)
import           Text.HTML.TagSoup    (Tag, fromAttrib, fromTagText, parseTags,
                                       renderTags, (~/=))

toByteString :: String -> BS.ByteString
toByteString = E.encodeUtf8 . T.pack 

type City = String
type State = String
type Country = String

data Location = Location Country State City
              deriving Show

data Event = Event
           { _name     :: String
           , _location :: Location
           , _date     :: String
           , _link     :: String
           }
           deriving Show

makeLenses ''Event

url :: String
url = "http://us.battle.net/hearthstone/en/fireside-gatherings?country=RU#fireside-gatherings"

getHtml :: IO String
getHtml = do
  response <- Wreq.get url
  return $ (T.unpack . E.decodeUtf8 . LBS.toStrict) (response ^. Wreq.responseBody)

findEventsTable :: String -> [Tag String]
findEventsTable = takeWhile (~/= "</div>") . drop 2 . dropWhile (~/= "</div>") . dropWhile (~/= "<div class=meetups-event-table>") . parseTags

splitEvents :: [Tag String] -> [[Tag String]]
splitEvents = chunksOf 32

parseEvent :: [Tag String] -> Event
parseEvent tags = Event name location date link
  where link = "http://us.battle.net/" ++ fromAttrib "href" (head $ dropWhile (~/= "<a>") tags)
        name = fromTagText $ dropWhile (~/= "<span class=meetups-event-table__cell__name>") tags !! 1
        location = Location "" "" ""
        date = ""

test :: IO ()
test = do
  tags <- (splitEvents . findEventsTable) <$> getHtml
--  BS.writeFile "test.html" $ toByteString $ renderTags $ intercalate [] tags
  print $ map parseEvent tags
--  print tags

downloadHtml :: IO ()
downloadHtml = toByteString <$> getHtml >>= BS.writeFile "downloaded.html"

main :: IO ()
main = return ()
