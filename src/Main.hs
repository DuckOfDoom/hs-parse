{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Lens         (makeLenses, (^.))
import           Data.Char            (isAlphaNum)
import           Data.List            (concat)
import           Data.List.Split      (split, whenElt, keepDelimsL)

import qualified Data.ByteString      as BS (ByteString, writeFile)
import qualified Data.ByteString.Lazy as LBS (toStrict)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as E (decodeUtf8, encodeUtf8)
import qualified Network.Wreq         as Wreq (get, responseBody)
import           Text.HTML.TagSoup    (Tag, fromAttrib, fromTagText, isTagOpenName, 
                                       isTagText, maybeTagText, parseTags,
                                       renderTags, (~/=))

toByteString :: String -> BS.ByteString
toByteString = E.encodeUtf8 . T.pack

type City = Maybe String
type State = Maybe String
type Country = Maybe String

data Location = Location Country State City
              deriving Show

data Event = Event
           { _link     :: String
           , _name     :: Maybe String
           , _location :: Location
           , _date     :: Maybe String
           }
           deriving Show

makeLenses ''Event

maybeTextAfterTag :: String -> [Tag String] -> Maybe String
maybeTextAfterTag tag allTags = fromNext $ dropWhile (~/= tag) allTags
  where fromNext xs = if length xs >= 2 then maybeTagText $ xs !! 1 else Nothing

url :: String
url = "http://us.battle.net/hearthstone/en/fireside-gatherings?country=RU#fireside-gatherings"

isNotEmptyTagText :: Tag String -> Bool
isNotEmptyTagText t = (isTagText t && (any isAlphaNum (fromTagText t))) || (not (isTagText t))

getHtml :: IO String
getHtml = do
  response <- Wreq.get url
  return $ (T.unpack . E.decodeUtf8 . LBS.toStrict) (response ^. Wreq.responseBody)

findEventsTable :: String -> [Tag String]
findEventsTable = takeWhile (~/= "</div>") . drop 2 . dropWhile (~/= "</div>") . dropWhile (~/= "<div class=meetups-event-table>") . parseTags

splitEvents :: [Tag String] -> [[Tag String]]
splitEvents = drop 1 . (split $ (keepDelimsL . whenElt) isStartTag) . filter isNotEmptyTagText
  where isStartTag t = isTagOpenName "a" t && (fromAttrib "class" t == "meetups-event-table__row")

parseEvent :: [Tag String] -> Maybe Event
parseEvent tags | length tags < 3 = Nothing
                | (~/= "<a>") $ head tags = Nothing
                | otherwise = let link = "http://us.battle.net/" ++ fromAttrib "href" (head $ dropWhile (~/= "<a>") tags)
                                  name = maybeTextAfterTag "<span class=meetups-event-table__cell__name>" tags
                                  location = Location Nothing Nothing Nothing
                                  date = Nothing
                               in Just (Event link name location date)

test :: IO ()
test = do
  eventTags <- (splitEvents . findEventsTable) <$> getHtml
--  BS.writeFile "test_split.html" $ toByteString $ renderTags $ concat tags
  print $ map parseEvent eventTags
--  print $ tags

downloadChunk :: IO ()
downloadChunk = do
  tags <- (splitEvents . findEventsTable) <$> getHtml
  BS.writeFile "test.html" $ toByteString $ renderTags $ concat tags

downloadHtml :: IO ()
downloadHtml = toByteString <$> getHtml >>= BS.writeFile "downloaded.html"

main :: IO ()
main = return ()
