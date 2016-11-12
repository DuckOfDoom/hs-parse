{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Lens         (makeLenses, (^.))
import           Data.Char            (isAlphaNum)
import           Data.List            (concat, intercalate, dropWhileEnd)
import           Data.List.Split      (keepDelimsL, split, whenElt)
import           Data.Maybe           (mapMaybe)

import qualified Data.ByteString      as BS (ByteString, writeFile)
import qualified Data.ByteString.Lazy as LBS (toStrict)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as E (decodeUtf8, encodeUtf8)
import qualified Network.Wreq         as Wreq (get, responseBody)
import           Text.HTML.TagSoup    (Tag, fromAttrib, fromTagText,
                                       isTagOpenName, isTagText, maybeTagText,
                                       parseTags, renderTags, (~/=))

toByteString :: String -> BS.ByteString
toByteString = E.encodeUtf8 . T.pack

url :: String
url = "http://us.battle.net/hearthstone/en/fireside-gatherings?country=RU#fireside-gatherings"

type City = Maybe String
type State = Maybe String
type Country = Maybe String

data Location = Location Country State City

instance Show Location where 
  show (Location country state city) = show country ++ " " ++ show state ++ " " ++ show city

data Event = Event
           { _link     :: String
           , _name     :: Maybe String
           , _location :: Location
           , _date     :: Maybe String
           }

makeLenses ''Event

instance Show Event where
  show evt = "Event:\n" ++
             "    Link: " ++ show (evt ^. link) ++ "\n" ++
             "    Name: " ++ show (evt ^. name) ++ "\n" ++
             "    Location: " ++ show (evt ^. location) ++ "\n" ++ 
             "    Date: " ++ show (evt ^. date)

maybeTextAfterTag :: String -> [Tag String] -> Maybe String
maybeTextAfterTag tag allTags = fromNext $ dropWhile (~/= tag) allTags
  where fromNext xs = if length xs >= 2 then maybeTagText $ xs !! 1 else Nothing

isNotEmptyTagText :: Tag String -> Bool
isNotEmptyTagText t = isTagText t && any isAlphaNum (fromTagText t) || not (isTagText t)

getHtml :: IO String
getHtml = do
  response <- Wreq.get url
  return $ (T.unpack . E.decodeUtf8 . LBS.toStrict) (response ^. Wreq.responseBody)

findEventsTable :: String -> [Tag String]
findEventsTable = takeWhile (~/= "</div>") . drop 2 . dropWhile (~/= "</div>") . dropWhile (~/= "<div class=meetups-event-table>") . parseTags

splitEvents :: [Tag String] -> [[Tag String]]
splitEvents = drop 1 . split ((keepDelimsL . whenElt) isStartTag) . filter isNotEmptyTagText
  where isStartTag t = isTagOpenName "a" t && (fromAttrib "class" t == "meetups-event-table__row")

parseEvent :: [Tag String] -> Maybe Event
parseEvent tags | length tags < 3 = Nothing
                | (~/= "<a>") $ head tags = Nothing
                | otherwise = let eventLink = "http://us.battle.net/" ++ fromAttrib "href" (head $ dropWhile (~/= "<a>") tags)
                                  eventName = maybeTextAfterTag "<span class=meetups-event-table__cell__name>" tags
                                  eventLocation = Location country state city
                                    where country = maybeTextAfterTag "<span class=meetups-event-table__cell__country>" tags
                                          state = maybeTextAfterTag "<span class=meetups-event-table__cell__state>" tags
                                          city = maybeTextAfterTag "<span class=meetups-event-table__cell__city>" tags
                                  eventDate = parseDate <$> maybeTextAfterTag "<span class=\"meetups-event-table__cell meetups-event-table__cell--time\">" tags
                                    where parseDate = dropWhile (not . isAlphaNum) . dropWhileEnd (not . isAlphaNum)
                               in Just (Event eventLink eventName eventLocation eventDate)

test :: IO ()
test = do
  eventTags <- (splitEvents . findEventsTable) <$> getHtml
--  BS.writeFile "test_split.html" $ toByteString $ renderTags $ concat tags
  BS.writeFile "results.txt" (toByteString (intercalate "\n\n" $ map show $ mapMaybe parseEvent eventTags))
--  print $ tags

downloadChunk :: IO ()
downloadChunk = do
  tags <- (splitEvents . findEventsTable) <$> getHtml
  BS.writeFile "test.html" $ toByteString $ renderTags $ concat tags

downloadHtml :: IO ()
downloadHtml = toByteString <$> getHtml >>= BS.writeFile "downloaded.html"

main :: IO ()
main = return ()
