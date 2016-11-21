{-# OPTIONS_GHC -Wall #-}

module Parse
( parseEventsPage )
  where

import           Data.Char         (isAlphaNum)
import           Data.List.Split   (keepDelimsL, split, whenElt)
import           Data.Maybe        (mapMaybe)
import           Data.List            (concat, intercalate, dropWhileEnd)
import           Text.HTML.TagSoup (Tag, fromAttrib, fromTagText, isTagOpenName,
                                    isTagText, maybeTagText, parseTags,
                                    renderTags, (~/=))
import           Types             

parseEventsPage :: String -> [Event]
parseEventsPage html = mapMaybe parseEvent $ (splitEvents . findEventsTable) $ parseTags html

maybeTextAfterTag :: String -> [Tag String] -> Maybe String
maybeTextAfterTag tag allTags = fromNext $ dropWhile (~/= tag) allTags
  where fromNext xs = if length xs >= 2 then maybeTagText $ xs !! 1 else Nothing

isNotEmptyTagText :: Tag String -> Bool
isNotEmptyTagText t = isTagText t && any isAlphaNum (fromTagText t) || not (isTagText t)

findEventsTable :: [Tag String] -> [Tag String]
findEventsTable = takeWhile (~/= "</div>") . drop 2 . dropWhile (~/= "</div>") . dropWhile (~/= "<div class=meetups-event-table>")

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
