{-# OPTIONS_GHC -Wall #-}

module Parse
  ( parseEventsPage
  , updateEvent
  )
  where

import           Control.Lens        ((&), (.~), (^.))
import           Control.Monad       (liftM)
import           Data.Char           (isAlphaNum, isDigit)
import           Data.List           (dropWhileEnd, find)
import           Data.List.Split     (keepDelimsL, split, whenElt)
import           Data.Maybe          (mapMaybe)
import           Debug.Trace         (trace)
import           Text.HTML.TagSoup   (Tag, fromAttrib, fromTagText,
                                      isTagOpenName, isTagText, maybeTagText,
                                      parseTags, (~/=))
import           Text.Read           (readMaybe)
import           Types

parseEventsPage :: String -> [Event]
parseEventsPage html = mapMaybe parseEventTableEntry $ (splitEvents . findEventsTable) $ parseTags html

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

parseEventTableEntry :: [Tag String] -> Maybe Event
parseEventTableEntry tags | length tags < 3 = Nothing
                          | (~/= "<a>") $ head tags = Nothing
                          | otherwise = let eventLink = Just $ "http://us.battle.net/" ++ fromAttrib "href" (head $ dropWhile (~/= "<a>") tags)
                                            eventId = case eventLink of
                                                           Just l -> readMaybe (dropWhile (not . isDigit) l)
                                                           Nothing -> return (-1)

                                            eventName = maybeTextAfterTag "<span class=meetups-event-table__cell__name>" tags
                                            eventLocation = Location country state city
                                              where country = maybeTextAfterTag "<span class=meetups-event-table__cell__country>" tags
                                                    state = maybeTextAfterTag "<span class=meetups-event-table__cell__state>" tags
                                                    city = maybeTextAfterTag "<span class=meetups-event-table__cell__city>" tags
                                            eventDate = parseDate <$> maybeTextAfterTag "<span class=\"meetups-event-table__cell meetups-event-table__cell--time\">" tags
                                              where parseDate = dropWhile (not . isAlphaNum) . dropWhileEnd (not . isAlphaNum)
                                         in Just (Event eventId eventLink eventName eventLocation (Just "") eventDate)

updateEvent :: Event -> String -> Event
updateEvent evt html = let tags = (dropWhile (~/= "<div class=\"meetup-header meetup-header--fsg-detail\">") $ parseTags html)
                           dateTime = liftM (fromAttrib "datetime") (find (isTagOpenName "time") tags)
                           oneMoreLink = liftM (fromAttrib "href") (find (isTagOpenName "a") tags)
                        in evt & date .~ (convertDate dateTime)
                               & extraLink .~ oneMoreLink

  where convertDate Nothing = Nothing
        convertDate (Just str) | (length str < 10) = Nothing
                               | otherwise = Just $ day ++ "." ++ month ++ "." ++ year
                               where year = take 4 str
                                     month = (take 2 . drop 5) str
                                     day = (take 2 . drop 8) str
