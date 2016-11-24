{-# OPTIONS_GHC -Wall #-}

module Parse
  ( parseEventsPage
  , updateEvent
  )
  where

import           Control.Lens      ((&), (.~))
import           Control.Monad     (liftM)
import           Data.Char         (isAlphaNum, isDigit)
import           Data.List         (find, isPrefixOf)
import           Data.List.Split   (keepDelimsL, split, whenElt)
import           Data.Maybe        (mapMaybe)
import           Prelude           hiding (id)
import           Text.HTML.TagSoup (Tag, fromAttrib, fromTagText, isTagOpenName,
                                    isTagText, maybeTagText, parseTags, (~/=), (~==))
import           Text.Read         (readMaybe)
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
                          | otherwise = Just (defaultEvent & id .~ eventId
                                                           & link .~ eventLink
                                                           & name .~ maybeTextAfterTag "<span class=meetups-event-table__cell__name>" tags
                                                           & location .~ (defaultLocation & country .~ maybeTextAfterTag "<span class=meetups-event-table__cell__country>" tags
                                                                                          & state .~ maybeTextAfterTag "<span class=meetups-event-table__cell__state>" tags
                                                                                          & city .~ maybeTextAfterTag "<span class=meetups-event-table__cell__city>" tags))
                            where eventLink = Just $ "http://eu.battle.net" ++ fromAttrib "href" (head $ dropWhile (~/= "<a>") tags)
                                  eventId = case eventLink of
                                                 Just l -> readMaybe (dropWhile (not . isDigit) l)
                                                 Nothing -> return (-1)

updateEvent :: Event -> String -> Event
updateEvent evt html = let tags = (dropWhile (~/= "<div class=\"meetup-header meetup-header--fsg-detail\">") $ parseTags html)
                           dateTime = liftM (fromAttrib "datetime") (find (isTagOpenName "time") tags)
                           oneMoreLink = liftM (fromAttrib "href") (find (isTagOpenName "a") tags)
                           mapTag = (find (~== "<div class=\"map-item\">") tags)
                        in evt & date .~ (convertDate dateTime)
                               & extraLink .~ (convertExtraLink oneMoreLink)
                               & coords .~ (convertCoords mapTag)

  where convertDate Nothing = Nothing
        convertDate (Just str) | (length str < 10) = Nothing
                               | otherwise = Just $ day ++ "." ++ month ++ "." ++ year
                               where year = take 4 str
                                     month = (take 2 . drop 5) str
                                     day = (take 2 . drop 8) str

        convertExtraLink Nothing = Nothing
        convertExtraLink (Just l) | ("https://maps.google.com" `isPrefixOf` l) = Nothing
                             | otherwise = Just l

        convertCoords Nothing = Nothing
        convertCoords (Just tag) = Just ((read (fromAttrib "data-lat" tag)) :: Double, -- Потенциальное место для фейла :D
                                         (read (fromAttrib "data-lng" tag)) :: Double)
