{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module EventProcessing
  ( getAllEvents )
  where

import           Control.Lens ((^.))
--import           Control.Concurrent (MVar, newMVar, modifyMVar_, readMVar, forkIO, threadDelay, setNumCapabilities)

import           Debug.Trace  (trace, traceIO)
import qualified Network      as Net (maybeGetWith)
import           Parse        (parseEventsPage, updateEvent)
import           Prelude      hiding (id)
import           Types

baseUrl :: String
baseUrl = "http://eu.battle.net/hearthstone/ru/fireside-gatherings"

getPage :: String -> Int -> IO (Maybe String)
getPage region pageNumber = Net.maybeGetWith baseUrl opts
  where opts = [ ("country", region)
               , ("page", show pageNumber)
               ]

updateAllEvents :: [Event] -> IO [Event]
updateAllEvents = mapM update
  where update e = case e ^. link of
                        Nothing -> trace ("Event #" ++ show (e ^. id) ++ "has no link!")
                                         (return e)
                        Just eventLink -> do
                          maybeEventPage <- Net.maybeGetWith eventLink []
                          traceIO ("   Processing event #" ++ drop 5 (show (e ^. id)) ++ "...")
                          case maybeEventPage of
                               Nothing -> update e -- Retrying failed events
                               Just p -> return $ Parse.updateEvent e p

--updateAllEventsConc :: [Event] -> IO [Event]
--updateAllEventsConc events = do 
--  setNumCapabilities 4
--  updatedMVar <- newMVar []
--  mapM_ (update updatedMVar) events
--  waitAndReturn (length events) updatedMVar
--  where update :: MVar [Event] -> Event -> IO ()
--        update updated' e = case e ^. link of
--                            Nothing -> do 
--                              traceIO ("Event #" ++ show (e ^. id) ++ "has no link!")
--                              modifyMVar_ updated' (\u -> return $ e : u)
--                            Just eventLink -> do 
--                              traceIO ("   Processing event #" ++ drop 5 (show (e ^. id)) ++ "...")
--                              _ <- forkIO $ do
--                                maybeEventPage <- Net.maybeGetWith eventLink []
--                                case maybeEventPage of
--                                     Nothing -> update updated' e -- Retrying failed events
--                                     Just p -> do modifyMVar_ updated' (\u -> return $ ((Parse.updateEvent e p) : u))
--                              return ()
--
--        -- Waiting until all events are updated by comparing length of source list and updated list in MVar
--        waitAndReturn :: Int -> MVar [Event] -> IO [Event]
--        waitAndReturn sourceLength updated' = do
--          updatedList <- readMVar updated'
--          threadDelay 100000
--          if length updatedList == sourceLength 
--             then return $ updatedList 
--             else waitAndReturn sourceLength updated'


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
          where events = Parse.parseEventsPage page
