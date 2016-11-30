{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module EventProcessing
  ( getAllEvents )
  where

import           Control.Lens ((^.))
--import           Control.Concurrent (MVar, newMVar, modifyMVar_, readMVar, forkIO, threadDelay, setNumCapabilities)

import           Debug.Trace  (trace, traceIO)
import           Network      (NetworkError(..))
import qualified Network      as Net (eitherGetWith)
import           Parse        (parseEventsPage, updateEvent)
import           Prelude      hiding (id)
import           Types

baseUrl :: String
baseUrl = "http://eu.battle.net/hearthstone/ru/fireside-gatherings"

updateAllEvents :: [Event] -> IO [Event]
updateAllEvents evts = mapM update evts
  where update e = case e ^. link of
                     Nothing -> trace ("Event #" ++ show (e ^. id) ++ "has no link!")
                                      (return e)
                     Just eventLink -> do
                       maybeEventPage <- Net.eitherGetWith eventLink []
                       case maybeEventPage of
                            Left NotFound -> do
                              traceIO ("   Did not find page for event " ++ evtId ++ " (Status 404) Link: " ++ evtLink)
                              return e
                            Left _ -> do
                              traceIO ("   Failed to get page for event " ++ evtId ++ ", retrying...")
                              update e 
                            Right p -> do
                              traceIO ("   Processing event " ++ evtId ++ "..." )
                              return $ Parse.updateEvent e p
                         where evtId = drop 5 (show (e ^. id))
                               evtLink = drop 5 (show (e ^. link))
                                    

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
getAllEvents locale = do
  traceIO "Loading pages..."
  getAllEvents' [] [1..] >>= updateAllEvents
  where getAllEvents' evts [] = return evts
        -- Downloading pages until there are no more pages on site
        getAllEvents' evts (x:xs) = do
          page <- getPage locale x
          case page of
            Left NotFound -> do
              traceIO ("Total pages: " ++  show x ++ "\n" ++ 
                       "Total events: " ++ (show . length) evts)
              return evts
            Left _ -> do
              traceIO ("Failed to get page " ++ show x ++ ", retrying...")
              getAllEvents' evts (x:xs)
            Right p -> do
              getAllEvents' (evts ++ Parse.parseEventsPage p) xs

        getPage :: String -> Int -> IO (Either NetworkError String)
        getPage region pageNumber = Net.eitherGetWith baseUrl opts
          where opts = [ ("country", region)
                       , ("page", show pageNumber)
                       ]
