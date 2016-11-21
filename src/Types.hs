{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import           Control.Lens         (makeLenses, (^.))

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
