{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import           Control.Lens (makeLenses, (^.))
import           GHC.Generics (Generic)
import           Data.Aeson (ToJSON, FromJSON, object, toJSON, (.=))

type Country = Maybe String
type State = Maybe String
type City = Maybe String

data Location = Location Country State City
              deriving (Generic)

instance Show Location where
  show (Location country state city) = show country ++ " " ++ show state ++ " " ++ show city

instance ToJSON Location where
  toJSON (Location country state city) =
    object ["city" .= city,  "state" .= state, "country" .= country]

instance FromJSON Location

data Event = Event
           { _link     :: String
           , _name     :: Maybe String
           , _location :: Location
           , _date     :: Maybe String
           } deriving (Generic, Show)

makeLenses ''Event

--instance Show Event where
--  show evt = "Event:\n" ++
--             "    Link: " ++ show (evt ^. link) ++ "\n" ++
--             "    Name: " ++ show (evt ^. name) ++ "\n" ++
--             "    Location: " ++ show (evt ^. location) ++ "\n" ++
--             "    Date: " ++ show (evt ^. date)

instance ToJSON Event where
  toJSON evt = object ["link" .= (evt ^. link), "name" .= (evt ^. name), "location" .= (evt ^. location), "date" .= (evt ^. date)]

instance FromJSON Event

