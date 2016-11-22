{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import           Control.Lens (makeLenses, (^.))
import           GHC.Generics (Generic)
import           Data.Aeson (ToJSON, FromJSON, toEncoding, genericToEncoding, defaultOptions)

type City = Maybe String
type State = Maybe String
type Country = Maybe String

data Location = Location Country State City
              deriving (Generic)

instance Show Location where
  show (Location country state city) = show country ++ " " ++ show state ++ " " ++ show city

instance ToJSON Location where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Location

data Event = Event
           { _link     :: String
           , _name     :: Maybe String
           , _location :: Location
           , _date     :: Maybe String
           } deriving (Generic)

makeLenses ''Event

instance Show Event where
  show evt = "Event:\n" ++
             "    Link: " ++ show (evt ^. link) ++ "\n" ++
             "    Name: " ++ show (evt ^. name) ++ "\n" ++
             "    Location: " ++ show (evt ^. location) ++ "\n" ++
             "    Date: " ++ show (evt ^. date)

instance ToJSON Event where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Event
