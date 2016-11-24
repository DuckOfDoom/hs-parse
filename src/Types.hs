{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Types where

import           Control.Lens (makeLenses, (^.))
import           Data.Aeson   (FromJSON, ToJSON, object, toJSON, (.=))
import           Data.Csv     (ToField, ToRecord, record, toField, toRecord)
import           GHC.Generics (Generic)

import           Prelude      hiding (id)

data Location = Location { _country :: Maybe String
                         , _state   :: Maybe String
                         , _city    :: Maybe String
                         }
              deriving (Generic)

makeLenses ''Location

instance Show Location where
  show (Location country state city) = show country ++ " " ++ show state ++ " " ++ show city

instance ToJSON Location where
  toJSON (Location country state city) =
    object [ "city" .= city
           , "state" .= state
           , "country" .= country
           ]

instance FromJSON Location

data Event = Event { _id        :: Maybe Int
                   , _link      :: Maybe String
                   , _name      :: Maybe String
                   , _location  :: Location
                   , _extraLink :: Maybe String
                   , _date      :: Maybe String
                   } deriving (Generic, Show)

makeLenses ''Event

defaultEvent = Event { _id = Nothing
                     , _link = Nothing
                     , _name = Nothing
                     , _location = Location { _country = Nothing
                                            , _state = Nothing
                                            , _city = Nothing
                                            }
                     , _extraLink = Nothing
                     , _date = Nothing
                     }

--instance Show Event where
--  show evt = "Event:\n" ++
--             "    Link: " ++ show (evt ^. link) ++ "\n" ++
--             "    Name: " ++ show (evt ^. name) ++ "\n" ++
--             "    Location: " ++ show (evt ^. location) ++ "\n" ++
--             "    Date: " ++ show (evt ^. date)

instance ToJSON Event where
  toJSON evt = object [ "id"         .= (evt ^. id)
                      , "link"       .= (evt ^. link)
                      , "name"       .= (evt ^. name)
                      , "location"   .= (evt ^. location)
                      , "extra_link" .= (evt ^. extraLink)
                      , "date"       .= (evt ^. date)
                      ]

instance FromJSON Event

instance ToRecord Event where
  toRecord evt = record [ toField (evt ^. id)
                        , toField (evt ^. name)
                        , toField (evt ^. date)
                        , toField (evt ^. (location . city))
                        , toField (evt ^. (location . country))
                        , toField (evt ^. link)
                        , toField (evt ^. extraLink)
                        ]


