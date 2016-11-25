{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Types where

import           Control.Lens (makeLenses, (^.))
import           Data.Aeson   (FromJSON, ToJSON, object, toJSON, (.=))
import           Data.Csv     (ToField, ToRecord, record, toField, toRecord)
import           Data.String  (fromString)
import           GHC.Generics (Generic)

import           Prelude      hiding (id)

data Coords = Coords { _lat :: Double
                     , _lng :: Double
                     }
  deriving (Show, Generic)

makeLenses ''Coords

instance ToField Coords where 
  toField c = fromString (show (c ^. lat) ++ "," ++ show (c ^. lng))
instance ToJSON Coords
instance FromJSON Coords

-- Location
data Location = Location { _country :: Maybe String
                         , _state   :: Maybe String
                         , _city    :: Maybe String
                         }
              deriving (Generic)

makeLenses ''Location

defaultLocation = Location { _country = Nothing
                           , _state = Nothing
                           , _city = Nothing
                           }

instance Show Location where
  show l = show (l ^. country) ++ " " ++ show (l ^. state) ++ " " ++ show (l ^. city)

instance ToJSON Location where
  toJSON l = 
    object [ "city" .= (l ^. city)
           , "state" .= (l ^. state)
           , "country" .= (l ^. country)
           ]

instance FromJSON Location

-- Event
data Event = Event { _id        :: Maybe Int
                   , _link      :: Maybe String
                   , _name      :: Maybe String
                   , _coords    :: Maybe Coords
                   , _location  :: Location
                   , _extraLink :: Maybe String
                   , _date      :: Maybe String
                   } deriving (Generic, Show)

makeLenses ''Event

defaultEvent = Event { _id = Nothing
                     , _link = Nothing
                     , _name = Nothing
                     , _coords = Nothing
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
                        , toField (evt ^. coords)
                        ]


