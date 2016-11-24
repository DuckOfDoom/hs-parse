{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Geocode
  ( positionToAddress
  )
  where

import qualified Network      as Net

url :: String
url = "https://maps.googleapis.com/maps/api/geocode/json"

geocodeAPIToken :: Maybe String
geocodeAPIToken = Just 

positionToAddress :: (Double, Double) -> IO (Maybe String)
positionToAddress coords = positionToAddress' geocodeAPIToken coords
  where positionToAddress' Nothing _ = return Nothing
        positionToAddress' (Just token) (lat, lng) = do
          json <- Net.maybeGetWith url opts
          print json
          return $ Just ""
            where opts = [ ("latlng", show lat ++ "," ++ show lng)
                         , ("language", "ru")
                         , ("key", token)
                         ]


