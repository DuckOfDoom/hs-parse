{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Network
  ( maybeGetWith
  , maybeGet
  )
  where

import           Control.Exception    (SomeException, try)
import           Control.Lens         ((&), (.~), (^.))
import qualified Data.ByteString.Lazy as LBS (ByteString, toStrict)
import           Data.String          (fromString)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as E (decodeUtf8)
import           Debug.Trace          (trace)
import qualified Network.Wreq         as Wreq (Response, defaults, getWith, param, responseBody)

responseToString :: Wreq.Response LBS.ByteString -> String
responseToString r = (T.unpack . E.decodeUtf8 . LBS.toStrict) (r ^. Wreq.responseBody)

maybeGetWith :: String -> [(T.Text, String)] -> IO (Maybe String)
maybeGetWith url optPairs = do
  response <- try (Wreq.getWith opts url) :: IO (Either SomeException (Wreq.Response LBS.ByteString))
  case response of
       Left ex -> trace ("Failed to get " ++ url ++ "!\nException:" ++ show ex ++ "!") return Nothing
       Right r -> return $ Just (responseToString r)
    where opts = foldl (\defaults (name, value) ->
                        defaults & Wreq.param name .~ [fromString value]) Wreq.defaults optPairs

maybeGet :: String -> IO (Maybe String)
maybeGet url = maybeGetWith url [] 
