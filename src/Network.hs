{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Network
  ( NetworkError(..)
  , eitherGetWith
  , maybeGetWith
  , maybeGet
  )
  where

import           Control.Exception    (SomeException, try)
import           Control.Lens         ((&), (.~), (^.))
import qualified Data.ByteString.Lazy as LBS (ByteString, toStrict)
import           Data.String          (fromString)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as E (decodeUtf8)
import           Network.HTTP.Client  (HttpException (..))
import qualified Network.Wreq         as Wreq (Response, defaults, getWith, statusCode,
                                               param, responseBody)

data NetworkError = NotFound | OtherError
                  deriving (Show)

responseToString :: Wreq.Response LBS.ByteString -> String
responseToString r = (T.unpack . E.decodeUtf8 . LBS.toStrict) (r ^. Wreq.responseBody)

maybeGetWith :: String -> [(T.Text, String)] -> IO (Maybe String)
maybeGetWith url optPairs = do
  response <- try (Wreq.getWith opts url) :: IO (Either SomeException (Wreq.Response LBS.ByteString))
  case response of
       Left _ -> return Nothing
       Right r -> return $ Just (responseToString r)
    where opts = foldl (\defaults (name, value) ->
                        defaults & Wreq.param name .~ [fromString value]) Wreq.defaults optPairs

eitherGetWith :: String -> [(T.Text, String)] -> IO (Either NetworkError String)
eitherGetWith url optPairs = do
  response <- try (responseToString <$> (Wreq.getWith opts url)) :: IO (Either HttpException String)
  case response of
       Left (StatusCodeException s _ _) -> if s ^. Wreq.statusCode == 404 
                                              then return $ Left NotFound
                                              else return $ Left OtherError
       Left _ -> return $ Left OtherError
       Right r -> return $ Right r
    where opts = foldl (\defaults (name, value) ->
                        defaults & Wreq.param name .~ [fromString value]) Wreq.defaults optPairs

maybeGet :: String -> IO (Maybe String)
maybeGet url = maybeGetWith url []
