{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Miner.Http(NodeInfo(..)) where

import           Data.Bifunctor
import           Control.Lens
import           Control.Monad.Catch
import           Miner.Types
import           Data.Aeson
import           Data.Text.Lazy as LT
import qualified Data.Text as T
import           GHC.Generics
import           Data.ByteString.Lazy
import qualified Data.ByteString.Char8 as B8
import qualified Network.HTTP.Types.Status as S
import qualified Network.HTTP.Client as C
import qualified Network.Wreq as W

data NodeInfo = NodeInfo
  { nodeVersion :: Text
  , nodeApiVersion :: Text
  , nodeChains :: [Text]
  , nodeNumberOfChains :: !Int
  } deriving (Generic)

instance FromJSON NodeInfo

get :: FromJSON a => HostAddress -> String -> IO (Either Text a)
get addr path = readResponse $ W.get (url addr)
  where
    url (HostAddress h p) = "https://" <> T.unpack h <> show p <> path


readResponse :: FromJSON a => IO (W.Response ByteString) -> IO (Either Text a)
readResponse responseIO = 
  (readJSON =<< responseIO) 
    `catch` liftHandler handleHttpError 
    `catch` liftHandler handleJsonError
 where
  liftHandler f = pure . Left . LT.pack . f

  readJSON response = Right . C.responseBody <$> W.asJSON response

  handleJsonError :: W.JSONError -> String
  handleJsonError (W.JSONError s) = "Bad JSON returned from server: " <> s

  handleHttpError :: C.HttpException -> String
  handleHttpError (C.InvalidUrlException url message) = "URL " <> url <> " is malformed: " <> message
  handleHttpError (C.HttpExceptionRequest _ ex) = case ex of
    C.ConnectionTimeout -> "HTTP connection timed out."
    C.ResponseTimeout -> "HTTP response timed out."
    C.StatusCodeException res _ -> 
      "HTTP Request failed with code " 
      <> show (res ^. W.responseStatus ^. W.statusCode) 
      <> ": " <> B8.unpack (res ^. W.responseStatus ^. W.statusMessage)
    other -> "Unhandled HTTP Error: " <> show other
