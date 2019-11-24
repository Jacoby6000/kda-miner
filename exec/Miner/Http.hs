{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Miner.Http(getJSON, getWithBody, post) where

import           Control.Lens
import           Control.Monad.Catch
import           Miner.Types
import           Data.Aeson
import           Data.Bifunctor
import qualified Data.Text as T
import qualified Data.List as L
import           Data.ByteString.Lazy
import qualified Data.ByteString.Char8 as B8
import qualified Network.HTTP.Client as C
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.Wreq as W
import qualified Network.Wreq.Types as WT
import Prelude as P

url :: HostAddress -> String -> String
url (HostAddress h p) path = "https://" <> T.unpack h <> ":" <> show p <> path

withParams :: W.Options -> [(T.Text, [T.Text])] -> W.Options
withParams = L.foldl (\acc (k, vs) -> acc & W.param k .~ vs)

httpOpts :: W.Options
httpOpts = W.defaults & W.manager .~ Left (TLS.mkManagerSettings tlsSettings Nothing)

post :: WT.Postable a => HostAddress -> String -> a -> IO (Either T.Text ByteString)
post addr path a = second C.responseBody <$> readResponse (W.postWith httpOpts (url addr path) a)

getWithBody :: (Show a, WT.Postable a) => HostAddress -> String -> a -> IO (Either T.Text ByteString)
getWithBody addr path a = second C.responseBody <$> readResponse (W.customPayloadMethodWith "GET" httpOpts (url addr path) a)

getJSON :: FromJSON a => HostAddress -> String -> IO (Either T.Text a)
getJSON addr path = getJSONWith addr path []

getJSONWith :: FromJSON a => HostAddress -> String -> [(T.Text, [T.Text])] -> IO (Either T.Text a)
getJSONWith addr path params = 
  second C.responseBody <$> readJSONResponse (W.getWith (withParams httpOpts params) (url addr path))

readJSONResponse :: FromJSON a => IO (W.Response ByteString) -> IO (Either T.Text (W.Response a))
readJSONResponse responseIO = (readJSON =<< readResponse responseIO) `catch` liftHandler handleJsonError
 where 
  readJSON = traverse W.asJSON

  handleJsonError :: W.JSONError -> String
  handleJsonError (W.JSONError s) = "Bad JSON returned from server: " <> s


readResponse :: IO (W.Response ByteString) -> IO (Either T.Text (W.Response ByteString))
readResponse responseIO = (Right <$> responseIO) `catch` liftHandler handleHttpError
 where
  handleHttpError :: C.HttpException -> String
  handleHttpError (C.InvalidUrlException u message) = "URL " <> u <> " is malformed: " <> message
  handleHttpError (C.HttpExceptionRequest _ ex) = case ex of
    C.ConnectionTimeout -> "HTTP connection timed out."
    C.ResponseTimeout -> "HTTP response timed out."
    C.StatusCodeException res _ -> 
      "HTTP Request failed with code " 
      <> show (res ^. (W.responseStatus . W.statusCode))
      <> ": " <> B8.unpack (res ^. (W.responseStatus . W.statusMessage))
    other -> "Unhandled HTTP Error: " <> show other

liftHandler :: Applicative f => (a -> String) -> a -> f (Either T.Text b)
liftHandler f = pure . Left . T.pack . f
