module Network.Globus.Request where

import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch, MonadThrow, SomeException, catch, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON (..), ToJSON (..), eitherDecode, encode)
import Data.Tagged
import Data.Text.Encoding (encodeUtf8)
import Network.Globus.Types
import Network.HTTP.Client as Http
import Network.HTTP.Types


request :: (MonadThrow m, MonadCatch m) => Method -> Uri a -> [Header] -> RequestBody -> m Http.Request
request m (Tagged u) hs body = do
  req <- catch (Http.requestFromURI u) invalidUri
  pure $ req{method = m, requestHeaders = hs, requestBody = body}
 where
  invalidUri :: (MonadThrow m) => SomeException -> m a
  invalidUri e = throwM $ InvalidURI (show e) u


get :: (MonadThrow m, MonadCatch m) => Uri a -> [Header] -> m Http.Request
get u hs = request methodGet u hs ""


post :: (MonadThrow m, MonadCatch m, ToJSON b) => Uri a -> [Header] -> b -> m Http.Request
post u hs b = request methodPost u hs (RequestBodyLBS $ encode b)


sendJSON :: (MonadIO m, FromJSON a, MonadThrow m) => Manager -> Request -> m a
sendJSON mgr req = do
  res <- liftIO $ Http.httpLbs req mgr

  unless (responseStatus res == status200) $ do
    throwM $ ResponseBadStatus (responseStatus res) (responseBody res)

  case eitherDecode (responseBody res) of
    Left e -> throwM $ ResponseBadJSON (show e) (responseBody res)
    Right a -> pure a


oAuth2Bearer :: Token a -> Header
oAuth2Bearer (Tagged tok) = ("Authorization", "Bearer " <> encodeUtf8 tok)


