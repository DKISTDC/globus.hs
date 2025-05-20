{-# LANGUAGE QuasiQuotes #-}

module Network.Globus.Auth where

import Control.Monad.Catch (MonadCatch, MonadThrow)
import Data.Aeson
import Data.Aeson.Types
import Data.Function ((&))
import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Tagged
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Effectful (MonadIO)
import GHC.Generics (Generic)
import Network.Globus.Request as Request
import Network.Globus.Types
import Network.HTTP.Client (Manager, applyBasicAuth)
import Network.HTTP.Types (Header, methodPost)
import Network.URI.Static (uri)


-- | The end user must visit this url
authorizationUrl :: Token ClientId -> Uri Redirect -> NonEmpty Scope -> State -> Uri Authorization
authorizationUrl (Tagged cid) red scopes (State st) =
  -- TODO: does the auth url need the security token?
  authEndpoint /: "authorize"
    & param "client_id" cid
    & param "response_type" "code"
    & param "scope" (Text.intercalate " " (scopeText <$> NE.toList scopes))
    & param "state" st
    & redirectUri red


fetchAccessTokens :: (MonadIO m, MonadThrow m, MonadCatch m) => Manager -> Token ClientId -> Token ClientSecret -> Uri Redirect -> Token Exchange -> m (NonEmpty TokenItem)
fetchAccessTokens mgr (Tagged cid) (Tagged sec) red (Tagged code) = do
  req <- Request.request methodPost tokenUri [] ""
  TokenResponse toks <- sendJSON mgr (req & applyBasicAuth (encodeUtf8 cid) (encodeUtf8 sec))
  pure toks
 where
  tokenEndpoint :: Uri Tokens
  tokenEndpoint = Tagged $ [uri|https://auth.globus.org/v2/oauth2/token|]

  tokenUri :: Uri Tokens
  tokenUri =
    tokenEndpoint
      & param "grant_type" "authorization_code"
      & param "code" code
      & redirectUri red


redirectUri :: Uri Redirect -> Uri a -> Uri a
redirectUri red = param "redirect_uri" (renderUri red)


-- | fetchAccessTokens returns a non-empty list matching the scopes
newtype TokenResponse = TokenResponse (NonEmpty TokenItem)
  deriving (Show)


instance FromJSON TokenResponse where
  parseJSON = withObject "TokenResponse" $ \m -> do
    token <- parseJSON $ Object m :: Parser TokenItem
    other <- m .: "other_tokens"
    pure $ TokenResponse $ token :| other


scopeToken :: Scope -> NonEmpty TokenItem -> Maybe (Token Access)
scopeToken s ts = do
  item <- L.find (\i -> hasScope i.scope) $ NE.toList ts
  pure item.access_token
 where
  hasScope (Scopes ss) = s `elem` ss


-- | You MUST include the OpenId Scope for this to work
fetchUserInfo :: (MonadIO m, MonadCatch m, MonadThrow m) => Manager -> Token OpenId -> m UserInfoResponse
fetchUserInfo mgr to = do
  req <- Request.request methodPost (authEndpoint /: "userinfo") [identityAuth to] ""
  sendJSON mgr req


authEndpoint :: Uri Authorization
authEndpoint = Tagged $ [uri|https://auth.globus.org/v2/oauth2|]


identityAuth :: Token OpenId -> Header
identityAuth = oAuth2Bearer


--  where
--   tokenEndpoint :: Req.Url 'Https
--   tokenEndpoint = https "auth.globus.org" /: "v2" /: "oauth2" /: "token"
--
data UserInfoResponse = UserInfoResponse
  { info :: UserInfo
  , email :: Maybe UserEmail
  , profile :: Maybe UserProfile
  }


instance FromJSON UserInfoResponse where
  parseJSON = withObject "UserInfo" $ \m -> do
    info <- parseJSON $ Object m
    email <- m .:? "email"
    profile <- parseJSON $ Object m
    pure $ UserInfoResponse{info, email, profile}


data UserInfo = UserInfo
  { sub :: Text
  , last_authentication :: Int
  -- , identity_set :: Value
  }
  deriving (Generic, FromJSON, Show)


newtype UserEmail = UserEmail Text
  deriving newtype (FromJSON, Show, Eq)


data UserProfile = UserProfile
  { name :: Text
  , organization :: Text
  , preferred_username :: Text
  , identity_provider :: Text
  , identity_provider_display_name :: Text
  }
  deriving (Generic, FromJSON, Show)
