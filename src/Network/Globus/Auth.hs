module Network.Globus.Auth where

import Data.Aeson
import Data.Aeson.Types
import Data.ByteString (ByteString)
import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.String (IsString)
import Data.Tagged
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Effectful (MonadIO, liftIO)
import GHC.Generics (Generic)
import Network.Globus.Types
import Network.HTTP.Req as Req


-- | Opaque secret identifying the user. Validate on redirect
newtype State = State Text
  deriving newtype (IsString, FromJSON)
  deriving (Show)


-- | The end user must visit this url
authorizationUrl :: Token ClientId -> Uri Redirect -> NonEmpty Scope -> State -> Uri Authorization
authorizationUrl (Tagged cid) red scopes (State st) =
  -- TODO: does the auth url need the security token?
  authorizeEndpoint{params = query}
 where
  query :: Query
  query =
    "client_id" =: cid
      <> "response_type" =: ("code" :: Text)
      -- TODO: Hard-coded scopes?
      -- allow use to specify scopes?
      <> "scope" =: Text.intercalate " " (scopeText <$> NE.toList scopes)
      <> "state" =: st
      <> redirectUri red

  authorizeEndpoint :: Uri Authorization
  authorizeEndpoint = Uri Https "auth.globus.org" ["v2", "oauth2", "authorize"] (Query [])


fetchAccessTokens :: (MonadIO m) => Token ClientId -> Token ClientSecret -> Uri Redirect -> Token Exchange -> m (NonEmpty TokenItem)
fetchAccessTokens (Tagged cid) (Tagged sec) red (Tagged code) = do
  runReq defaultHttpConfig $ do
    res <-
      req POST tokenEndpoint NoReqBody jsonResponse $
        "grant_type" =: ("authorization_code" :: Text)
          <> "code" =: code
          <> redirectUri red
          -- TODO: is this the correct basic auth?
          <> basicAuth (encodeUtf8 cid) (encodeUtf8 sec)

    -- liftIO $ print $ responseBody res
    let TokenResponse toks = responseBody res :: TokenResponse
    pure toks
 where
  tokenEndpoint :: Req.Url 'Https
  tokenEndpoint = https "auth.globus.org" /: "v2" /: "oauth2" /: "token"


redirectUri :: (QueryParam param) => Uri Redirect -> param
redirectUri red = "redirect_uri" =: renderUri red


-- | fetchAccessTokens returns a non-empty list matching the scopes
newtype TokenResponse = TokenResponse (NonEmpty TokenItem)
  deriving (Show)


instance FromJSON TokenResponse where
  parseJSON = withObject "TokenResponse" $ \m -> do
    token <- parseJSON $ Object m :: Parser TokenItem
    other <- m .: "other_tokens"
    pure $ TokenResponse $ token :| other


data TokenItem = TokenItem
  { scope :: Scopes
  , access_token :: Token Access
  , expires_in :: Int
  , -- , resource_server :: Text -- "transfer.api.globus.org"
    -- , tokenType :: Text -- "Bearer"
    state :: State
    -- , refresh_token :: Token Refresh
    -- id_token :: Token Identity
  }
  deriving (Generic, FromJSON, Show)


scopeToken :: Scope -> NonEmpty TokenItem -> Maybe (Token Access)
scopeToken s ts = do
  item <- L.find (\i -> hasScope i.scope) $ NE.toList ts
  pure item.access_token
 where
  hasScope (Scopes ss) = s `elem` ss


-- | You MUST include the OpenId Scope for this to work
fetchUserInfo :: (MonadIO m) => Token OpenId -> m UserInfoResponse
fetchUserInfo to =
  runReq defaultHttpConfig $ do
    res <-
      req POST endpoint NoReqBody jsonResponse $
        identityAuth to
    pure $ responseBody res
 where
  endpoint :: Req.Url 'Https
  endpoint = https "auth.globus.org" /: "v2" /: "oauth2" /: "userinfo"


identityAuth :: Token OpenId -> Option Https
identityAuth (Tagged oid) = oAuth2Bearer (encodeUtf8 oid)


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
