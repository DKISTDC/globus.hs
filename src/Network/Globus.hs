{-# LANGUAGE OverloadedLists #-}

module Network.Globus
  ( Token (..)
  , Token' (..)
  , State (..)
  , Scope (..)
  , scope
  , Endpoint (..)
  , Uri (..)
  , renderUri
  , Query (..)
  , Scheme (..)
  , authorizationUrl
  , fetchAccessToken
  ) where

import Control.Monad.Except
import Data.Aeson (FromJSON)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics
import GHC.IsList (IsList (..))
import Network.HTTP.Req as Req
import Network.HTTP.Types (urlEncode)
import Web.HttpApiData (toQueryParam)


-- | Tagged tokens
newtype Token (a :: Token') = Token {text :: Text}
  deriving newtype (IsString, FromJSON)
  deriving (Show)


data Token'
  = ClientId
  | ClientSecret
  | Exchange
  | Access


-- | Opaque secret identifying the user. Validate on redirect
newtype State = State Text
  deriving newtype (IsString, FromJSON)
  deriving (Show)


data Scope
  = -- TODO: figure out all scopes and hard-code
    TransferAll


scopeText :: Scope -> Text
scopeText TransferAll = "urn:globus:auth:scope:transfer.api.globus.org:all"


scope :: Text -> Maybe Scope
scope "urn:globus:auth:scope:transfer.api.globus.org:all" = Just TransferAll
scope _ = Nothing


data Endpoint
  = Redirect
  | Authorization
  | Tokens


-- | Simple URI Type, since all the others are obnoxious
data Uri (a :: Endpoint) = Uri
  { scheme :: Scheme
  , domain :: Text
  , path :: [Text]
  , params :: Query
  }


renderUri :: Uri a -> Text
renderUri u =
  scheme <> endpoint <> path <> query
 where
  scheme =
    case u.scheme of
      Http -> "http://"
      Https -> "https://"
  endpoint = cleanSlash u.domain
  path = "/" <> Text.intercalate "/" (map cleanSlash u.path)
  query =
    case renderQuery u.params of
      "" -> ""
      q -> "?" <> q
  cleanSlash = Text.dropWhileEnd (== '/') . Text.dropWhile (== '/')


instance Show (Uri a) where
  show = Text.unpack . renderUri


newtype Query = Query [(Text, Maybe Text)]
  deriving newtype (Monoid, Semigroup)


instance Show Query where
  show = Text.unpack . renderQuery


instance IsList Query where
  type Item Query = (Text, Maybe Text)
  fromList = Query
  toList (Query ps) = ps


instance Req.QueryParam Query where
  queryParam t ma = Query [(t, toQueryParam <$> ma)]
  queryParamToList (Query ps) = ps


renderQuery :: Query -> Text
renderQuery (Query ps) = Text.intercalate "&" $ map toText ps
 where
  toText (p, Nothing) = p
  toText (p, Just v) = p <> "=" <> value v

  value = decodeUtf8 . urlEncode True . encodeUtf8


-- | The end user must visit this url
authorizationUrl :: Token ClientId -> Uri Redirect -> [Scope] -> State -> Uri Authorization
authorizationUrl (Token cid) red scopes (State st) =
  -- TODO: does the auth url need the security token?
  authorizeEndpoint{params = query}
 where
  query :: Query
  query =
    "client_id" =: cid
      <> "response_type" =: ("code" :: Text)
      -- TODO: Hard-coded scopes?
      -- allow use to specify scopes?
      <> "scope" =: Text.intercalate " " (map scopeText scopes)
      <> "state" =: st
      <> redirectUri red

  authorizeEndpoint :: Uri Authorization
  authorizeEndpoint = Uri Https "auth.globus.org" ["v2", "oauth2", "authorize"] []


-- Depends on Scopes as well
-- use Effects? (Reader, etc)
fetchAccessToken :: (MonadIO m) => Token ClientId -> Token ClientSecret -> Uri Redirect -> Token Exchange -> m (Token Access)
fetchAccessToken (Token cid) (Token sec) red (Token code) =
  -- TODO: Make this correct from documentation. Secret?
  runReq defaultHttpConfig $ do
    res <-
      req POST tokenEndpoint (ReqBodyBs "woot") jsonResponse $
        "grant_type" =: ("authorization_code" :: Text)
          <> "code" =: code
          <> redirectUri red
          -- TODO: is this the correct basic auth?
          <> basicAuth (encodeUtf8 cid) (encodeUtf8 sec)

    -- TODO: verify that we received expected scopes
    -- TODO: verify that state matches what was sent in authorization request
    -- TODO: store token for future use with expiry time
    let tokRes = responseBody res :: TokenResponse
    pure tokRes.access_token
 where
  tokenEndpoint :: Req.Url 'Https
  tokenEndpoint = https "auth.globus.org" /: "v2" /: "oauth2" /: "token"


-- testFetchScopes :: Token ClientId -> Token ClientSecret -> IO ()
-- testFetchScopes (Token cid) (Token sec) = do
--   putStrLn "test"
--   res <- runReq defaultHttpConfig $ do
--     req GET scopesEndpoint NoReqBody bsResponse $
--       basicAuth (encodeUtf8 cid) (encodeUtf8 sec)
--   print (responseBody res)
--  where
--   scopesEndpoint :: Req.Url 'Https
--   scopesEndpoint = https "auth.globus.org" /: "v2" /: "api" /: "scopes"

redirectUri :: (QueryParam param) => Uri Redirect -> param
redirectUri red = "redirect_uri" =: renderUri red


-- basicAuth :: ByteString
-- basicAuth =
--   let enc = Base64.encode $ encodeUtf8 cid <> ":" <> encodeUtf8 sec
--    in "Basic " <> enc

-- returns the first requested scope first
-- then the rest in the otherTokens
data TokenResponse = TokenResponse
  { access_token :: Token Access
  , expires_in :: Int
  , -- , resource_server :: Text -- "transfer.api.globus.org"
    -- , tokenType :: Text -- "Bearer"
    state :: State
  , scope :: Text -- TODO: scopes object
  , other_tokens :: [TokenResponse]
  -- , refresh_token :: Token Refresh
  -- , idToken :: _
  }
  -- TODO: Get transfer access token
  -- TODO: Get api access token
  deriving (Generic, FromJSON, Show)

-- data GlobusError
--   = TokenError TokenResponseError
--   deriving (Show, Exception)

-- https://nso.edu/?code=NaiIGkfqUSjTEuBxqToBmRBzZYTUCf&state=state

-- OAuth2Token {accessToken = AccessToken {atoken = "Ag3Jbgmv1bGEyEmX4y59mMz577yrBbvv1veJ1ayP2x10gw01b0fVCb5g0DWMJEO013NmWGqwEPqpnXhdmMP1VUQJg9K"}, refreshToken = Nothing, expiresIn = Just 172800, tokenType = Just "Bearer", idToken = Nothing}
