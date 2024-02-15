{-# LANGUAGE OverloadedLists #-}

module Network.Globus where

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
newtype Token (a :: Token') = Token Text
  deriving newtype (IsString)
  deriving (Show)


data Token'
  = ClientId
  | ClientSecret
  | Exchange


-- | Opaque secret identifying the user. Validate on redirect
newtype State = State Text
  deriving newtype (IsString)


data Scope
  = -- TODO: figure out all scopes and hard-code
    TransferAll


data Endpoint
  = Redirect
  | Authorization
  | Tokens


-- | Simple URI Type, since all the others are obnoxious
data Uri (a :: Endpoint) = Uri
  { endpoint :: Text
  , params :: Query
  }


renderUri :: Uri a -> Text
renderUri u =
  case renderQuery u.params of
    "" -> u.endpoint
    q -> u.endpoint <> "?" <> q


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


redirect :: Text -> Uri Redirect
redirect r = Uri r (Query [])


-- | The end user must visit this url
authorizationUrl :: Token ClientId -> Uri Redirect -> State -> Uri Authorization
authorizationUrl (Token cid) red (State st) =
  -- TODO: does the auth url need the security token?
  authorizeEndpoint{params = query}
 where
  query :: Query
  query =
    "response_type" =: ("code" :: Text)
      <> "state" =: st
      -- TODO: Hard-coded scopes
      <> "scope" =: ("urn:globus:auth:scope:transfer.api.globus.org:all" :: Text)
      <> "client_id" =: cid
      <> "redirect_url" =: renderUri red

  authorizeEndpoint :: Uri Authorization
  authorizeEndpoint = Uri "https://auth.globus.org/v2/oauth2/authorize" []


fetchAccessToken :: Token ClientId -> Token ClientSecret -> (MonadIO m) => m TokenResponse
fetchAccessToken (Token cid) (Token sec) =
  -- TODO: Make this correct from documentation. Secret?
  runReq defaultHttpConfig $ do
    res <-
      req POST tokenEndpoint (ReqBodyBs "woot") jsonResponse $
        "woot" =: ("boot" :: Text)
    pure $ responseBody res
 where
  tokenEndpoint :: Req.Url 'Https
  tokenEndpoint = https "auth.globus.org" /: "v2" /: "oauth2" /: "token"


data TokenResponse = TokenResponse
  -- TODO: Get transfer access token
  -- TODO: Get api access token
  deriving (Generic, FromJSON)

-- data GlobusError
--   = TokenError TokenResponseError
--   deriving (Show, Exception)

-- https://nso.edu/?code=NaiIGkfqUSjTEuBxqToBmRBzZYTUCf&state=state

-- OAuth2Token {accessToken = AccessToken {atoken = "Ag3Jbgmv1bGEyEmX4y59mMz577yrBbvv1veJ1ayP2x10gw01b0fVCb5g0DWMJEO013NmWGqwEPqpnXhdmMP1VUQJg9K"}, refreshToken = Nothing, expiresIn = Just 172800, tokenType = Just "Bearer", idToken = Nothing}
