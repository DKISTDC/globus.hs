module Network.Globus.Auth where

import Data.Aeson
import Data.String (IsString)
import Data.Tagged
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Effectful (MonadIO)
import GHC.Generics (Generic)
import Network.Globus.Types
import Network.HTTP.Req as Req


-- | Opaque secret identifying the user. Validate on redirect
newtype State = State Text
  deriving newtype (IsString, FromJSON)
  deriving (Show)


-- | The end user must visit this url
authorizationUrl :: Token ClientId -> Uri Redirect -> [Scope] -> State -> Uri Authorization
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
      <> "scope" =: Text.intercalate " " (map scopeText scopes)
      <> "state" =: st
      <> redirectUri red

  authorizeEndpoint :: Uri Authorization
  authorizeEndpoint = Uri Https "auth.globus.org" ["v2", "oauth2", "authorize"] (Query [])


-- Depends on Scopes as well
-- use Effects? (Reader, etc)
fetchAccessToken :: (MonadIO m) => Token ClientId -> Token ClientSecret -> Uri Redirect -> Token Exchange -> m (Token Access)
fetchAccessToken (Tagged cid) (Tagged sec) red (Tagged code) =
  runReq defaultHttpConfig $ do
    res <-
      req POST tokenEndpoint NoReqBody jsonResponse $
        "grant_type" =: ("authorization_code" :: Text)
          <> "code" =: code
          <> redirectUri red
          -- TODO: is this the correct basic auth?
          <> basicAuth (encodeUtf8 cid) (encodeUtf8 sec)

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
