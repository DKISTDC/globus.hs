module Network.Globus.Types where

import Control.Monad.Catch (Exception)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy (ByteString)
import Data.Char (toLower)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy (Proxy (..))
import Data.String (IsString (..))
import Data.Tagged
import Data.Text (Text, pack, splitOn, unpack)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic, Rep)
import GHC.TypeLits
import Network.HTTP.Client (Request)
import Network.HTTP.Types (Status, urlEncode)
import Network.URI
import System.FilePath


(/:) :: Uri a -> String -> Uri a
Tagged uri /: s = Tagged $ uri{uriPath = uri.uriPath </> s}
infixl 5 /:


-- (?:) :: Uri a -> QueryItem -> Uri a
-- Tagged uri ?: (qk, mqv) =
--  where

param :: Text -> Text -> Uri a -> Uri a
param k v (Tagged uri) = Tagged $ uri{uriQuery = appendQuery k (Just v) uri.uriQuery}


appendQuery :: Text -> Maybe Text -> String -> String
appendQuery k mv = \case
  "" -> "?" <> keyValue
  "?" -> "?" <> keyValue
  rest -> rest <> "&" <> keyValue
 where
  keyValue =
    BC.unpack $
      (urlEncode True $ encodeUtf8 k)
        <> "="
        <> maybe "" (urlEncode True . encodeUtf8) mv


renderUri :: Uri a -> Text
renderUri (Tagged u) = pack $ uriToString id u ""


-- | Opaque secret identifying the user. Validate on redirect
newtype State = State Text
  deriving newtype (IsString, FromJSON, Eq)
  deriving (Show)


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
  deriving (Generic, FromJSON, Eq, Show)


data GlobusError
  = InvalidURI String URI
  | Unauthorized Request ByteString
  | ResponseBadStatus Request Status ByteString
  | ResponseBadJSON Request String ByteString
  | MissingScope Scope (NonEmpty TokenItem)
  deriving (Show, Exception)


type Token a = Tagged a Text
type Id a = Tagged a Text
type Uri a = Tagged a URI


data Token'
  = ClientId
  | ClientSecret
  | Exchange
  | Access


data Id'
  = Submission
  | Request
  | Collection


dataLabelsToJSON :: (Generic a, GToJSON' Value Zero (Rep a)) => a -> Value
dataLabelsToJSON = genericToJSON defaultOptions{fieldLabelModifier = dataLabels}


dataLabelsFromJSON :: (Generic a, GFromJSON Zero (Rep a)) => Value -> Parser a
dataLabelsFromJSON = genericParseJSON defaultOptions{fieldLabelModifier = dataLabels}


dataLabels :: String -> String
dataLabels "data_" = "DATA"
dataLabels "data_type" = "DATA_TYPE"
dataLabels f = f


data DataType (s :: Symbol) = DataType


instance (KnownSymbol s) => ToJSON (DataType s) where
  toJSON _ = String $ pack $ symbolVal @s Proxy
instance FromJSON (DataType s) where
  parseJSON _ = pure DataType


data DataKey s = DataKey
  { data_type :: DataType s
  , value :: Text
  }
  deriving (Generic)
instance FromJSON (DataKey s) where
  parseJSON = dataLabelsFromJSON


data Endpoint
  = Redirect
  | Authorization
  | Tokens
  | App


-- -- | Simple URI Type, since all the others are obnoxious
-- data Uri (a :: Endpoint) = Uri
--   { scheme :: Scheme
--   , domain :: Text
--   , path :: [Text]
--   , params :: Query
--   }
--
--
-- renderUri :: Uri a -> Text
-- renderUri u =
--   scheme <> endpoint <> path <> query
--  where
--   scheme =
--     case u.scheme of
--       Http -> "http://"
--       Https -> "https://"
--   endpoint = cleanSlash u.domain
--   path = "/" <> Text.intercalate "/" (map cleanSlash u.path)
--   query =
--     case renderQuery u.params of
--       "" -> ""
--       q -> "?" <> q
--   cleanSlash = Text.dropWhileEnd (== '/') . Text.dropWhile (== '/')
--
--
-- instance Show (Uri a) where
--   show = Text.unpack . renderUri

-- newtype Query = Query [(Text, Maybe Text)]
--   deriving newtype (Monoid, Semigroup)
--
--
-- instance Show Query where
--   show = Text.unpack . renderQuery
--
--
-- instance IsList Query where
--   type Item Query = (Text, Maybe Text)
--   fromList = Query
--   toList (Query ps) = ps
--
--
-- -- instance Req.QueryParam Query where
-- --   queryParam t ma = Query [(t, toQueryParam <$> ma)]
-- --   queryParamToList (Query ps) = ps
--
-- renderQuery :: Query -> Text
-- renderQuery (Query ps) = Text.intercalate "&" $ map toText ps
--  where
--   toText (p, Nothing) = p
--   toText (p, Just v) = p <> "=" <> value v
--
--   value = decodeUtf8 . urlEncode True . encodeUtf8

data Scope
  = -- TODO: figure out all scopes and hard-code
    TransferAll
  | Identity ScopeIdentity
  deriving (Show, Eq)


data ScopeIdentity
  = OpenId
  | Email
  | Profile
  deriving (Show, Eq)


scopeText :: Scope -> Text
scopeText TransferAll = "urn:globus:auth:scope:transfer.api.globus.org:all"
scopeText (Identity i) = pack $ toLower <$> show i


scope :: Text -> Maybe Scope
scope "urn:globus:auth:scope:transfer.api.globus.org:all" = Just TransferAll
scope "email" = Just $ Identity Email
scope "profile" = Just $ Identity Profile
scope "openid" = Just $ Identity OpenId
scope _ = Nothing


instance FromJSON Scope where
  parseJSON = withText "Scope" $ \t -> do
    maybe (fail $ "Invalid scope:" <> unpack t) pure $ scope t


newtype Scopes = Scopes (NonEmpty Scope)
  deriving newtype (Show, Eq)


instance FromJSON Scopes where
  parseJSON = withText "Scopes" $ \t -> do
    ts <- parseSplitSpace t
    ss <- mapM (parseJSON . String) ts
    pure $ Scopes ss
   where
    parseSplitSpace t = do
      case splitOn " " t of
        (s : ss) -> pure $ s :| ss
        _ -> fail $ "Scopes split on spaces " <> unpack t
