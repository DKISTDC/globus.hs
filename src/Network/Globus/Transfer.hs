module Network.Globus.Transfer where

import Data.Aeson
import Data.Tagged
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Effectful (MonadIO)
import GHC.Generics (Generic, Rep)
import Network.Globus.Types
import Network.HTTP.Req as Req


-- -----------------------------------------
-- API
-- -----------------------------------------

fetchSubmissionId :: (MonadIO m) => Token Access -> m (Id Submission)
fetchSubmissionId access =
  runReq defaultHttpConfig $ do
    res <- req GET (transferEndpoint /: "submission_id") NoReqBody jsonResponse (transferAuth access)
    let idRes = responseBody res :: IdResponse
    pure $ Tagged idRes.value


transferAuth :: Token Access -> Option Https
transferAuth (Tagged access) = oAuth2Bearer (encodeUtf8 access)


transferEndpoint :: Req.Url 'Https
transferEndpoint = https "transfer.api.globus.org" /: "v0.10"


sendTransfer :: (MonadIO m) => Token Access -> TransferRequest -> m TransferResponse
sendTransfer access request =
  runReq defaultHttpConfig $ do
    res <- req POST (transferEndpoint /: "transfer") (ReqBodyJson request) jsonResponse (transferAuth access)
    let tr = responseBody res :: TransferResponse
    pure tr


-- -----------------------------------------
-- Submission Ids
-- -----------------------------------------

data IdResponse = IdResponse
  { value :: Text
  }
  deriving (Generic, FromJSON)


-- -----------------------------------------
-- Transfers
-- -----------------------------------------

data TransferResponse = TransferResponse
  { task_id :: Id Task
  , submission_id :: Token Submission
  , -- , code :: TransferCode -- Accepted, Duplicate
    message :: Text
  , resource :: Text
  , request_id :: Token Request
  }
  deriving (Generic, FromJSON)


-- https://docs.globus.org/api/transfer/task_submit/#transfer_and_delete_documents
data TransferRequest = TransferRequest
  { data_type :: DataType "transfer"
  , submission_id :: Id Submission
  , label :: Maybe Text
  , -- , notify_on_succeeded :: Bool -- Default true
    -- , notify_on_failed :: Bool -- Default true
    -- deadline
    source_endpoint :: Id Collection
  , destination_endpoint :: Id Collection
  , data_ :: [TransferItem]
  , -- , filter_rules :: [FilterRule]
    sync_level :: SyncLevel
    -- , encrypt_data
  }
  deriving (Generic)


instance ToJSON TransferRequest where
  toJSON = dataLabelsJson


-- https://docs.globus.org/api/transfer/task_submit/#transfer_item_fields
data TransferItem = TransferItem
  { data_type :: DataType "transfer_item"
  , source_path :: FilePath
  , destination_path :: FilePath
  , recursive :: Bool
  -- , external_checksum :: Text
  -- , checksum_algorithm :: Text
  -- , verify_checksum = false
  -- preserve_timestamp = false
  -- delete_destination_extra = false
  -- recursive_symlinks
  -- skip_source-errors ?? default?
  -- fail_on_quota_errors ??
  }
  deriving (Generic)


instance ToJSON TransferItem where
  toJSON = dataLabelsJson


-- newtype FilterRule = FilterRule
--   { data_type :: DataType "filter_rule"
--   -- , method :: FilterMethod
--   -- , type_ :: FilterType
--   -- , name :: Text
--   }
--   deriving (Generic, ToJSON)

data SyncLevel
  = SyncExists
  | SyncSize
  | SyncTimestamp
  | SyncChecksum


instance ToJSON SyncLevel where
  toJSON = Number . toInt
   where
    toInt SyncExists = 0
    toInt SyncSize = 1
    toInt SyncTimestamp = 2
    toInt SyncChecksum = 3


dataLabelsJson :: (Generic a, GToJSON' Value Zero (Rep a)) => a -> Value
dataLabelsJson = genericToJSON defaultOptions{fieldLabelModifier = dataLabels}


dataLabels :: String -> String
dataLabels "data_" = "DATA"
dataLabels "data_type" = "DATA_TYPE"
dataLabels f = f
