module Effectful.Globus
  ( Globus (..)
  , GlobusClient (..)
  , runGlobus
  , State (..)
  , Req.Scheme (..)
  , Tagged (..)
  , module Network.Globus.Types
  , TransferRequest (..)
  , TransferResponse (..)
  , TransferItem (..)
  , SyncLevel (..)
  , Task (..)
  , TaskStatus (..)
  , TaskFilters (..)
  , TaskList (..)
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.Tagged
import Effectful
import Effectful.Dispatch.Dynamic
import Network.Globus.Auth
import Network.Globus.Transfer
import Network.Globus.Types
import Network.HTTP.Req as Req


data GlobusClient = GlobusClient
  { clientId :: Token ClientId
  , clientSecret :: Token ClientSecret
  }


data Globus :: Effect where
  AuthUrl :: Uri Redirect -> NonEmpty Scope -> State -> Globus m (Uri Authorization)
  GetUserInfo :: Token OpenId -> Globus m UserInfoResponse
  GetAccessTokens :: Token Exchange -> Uri Redirect -> Globus m (NonEmpty TokenItem)
  SubmissionId :: Token Access -> Globus m (Id Submission)
  Transfer :: Token Access -> TransferRequest -> Globus m TransferResponse
  StatusTask :: Token Access -> Id Task -> Globus m Task
  StatusTasks :: Token Access -> TaskFilters -> Globus m TaskList


type instance DispatchOf Globus = 'Dynamic


runGlobus
  :: (IOE :> es)
  => GlobusClient
  -> Eff (Globus : es) a
  -> Eff es a
runGlobus g = interpret $ \_ -> \case
  GetAccessTokens exc red -> do
    liftIO $ fetchAccessTokens g.clientId g.clientSecret red exc
  GetUserInfo ti -> do
    liftIO $ fetchUserInfo ti
  AuthUrl red scopes state -> do
    pure $ authorizationUrl g.clientId red scopes state
  SubmissionId access -> do
    liftIO $ fetchSubmissionId access
  Transfer access request -> do
    liftIO $ sendTransfer access request
  StatusTask access ti -> do
    liftIO $ fetchTask access ti
  StatusTasks access tf -> do
    liftIO $ fetchTasks access tf
