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
  AuthUrl :: [Scope] -> State -> Globus m (Uri Authorization)
  AccessToken :: Token Exchange -> Globus m (Token Access)
  SubmissionId :: Token Access -> Globus m (Id Submission)
  Transfer :: Token Access -> TransferRequest -> Globus m TransferResponse
  StatusTask :: Token Access -> Id Task -> Globus m Task
  StatusTasks :: Token Access -> TaskFilters -> Globus m TaskList


type instance DispatchOf Globus = 'Dynamic


runGlobus
  :: (IOE :> es)
  => GlobusClient
  -> Uri Redirect
  -> Eff (Globus : es) a
  -> Eff es a
runGlobus g red = interpret $ \_ -> \case
  AccessToken exc -> do
    liftIO $ fetchAccessToken g.clientId g.clientSecret red exc
  AuthUrl scopes state -> do
    pure $ authorizationUrl g.clientId red scopes state
  SubmissionId access -> do
    liftIO $ fetchSubmissionId access
  Transfer access request -> do
    liftIO $ sendTransfer access request
  StatusTask access ti -> do
    liftIO $ fetchTask access ti
  StatusTasks access tf -> do
    liftIO $ fetchTasks access tf
