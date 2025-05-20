module Effectful.Globus
  ( Globus (..)
  , GlobusClient (..)
  , runGlobus
  , State (..)
  , Tagged (..)
  , TransferRequest (..)
  , TransferResponse (..)
  , TransferItem (..)
  , SyncLevel (..)
  , Task (..)
  , TaskStatus (..)
  , TaskFilters (..)
  , TaskList (..)
  , module Network.Globus.Types
  , requireScopeToken
  ) where

import Control.Monad.Catch (catch)
import Data.List.NonEmpty (NonEmpty)
import Data.Tagged
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Network.Globus.Auth
import Network.Globus.Transfer
import Network.Globus.Types hiding (appendQuery, param)
import Network.HTTP.Client (Manager)


data GlobusClient = GlobusClient
  { clientId :: Token ClientId
  , clientSecret :: Token ClientSecret
  }


data Globus :: Effect where
  AuthUrl :: Uri Redirect -> NonEmpty Scope -> State -> Globus m (Uri Authorization)
  GetUserInfo :: Token OpenId -> Globus m UserInfoResponse
  GetAccessTokens :: Token Exchange -> Uri Redirect -> Globus m (NonEmpty TokenItem)
  GetSubmissionId :: Token Access -> Globus m (Id Submission)
  Transfer :: Token Access -> TransferRequest -> Globus m TransferResponse
  StatusTask :: Token Access -> Id Task -> Globus m Task
  StatusTasks :: Token Access -> TaskFilters -> Globus m TaskList


type instance DispatchOf Globus = 'Dynamic


runGlobus
  :: (IOE :> es, Error GlobusError :> es)
  => GlobusClient
  -> Manager
  -> Eff (Globus : es) a
  -> Eff es a
runGlobus g mgr = interpret $ \_ -> \case
  GetAccessTokens exc red -> do
    runGlobusIO $ fetchAccessTokens mgr g.clientId g.clientSecret red exc
  GetUserInfo ti -> do
    runGlobusIO $ fetchUserInfo mgr ti
  AuthUrl red scopes state -> do
    pure $ authorizationUrl g.clientId red scopes state
  GetSubmissionId access -> do
    runGlobusIO $ fetchSubmissionId mgr access
  Transfer access request -> do
    runGlobusIO $ sendTransfer mgr access request
  StatusTask access ti -> do
    runGlobusIO $ fetchTask mgr access ti
  StatusTasks access tf -> do
    runGlobusIO $ fetchTasks mgr access tf
 where
  onGlobusErr :: (Error GlobusError :> es) => GlobusError -> Eff es a
  onGlobusErr = throwError

  runGlobusIO :: (IOE :> es, Error GlobusError :> es) => IO a -> Eff es a
  runGlobusIO ma = catch (liftIO ma) onGlobusErr


requireScopeToken :: (Error GlobusError :> es) => Scope -> NonEmpty TokenItem -> Eff es (Token a)
requireScopeToken s tis = do
  Tagged t <- maybe (throwError $ MissingScope s tis) pure $ scopeToken s tis
  pure $ Tagged t
