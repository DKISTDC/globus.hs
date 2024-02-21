module Effectful.Globus
  ( module Network.Globus
  , Globus (..)
  , GlobusClient (..)
  , runGlobus
  ) where

import Effectful
import Effectful.Dispatch.Dynamic
import Network.Globus


data GlobusClient = GlobusClient
  { clientId :: Token ClientId
  , clientSecret :: Token ClientSecret
  }


data Globus :: Effect where
  AuthUrl :: [Scope] -> State -> Globus m (Uri Authorization)
  AccessToken :: Token Exchange -> Globus m (Token Access)


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
