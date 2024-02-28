module Network.Globus
  ( module Network.Globus.Types
  , Tagged (..)
  , module Network.Globus.Auth
  , Req.Scheme (..)
  , module Network.Globus.Transfer
  ) where

import Data.Tagged (Tagged (..))
import Network.Globus.Auth
import Network.Globus.Transfer
import Network.Globus.Types
import Network.HTTP.Req as Req


-- data GlobusError
--   = TokenError TokenResponseError
--   deriving (Show, Exception)

-- https://nso.edu/?code=NaiIGkfqUSjTEuBxqToBmRBzZYTUCf&state=state

-- OAuth2Token {accessToken = AccessToken {atoken = "Ag3Jbgmv1bGEyEmX4y59mMz577yrBbvv1veJ1ayP2x10gw01b0fVCb5g0DWMJEO013NmWGqwEPqpnXhdmMP1VUQJg9K"}, refreshToken = Nothing, expiresIn = Just 172800, tokenType = Just "Bearer", idToken = Nothing}
