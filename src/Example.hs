module Example where

import Data.Text (pack, unpack)
import Network.Globus
import System.Environment


-- Modern Research Data Portal
-- https://docs.globus.org/guides/recipes/modern-research-data-portal/
-- https://github.com/globus/globus-sample-data-portal

main :: IO ()
main = do
  cid <- Token . pack <$> getEnv "GLOBUS_CLIENT_ID"
  sec <- Token . pack <$> getEnv "GLOBUS_CLIENT_SECRET"
  print cid
  print sec

  let red = Uri Https "thunderbolts.dev.dkistdc.nso.edu" ["redirect"] (Query [])
  -- print red
  --
  -- let cfg = config (Token (pack cid)) (Token (pack sec)) red
  --
  let authUrl = authorizationUrl cid red [TransferAll] (State "state")
  putStrLn $ unpack $ renderUri authUrl

  -- putStrLn "TOKEN REQUEST"
  -- putStrLn "-------------"
  -- res <- fetchAccessToken cid sec red (Token "M0EbLfeI64drpU7eWtIhiQx7YkzlWY")
  -- print res

  putStrLn "SCOPES"
  putStrLn "-------------"

-- testFetchScopes cid sec

-- res <- fetchAccessToken cfg (Token "NaiIGkfqUSjTEuBxqToBmRBzZYTUCf")
-- print res
--
--
-- https://nso.edu/?code=M0EbLfeI64drpU7eWtIhiQx7YkzlWY&state=state
--
--
-- email
-- profile
-- openid
-- urn:globus:auth:scope:auth.globus.org:view_identities+openid+email+profile
-- urn:globus:auth:scope:auth.globus.org:view_identities
-- urn:globus:auth:scope:nexus.api.globus.org:groups
-- urn:globus:auth:scope:atmosphere.jetstream.xsede.org:manage_data
-- urn:globus:auth:scope:transfer.api.globus.org:monitor_ongoing
-- urn:globus:auth:scope:groups.api.globus.org:view_my_groups_and_memberships
-- urn:globus:auth:scope:transfer.api.globus.org:all
-- urn:globus:auth:scope:groups.api.globus.org:all
