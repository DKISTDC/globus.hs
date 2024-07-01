module Example where

import Data.Tagged

-- import Data.Text (pack, unpack)

import Data.Aeson
import Network.Globus


-- import System.Environment

-- Modern Research Data Portal
-- https://docs.globus.org/guides/recipes/modern-research-data-portal/
-- https://github.com/globus/globus-sample-data-portal

main :: IO ()
main = do
  -- cid <- Tagged . pack <$> getEnv "GLOBUS_CLIENT_ID"
  -- sec <- Tagged . pack <$> getEnv "GLOBUS_CLIENT_SECRET"
  -- print cid
  -- print sec
  --
  -- let red = Uri Https "thunderbolts.dev.dkistdc.nso.edu" ["redirect"] (Query [])
  -- -- print red
  -- --
  -- -- let cfg = config (Token (pack cid)) (Token (pack sec)) red
  -- --
  -- let authUrl = authorizationUrl cid red [TransferAll] (State "state")
  -- putStrLn $ unpack $ renderUri authUrl
  --
  -- -- putStrLn "TOKEN REQUEST"
  -- -- putStrLn "-------------"
  -- -- res <- fetchAccessToken cid sec red (Token "M0EbLfeI64drpU7eWtIhiQx7YkzlWY")
  -- -- print res
  --
  -- putStrLn "SCOPES"
  -- putStrLn "-------------"
  -- let tok = Tagged "Agggw8GOMQMqr19jO2lj4e3px2Jvm5Bn7vG26pdlDx8ppj6jEKTJCpNDaYpxoE2EN3GbxXzw8M87EQS0n00bBt4DWpv"
  -- -- let task = Tagged "bd9f297a-d679-11ee-8702-a14c48059678"
  -- t <- fetchTasks tok (TaskFilters [Succeeded])
  -- print $ map (.label) t.data_

  let resp = "{\"access_token\":\"AgqX7wvDDNlV5284mEbjJw2ekYrJ76wyy7QoeaNqKg7wp5wnadfqCaPl8Bx7n77VdEWEDgx5e3naVwH5XYqqXSBYYEj\",\"scope\":\"email\",\"expires_in\":172800,\"token_type\":\"Bearer\",\"resource_server\":\"auth.globus.org\",\"state\":\"_\",\"other_tokens\":[{\"access_token\":\"AgjQP8WGzb7kE4v52kXln7JBbw85x4dzgO0eKjDm8gj8wNKByGh9C0wdOo0l3y3N2b2VzbY671E1lMuO1Kxx1S1razo\",\"scope\":\"urn:globus:auth:scope:transfer.api.globus.org:all\",\"expires_in\":172800,\"token_type\":\"Bearer\",\"resource_server\":\"transfer.api.globus.org\",\"state\":\"_\"}]}"
  let etr = eitherDecode resp :: Either String TokenResponse
  print etr

  case etr of
    Left e -> fail $ "OH NO" <> e
    Right (TokenResponse tis) -> do
      print $ scopeToken TransferAll tis
      print $ scopeToken (Identity Email) tis

-- Depends
-- Depends

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
