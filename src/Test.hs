module Test where

import Data.Text (pack, unpack)
import Network.Globus
import Network.HTTP.Types (urlEncode)
import System.Environment


-- We need to be able to go from redirect -> identify the user, and make sure the state matches
-- Erm....
main :: IO ()
main = do
  cid <- Token . pack <$> getEnv "GLOBUS_CLIENT_ID"
  sec <- Token . pack <$> getEnv "GLOBUS_CLIENT_SECRET"
  print cid
  print sec

  let red = Uri "https://www.nso.edu" (Query [])
  -- print red
  --
  -- let cfg = config (Token (pack cid)) (Token (pack sec)) red
  --
  let authUrl = authorizationUrl cid red (State "state")
  putStrLn $ unpack $ renderUri authUrl

-- res <- fetchAccessToken cfg (Token "NaiIGkfqUSjTEuBxqToBmRBzZYTUCf")
-- print res
