module MyCloud
  ( module MyCloud.Types
  , module MyCloud.Files
  ) where

import Database.HDBC.PostgreSQL
import Network

import MyCloud.Types
import MyCloud.Files

postgresConf :: Config
postgresConf = Config
  { runOn      = PortNumber 7767
  , encryption = ForceAES "aes.key"
  , postgreSQL = connectPostgreSQL "user=mycloud db=mycloud password=test"
  }

runMyCloud :: Config -> IO ()
runMyCloud _cfg = do
  return ()
