module MyCloud
  ( module MyCloud.Types
  --, module MyCloud.Files
  , runMyCloud
  , postgresConf
    -- * DB queries
  , recentEvents
  ) where

import Control.Concurrent.MState
import Control.Monad.Reader
--import Database.HDBC.PostgreSQL
import Network

import qualified Data.Map as M

import MyCloud.DB
import MyCloud.Internal.Types
import MyCloud.Types
--import MyCloud.Files

postgresConf :: Config
postgresConf = Config
  { runOn                = PortNumber 7767
  , encryption           = ForceAES "aes.key"
  , postgresqlConnection = "user=mycloud dbname=mycloud password=test"
  }

initialState :: CloudState
initialState = CloudState
  { connectedClients = M.empty
  }

runMyCloud :: Config -> MyCloud a -> IO a
runMyCloud cfg (MyCloud go) =
  runReaderT (evalMState True go initialState) cfg
