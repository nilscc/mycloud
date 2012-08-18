{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MyCloud.Internal.Types where

import Control.Monad.Reader
import Control.Concurrent.MState
import Network
import System.IO

import Data.Map (Map)

import Database.HDBC.PostgreSQL

--------------------------------------------------------------------------------
-- Cloud configuration

data Config = Config
  { runOn       :: PortID
  , encryption  :: Encryption
  , postgreSQL  :: IO Connection
  }

newtype BlockSize = BlockSize { unBS :: Int }

data Encryption
  = NoEncryption
  | OptionalAES { keyFile :: FilePath }
  | ForceAES    { keyFile :: FilePath }

--------------------------------------------------------------------------------
-- Cloud state

data CloudState = CloudState
  { connectedClients  :: Map SessionID ClientInfo
  }

newtype SessionID = SessionID { unSID :: Int }
  deriving (Eq, Ord)

data ClientInfo = ClientInfo
  { clientHandle    :: Handle
  , usesEncryption  :: Maybe FilePath
  }

--------------------------------------------------------------------------------
-- The cloud monad

newtype MyCloud a = MyCloud { runMC :: MState CloudState (ReaderT Config IO) a }
  deriving (Monad, MonadIO)
