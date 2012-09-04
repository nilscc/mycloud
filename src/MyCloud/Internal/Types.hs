{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MyCloud.Internal.Types where

import Control.Monad.Reader
import Control.Concurrent.MState
import Data.Map (Map)
import Data.Time
import Network
import System.IO

data FileWithInfo = FileWithInfo
  { fileWithInfoPath :: FilePath
  }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Cloud configuration

data Config = Config
  { runOn                 :: PortID
  , encryption            :: Encryption
  , postgresqlConnection  :: String
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
  --, commandContext  :: Maybe Command
  }

--------------------------------------------------------------------------------
-- The cloud monad

newtype MyCloud a = MyCloud { runMC :: MState CloudState (ReaderT Config IO) a }
  deriving (Monad, MonadIO, Functor)


--------------------------------------------------------------------------------
-- Events

data Event = Event
  { eventTime     :: UTCTime
  , eventPath     :: FilePath
  , eventType     :: EventType
  }
  deriving (Eq, Show)

data EventType
  = CreateFile
  | ChangeFile
  | DeleteFile
  | RenameFile
  | CreateDir
  | DeleteDir
  | RenameDir
  deriving (Eq, Show, Enum)
