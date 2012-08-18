module MyCloud.DB where

import Control.Monad.Reader

--import Database.HDBC
import Database.HDBC.PostgreSQL

import MyCloud.Internal.Types

withConnection :: (Connection -> MyCloud a) -> MyCloud a
withConnection go = MyCloud $ do
  Config { postgreSQL = getCon } <- ask
  con <- liftIO getCon
  runMC $ go con
