{-# LANGUAGE FlexibleInstances #-}

module MyCloud.DB where

import Control.Monad.IO.Peel
import Control.Monad.Reader

import qualified Database.HDBC as H
import           Database.HDBC hiding (catchSql, handleSql, throwSqlError)
import           Database.HDBC.PostgreSQL

import MyCloud.Internal.Types

withConnection :: (Connection -> IO a) -> MyCloud a
withConnection go = MyCloud $ do
  Config { postgresqlConnection = conStr } <- ask
  liftIO $ withPostgreSQL conStr go

--------------------------------------------------------------------------------
---- SQL helpers

updateSql :: String -> [SqlValue] -> MyCloud Integer
updateSql s v = withConnection $ \c -> do
  i <- run c s v
  commit c
  return i

updateSql_ :: String -> [SqlValue] -> MyCloud ()
updateSql_ s v = updateSql s v >> return ()

querySql :: String -> [SqlValue] -> MyCloud [[SqlValue]]
querySql s v = withConnection $ \c -> quickQuery' c s v


--------------------------------------------------------------------------------
-- Exceptions

catchSql :: MonadPeelIO m => m a -> (SqlError -> m a) -> m a
catchSql m h = do
  k <- peelIO
  join . liftIO $ H.catchSql (k m) (\e -> k (h e))

handleSql :: MonadPeelIO m => (SqlError -> m a) -> m a -> m a
handleSql = flip catchSql

throwSqlError :: MonadIO m => SqlError -> m a
throwSqlError e = do
  liftIO $ H.throwSqlError e

--------------------------------------------------------------------------------
-- SQL select

{-

-- | Minimal complete definition: `select` and `convertFromSql`. Example
-- implementation:
--
-- > instance Select Foo where
-- >   select         = withSelectStr "SELECT foo FROM foos"
-- >   convertFromSql = convertSqlToFoo
--
-- Example usage:
--
-- > getFooById :: MonadIO m => Id -> m Foo
-- > getFooById id = select "WHERE id = ?" [toSql id]
class Select res where

  convertFromSql :: [[SqlValue]] -> res
  select         :: String -> [SqlValue] -> MyCloud res
  fullSelect     :: String -> [SqlValue] -> MyCloud res
  withSelectStr  :: String        -- ^ "SELECT .. FROM .."
                 -> String        -- ^ "WHERE .." / "JOIN .." etc
                 -> [SqlValue]
                 -> MyCloud res

  -- default implementations
  fullSelect    s     v = querySql s v >>= return . convertFromSql
  withSelectStr s1 s2 v = fullSelect (s1++" "++s2) v

--------------------------------------------------------------------------------
-- SQL queries

instance Select [Event] where
  select         = withSelectStr "SELECT (time, path, event) FROM recent_events"
  convertFromSql = map conv1
   where
    conv1 [t,p,e]
      | Right t' <- safeFromSql t
      , Right p' <- safeFromSql p
      , Right e' <- safeFromSql e
      = Event t' p' (toEnum e')
    conv1 v
      = error $ "Cannot convert " ++ show v
-}

recentEvents :: SessionID -> MyCloud [Event]
recentEvents (SessionID sid) = toEvents `fmap`
  querySql "SELECT time, path, event \
           \  FROM recent_events_by_session_id \
           \ WHERE session_id = ?"
           [ toSql sid ]
 where
  toEvents = map conv1
  conv1 [t,p,e]
    | Right t' <- safeFromSql t
    , Right p' <- safeFromSql p
    , Right e' <- safeFromSql e
    = Event t' p' (toEnum e')
  conv1 v
    = error $ "Cannot convert " ++ show v
