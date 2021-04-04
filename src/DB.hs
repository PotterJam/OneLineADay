module DB (withDbConnection) where

import qualified Database.PostgreSQL.Simple as PG
import Control.Monad.Reader
import Data.Functor ((<&>))
import Env
import Config

withDbConnection
  :: (MonadReader Env m, MonadIO m)
  => (PG.Connection -> IO a) -> m a
withDbConnection f = do
  connection <- getDbConnection
  res <- liftIO $ f connection
  liftIO $ PG.close connection
  return res

getDbConnection
  :: (MonadReader env m, HasConfig env, MonadIO m)
  => m PG.Connection
getDbConnection = do
  Config{..} <- ask <&> getConfig
  liftIO $ PG.connect PG.defaultConnectInfo
    { PG.connectDatabase = cfgDbName
    , PG.connectUser = cfgDbUser
    , PG.connectPort = cfgDbPort
    , PG.connectPassword = cfgDbPass
    }
  