module Env where

import Prelude hiding (log)
import Control.Monad.Reader
import Config

data Env = Env
    { envLog        :: String -> IO ()
    , envConfig     :: Config
    }

class HasLog a where
  getLog :: a -> (String -> IO ())
instance HasLog (String -> IO ()) where
  getLog = id
instance HasLog Env where
  getLog = envLog

class HasConfig a where
  getConfig :: a -> Config
instance HasConfig Config where
  getConfig = id
instance HasConfig Env where
  getConfig = envConfig

log :: (MonadReader env m, HasLog env, MonadIO m)
    => String
    -> m ()
log msg = do
  env <- ask
  liftIO $ getLog env msg