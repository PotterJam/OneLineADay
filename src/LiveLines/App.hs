{-# LANGUAGE DataKinds #-}

module LiveLines.App(run, mkApp) where

import qualified Network.Wai.Handler.Warp as Warp
import Control.Monad.Reader
import qualified Say
import Servant

import LiveLines.Api
import LiveLines.Context
import Env
import Config

run :: Int -> IO ()
run port = do
  putStrLn $ "Running live lines on port: " <> show port
  config <- loadConfig "./dev.dhall"
  let env = Env { envLog = Say.sayString
                , envConfig = config
                }
  app <- mkApp env
  Warp.run port app

liveLinesProxy :: Proxy LiveLinesApi
liveLinesProxy = Proxy

mkApp :: Env -> IO Application
mkApp env = do
  return $ serveWithContext liveLinesProxy EmptyContext $
    hoistServerWithContext liveLinesProxy (Proxy :: Proxy '[])
        (appMToHandler env) liveLinesServer

appMToHandler :: Env -> AppM a -> Handler a
appMToHandler env r = do
  result <- liftIO $ runReaderT (unAppM r) env
  return result