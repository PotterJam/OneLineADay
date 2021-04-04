{-# LANGUAGE DataKinds, DeriveGeneric, TypeFamilies, TypeOperators #-}

module Server.App(run) where

import qualified Network.Wai.Handler.Warp as Warp
import Control.Monad.Reader
import Control.Monad.Except
import qualified Say
import Servant
import Servant.Auth.Server as SAS

import Server.Api
import Server.Context
import Env
import Config

run :: Int -> IO ()
run port = do
  putStrLn $ "Running live lines on port: " <> show port
  config <- loadConfig "./dev.dhall"
  myKey <- SAS.generateKey
  let env = Env { envLog = Say.sayString
                , envConfig = config
                }
      warpSettings = Warp.defaultSettings
      portSettings = Warp.setPort port warpSettings
      settings = Warp.setTimeout 55 portSettings
      jwtCfg = defaultJWTSettings myKey
      cookieCfg = defaultCookieSettings{cookieIsSecure=SAS.NotSecure} -- TODO: don't care about this yet
      cfg = jwtCfg :. cookieCfg :. EmptyContext
  Warp.runSettings settings $ mkApp cfg cookieCfg jwtCfg env

liveLinesProxy :: Proxy LiveLinesApi
liveLinesProxy = Proxy

mkApp
  :: Context '[SAS.JWTSettings, SAS.CookieSettings]
  -> CookieSettings
  -> JWTSettings
  -> Env
  -> Application
mkApp cfg cs jwts env =
  serveWithContext liveLinesProxy cfg $
    hoistServerWithContext liveLinesProxy (Proxy :: Proxy '[SAS.JWTSettings, SAS.CookieSettings])
        (appMToHandler env) (liveLinesServer cs jwts)

appMToHandler :: Env -> AppM a -> Handler a
appMToHandler env r = do
  resultOrError <- liftIO $ flip runReaderT env . runExceptT . unAppM $ r
  case resultOrError of
    Left err   -> throwError err
    Right result -> return result