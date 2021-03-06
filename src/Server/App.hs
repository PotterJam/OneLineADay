{-# LANGUAGE DataKinds, DeriveGeneric, TypeFamilies, TypeOperators #-}

module Server.App(run) where

import qualified Network.Wai.Handler.Warp as Warp
import Control.Monad.Reader
import Control.Monad.Except
import Servant
import Servant.Auth.Server as SAS
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Logger (withStdoutLogger)

import Server.Api ( LiveLinesApi, liveLinesServer )
import Server.Context
import Env
import Config

run :: Int -> IO ()
run port = do
  withStdoutLogger $ \aplogger -> do
    putStrLn $ "Running live lines on port: " <> show port
    config <- loadConfig "./dev.dhall"
    myKey <- SAS.generateKey
    let env = Env { envLog = putStrLn
                  , envConfig = config
                  }
        portSettings = Warp.setPort port Warp.defaultSettings
        timeoutSettings = Warp.setTimeout 55 portSettings
        settings = Warp.setLogger aplogger timeoutSettings
        jwtCfg = defaultJWTSettings myKey
        cookieCfg = defaultCookieSettings{ cookieSameSite = SameSiteStrict, cookieIsSecure=SAS.NotSecure } -- TODO: don't care about this yet
        cfg = jwtCfg :. cookieCfg :. EmptyContext
    Warp.runSettings settings $ simpleCors $ mkApp cfg cookieCfg jwtCfg env

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