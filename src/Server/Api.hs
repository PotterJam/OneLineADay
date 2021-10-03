{-# LANGUAGE TypeOperators, DataKinds #-}

module Server.Api where

import Servant
import Servant.Auth as SA
import Servant.Auth.Server as SAS
import Server.Context
import Auth.Api
import Lines.Api

type LiveLinesApi = "api" :>
    ((SAS.Auth '[SA.Cookie, SA.JWT] AuthenticatedUser :> IsLoggedInApi) 
    :<|> (SAS.Auth '[SA.Cookie, SA.JWT] AuthenticatedUser :> LinesApi)
    :<|> (SAS.Auth '[SA.Cookie, SA.JWT] AuthenticatedUser :> LogoutApi)
    :<|> LoginApi
    :<|> SignupApi)

liveLinesServer
    :: SAS.CookieSettings
    -> SAS.JWTSettings
    -> ServerT LiveLinesApi AppM
liveLinesServer cs jwts = isLoggedInHandler
    :<|> linesHandler
    :<|> logoutHandler
    :<|> loginHandler cs jwts
    :<|> signupHandler cs jwts