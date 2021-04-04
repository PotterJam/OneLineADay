{-# LANGUAGE TypeOperators, DataKinds #-}

module Server.Api where

import Servant
import Servant.Auth as SA
import Servant.Auth.Server as SAS
import Server.Context
import Auth.Api
import Lines.Api

type LiveLinesApi = (SAS.Auth '[SA.JWT] AuthenticatedUser :> LinesApi) :<|> LoginApi :<|> SignupApi

liveLinesServer :: SAS.CookieSettings -> SAS.JWTSettings -> ServerT LiveLinesApi AppM
liveLinesServer cs jwts = linesHandler :<|> (loginHandler cs jwts) :<|> (signupHandler cs jwts)