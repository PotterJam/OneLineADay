{-# LANGUAGE TypeFamilies, DataKinds, DeriveGeneric, TypeOperators, DeriveAnyClass #-}

module Auth.Api where

import Data.Aeson
import GHC.Generics
import Data.Text
import Control.Monad.IO.Class (liftIO)
import Servant as S
import Servant.Auth.Server as SAS
import Server.Context
import Env

data AuthenticatedUser = AuthenticatedUser {
    userId :: Int
  , userName :: Text
} deriving (Show, Generic, ToJSON, FromJSON, SAS.ToJWT, SAS.FromJWT)

data LoginForm = LoginForm {
  username :: Text
  , password :: Text
} deriving (Eq, Show, Generic, ToJSON, FromJSON)

type LoginApi = "login"
      :> ReqBody '[JSON] LoginForm
      :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)

loginHandler
  :: SAS.CookieSettings
  -> SAS.JWTSettings
  -> LoginForm
  -> AppM (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
loginHandler cookieSettings jwtSettings form = do
  case validateLogin form of
    Nothing -> do throwError err401
    Just user -> do
      mApplyCookies <- liftIO $ SAS.acceptLogin cookieSettings jwtSettings user
      case mApplyCookies of
        Nothing           -> do throwError err401
        Just applyCookies -> do
          Env.log("User successfully authenticated!")
          pure $ applyCookies NoContent

validateLogin :: LoginForm -> Maybe AuthenticatedUser
validateLogin (LoginForm username password) =
    if (username == "test") && (password == "test")
    then Just $ AuthenticatedUser 1 username
    else Nothing