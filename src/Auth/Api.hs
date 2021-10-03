{-# LANGUAGE TypeFamilies, DataKinds, DeriveGeneric, TypeOperators, DeriveAnyClass #-}

module Auth.Api where

import qualified Auth.Crypto as Crypto
import Data.Aeson
import GHC.Generics
import Data.Text
import Control.Monad.IO.Class (liftIO)
import Control.Monad (guard)
import Servant as S
import Servant.Auth.Server as SAS
import Server.Context
import User.Store as UserStore
import User.Model as U
import Env

data AuthenticatedUser = AuthenticatedUser {
  authedUserId :: Int
  , authedUsername :: Text
  , authedUserEmail :: Text
} deriving (Show, Generic, ToJSON, FromJSON, SAS.ToJWT, SAS.FromJWT)

data LoginForm = LoginForm {
  loginUsername :: Text
  , loginPassword :: Text
} deriving (Eq, Show, Generic, ToJSON, FromJSON)

data SignupForm = SignupForm {
  signupUsername :: Text
  , signupEmail :: Text
  , signupPassword :: Text
} deriving (Eq, Show, Generic, ToJSON, FromJSON)

type LoginApi = "login"
      :> ReqBody '[JSON] LoginForm
      :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)

type SignupApi = "signup"
      :> ReqBody '[JSON] SignupForm
      :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)

type IsLoggedInApi = "isLoggedIn"
      :> Get '[JSON] NoContent

isLoggedInHandler
  :: AuthResult AuthenticatedUser
  -> AppM NoContent
isLoggedInHandler (SAS.Authenticated _) = pure NoContent
isLoggedInHandler _ = throwError err401

loginHandler
  :: SAS.CookieSettings
  -> SAS.JWTSettings
  -> LoginForm
  -> AppM (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
loginHandler cookieSettings jwtSettings form = do
  validatedUser <- validateLogin form
  case validatedUser of
    Nothing -> throwError err401
    Just user -> do
      mApplyCookies <- liftIO $ SAS.acceptLogin cookieSettings jwtSettings user
      case mApplyCookies of
        Nothing           -> throwError err401
        Just applyCookies -> do
          Env.log "User successfully authenticated"
          pure $ applyCookies NoContent

validateLogin :: LoginForm -> AppM (Maybe AuthenticatedUser)
validateLogin (LoginForm formUsername formPassword) = do
  retrievedUser <- UserStore.getUserByUsername formUsername
  case retrievedUser of
    Nothing -> throwError err404
    Just user -> do
      let storedPass = U.password user
          hasPermission = Crypto.validatePassword formPassword storedPass
      return $ guard hasPermission >> Just (authedUserFromUser user)

signupHandler
  :: SAS.CookieSettings
  -> SAS.JWTSettings
  -> SignupForm
  -> AppM (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
signupHandler cookieSettings jwtSettings form = do
  newUser <- createNewUser form
  case newUser of
    Nothing -> throwError err409
    Just user -> do
      mApplyCookies <- liftIO $ SAS.acceptLogin cookieSettings jwtSettings user
      case mApplyCookies of
        Nothing           -> throwError err401
        Just applyCookies -> do
          Env.log "User successfully authenticated"
          pure $ applyCookies NoContent

createNewUser :: SignupForm -> AppM (Maybe AuthenticatedUser)
createNewUser (SignupForm username email password) = do
  hashedPassword <- liftIO $ Crypto.hashPassword password
  newUser <- UserStore.createUser (UserToCreate username email hashedPassword)
  return $ authedUserFromUser <$> newUser

authedUserFromUser :: User -> AuthenticatedUser
authedUserFromUser (User uid username email _) = AuthenticatedUser uid username email