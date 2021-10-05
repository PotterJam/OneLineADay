{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DataKinds, TypeOperators #-}

module Lines.Api where

import qualified Data.Aeson.Types as JSON
import Servant.Auth.Server as SAS
import Control.Monad.Reader
import Control.Monad.Except
import Servant
import Env
import Auth.Api
import Lines.Model
import Data.DateTime
import qualified Lines.Store as LineStore
import GHC.Generics

newtype LineRequest = LineRequest
  { message :: String
  }
  deriving (Generic, JSON.FromJSON)

data LineResponse = LineResponse
  { respId :: Int
  , respBody :: String
  , respCreated :: DateTime
  }
  deriving (Generic, JSON.ToJSON)

type LinesApi = "lines" :>
  (Get '[JSON] [LineResponse]
  :<|> (ReqBody '[JSON] LineRequest :> Post '[JSON] LineResponse))

linesHandler
  :: (MonadError ServerError m, MonadIO m, MonadReader Env m)
  => AuthResult AuthenticatedUser
  -> ServerT LinesApi m
linesHandler (SAS.Authenticated authedUser) =
  lineGetHandler userId
  :<|> lineCreateHandler userId
    where userId = authedUserId authedUser

linesHandler _ = throwError err401 :<|> \_ -> throwError err401


lineGetHandler
  :: (MonadError ServerError m, MonadIO m, MonadReader Env m)
  => Int
  -> m [LineResponse]
lineGetHandler userId = do
  usersLines <- LineStore.getLines userId
  return $ createLineResponse <$> usersLines

lineCreateHandler
  :: (MonadError ServerError m, MonadIO m, MonadReader Env m)
  => Int
  -> LineRequest
  -> m LineResponse
lineCreateHandler userId (LineRequest message) = do
  createdLine <- LineStore.createLine message userId
  case createdLine of
    Just li -> return $ createLineResponse li
    Nothing -> do
      Env.log $ "Could not create line for user " ++ show userId
      throwError err500


createLineResponse :: Line -> LineResponse
createLineResponse (Line lineId body created) = LineResponse lineId body created
