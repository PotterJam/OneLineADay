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
  { id :: Int
  , body :: String
  , created :: DateTime
  }
  deriving (Generic, JSON.ToJSON)

type LinesApi = "lines" :> ReqBody '[JSON] LineRequest :> Post '[JSON] LineResponse

linesHandler
  :: (MonadError ServerError m, MonadIO m, MonadReader Env m)
  => AuthResult AuthenticatedUser
  -> LineRequest
  -> m LineResponse
linesHandler (SAS.Authenticated authedUser) (LineRequest message) = do
  createdLine <- LineStore.createLine message (authedUserId authedUser)
  case createdLine of
    Just li -> return $ createLineResponse li
    Nothing -> throwError err500

linesHandler _ _ = throwError err401

createLineResponse :: Line -> LineResponse
createLineResponse (Line lineId body created) = LineResponse lineId body created

