{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DataKinds, TypeOperators #-}

module Lines.Api where

import qualified Database.PostgreSQL.Simple.FromRow as PG
import qualified Data.Aeson.Types as JSON
import Servant.Auth.Server as SAS
import Control.Monad.Reader
import Control.Monad.Except
import Servant
import GHC.Generics
import Env
import Auth.Api -- TODO: remove this dep

--type LinesApi = "lines" :> ReqBody '[JSON] String :> Post '[JSON] Line
type LinesApi = "lines" :> Capture "text" String :> Get '[JSON] Line

data Line = Line
    { id :: Int
    , body :: String
    }
    deriving (Eq, Show, Generic, PG.FromRow, JSON.FromJSON, JSON.ToJSON)

linesHandler
    :: (MonadError ServerError m, MonadIO m, MonadReader Env m)
    => AuthResult AuthenticatedUser
    -> String
    -> m Line
linesHandler (SAS.Authenticated authedUser) text = pure $ Line (authedUserId authedUser) text
linesHandler _ _ = throwError err401