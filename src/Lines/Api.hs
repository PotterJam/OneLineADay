{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Lines.Api where

import qualified Database.PostgreSQL.Simple.FromRow as PG
import qualified Data.Aeson.Types as JSON
import Control.Monad.Reader
import Servant
import GHC.Generics
import Env

--type LinesApi = "lines" :> ReqBody '[JSON] String :> Post '[JSON] Line
type LinesApi = "lines" :> Capture "text" String :> Get '[JSON] Line

data Line = Line
    { id :: Int
    , body :: String
    }
    deriving (Eq, Show, Generic, PG.FromRow, JSON.FromJSON, JSON.ToJSON)

linesHandler
    :: (MonadIO m, MonadReader Env m)
    => String
    -> m Line
linesHandler text = do
  return $ Line 1 text