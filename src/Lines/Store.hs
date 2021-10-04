{-# LANGUAGE QuasiQuotes, DeriveGeneric, DeriveAnyClass, ScopedTypeVariables #-}

module Lines.Store where

import qualified Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.SqlQQ
import GHC.Generics (Generic)
import Data.DateTime
import Env
import Control.Monad.Reader
import Lines.Model
import DB

data LineToCreate = LineToCreate
  { message :: String
  , userId :: Int
  }
  deriving (Eq, Show, Generic)

data StoredLine = StoredLine
  { id :: Int
  , storedBody :: String
  , created :: DateTime
  }
  deriving (Eq, Show, Generic, PG.FromRow)

createLine
  :: (MonadReader Env m, MonadIO m)
  => String
  -> Int
  -> m (Maybe Line)
createLine message userId = do
  returned :: [(Int, DateTime)] <- withDbConnection $ \conn -> PG.query conn sqlQuery (userId, message)
  case returned of
    [(lineId, createdAt)] -> do
      return $ Just $ getLineFromStored (StoredLine lineId message createdAt)
    _ -> do
      Env.log $ "Couldn't create line for user " ++ show userId
      return Nothing
  where
    sqlQuery =
      [sql|
        INSERT INTO lines(user_id, body)
        VALUES (?, ?)
        RETURNING id, created_at
      |]

getLineFromStored :: StoredLine -> Line
getLineFromStored (StoredLine uid body created) = Line uid body created