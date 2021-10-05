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
  , body :: String
  , created_at :: DateTime
  }
  deriving (Eq, Show, Generic, PG.FromRow)

getLines
  :: (MonadReader Env m, MonadIO m)
  => Int
  -> m [Line]
getLines userId = do
  withDbConnection $ \conn -> do
    storedLines <- PG.query conn sqlQuery (PG.Only userId)
    return $ getLineFromStored <$> storedLines
  where
    sqlQuery =
      [sql|
        SELECT id, body, created_at
        FROM lines
        WHERE user_id = ?
        ORDER BY created_at desc
      |]

createLine
  :: (MonadReader Env m, MonadIO m)
  => String
  -> Int
  -> m (Maybe Line)
createLine message userId = do
  withDbConnection $ \conn -> do
    returned :: [(Int, DateTime)] <- PG.query conn sqlQuery (userId, message)
    return $ case returned of
      [(lineId, createdAt)] -> Just $ getLineFromStored (StoredLine lineId message createdAt)
      _ -> Nothing
  where
    sqlQuery =
      [sql|
        INSERT INTO lines(user_id, body)
        VALUES (?, ?)
        RETURNING id, created_at
      |]

getLineFromStored :: StoredLine -> Line
getLineFromStored (StoredLine uid body created) = Line uid body created