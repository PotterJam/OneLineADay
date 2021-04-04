{-# LANGUAGE QuasiQuotes, DeriveAnyClass, DeriveGeneric #-}

module User.Store where

import qualified Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.SqlQQ
import Data.Functor ((<&>))
import GHC.Generics
import Control.Monad.Reader
import Data.Maybe
import Data.Text
import User.Model
import DB
import Env

data StoredUser = StoredUser {
  id :: Int
  , username :: Text
  , email :: Text
  , password :: Text
} deriving (Generic, PG.FromRow)

getUserById
  :: (MonadReader Env m, MonadIO m)
  => Int
  -> m (Maybe User)
getUserById uid = do
  withDbConnection $ \conn -> do
    storedUser <- PG.query conn sqlQuery (PG.Only uid) <&> listToMaybe
    return $ getUserFromStored <$> storedUser
    where
      sqlQuery =
        [sql|
          SELECT id, username, email, password
          FROM users
          WHERE id = ?
        |]

getUserByUsername
  :: (MonadReader Env m, MonadIO m)
  => Text
  -> m (Maybe User)
getUserByUsername username = do
  withDbConnection $ \conn -> do
    storedUser <- PG.query conn sqlQuery (PG.Only username) <&> listToMaybe
    return $ getUserFromStored <$> storedUser
    where
      sqlQuery =
        [sql|
          SELECT id, username, email, password
          FROM users
          WHERE username = ?
        |]

createUser
  :: (MonadReader Env m, MonadIO m)
  => User
  -> m (Maybe User)
createUser (User _ username email password) = do
  rowsChanged <- withDbConnection $ \conn -> PG.execute conn sqlQuery (username, email, password)
  case rowsChanged of
    0 -> do
      Env.log $ "Tried to create account that aleady exists"
      return Nothing
    _ -> getUserByUsername username
  where
    sqlQuery =
      [sql|
        INSERT INTO users(username, email, password)
        VALUES(?, ?, ?)
        ON CONFLICT (username, email) DO NOTHING
      |]

getUserFromStored :: StoredUser -> User
getUserFromStored (StoredUser uid username email password) = User uid username email password