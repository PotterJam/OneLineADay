{-# LANGUAGE QuasiQuotes, DeriveAnyClass, DeriveGeneric #-}

module User.Store where

import qualified Database.PostgreSQL.Simple as PG
import qualified Data.ByteString.Char8 as B
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
  , password :: B.ByteString
} deriving (Generic, PG.FromRow)

data UserToCreate = UserToCreate {
  newUserame :: Text
  , newEmail :: Text
  , newPassword :: B.ByteString
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
  => UserToCreate
  -> m (Maybe User)
createUser (UserToCreate username email hashedPassword) = do
  rowsChanged <- withDbConnection $ \conn -> PG.execute conn sqlQuery (username, email, hashedPassword)
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
        ON CONFLICT DO NOTHING
      |]

getUserFromStored :: StoredUser -> User
getUserFromStored (StoredUser uid username email password) = User uid username email password