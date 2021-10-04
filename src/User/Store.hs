{-# LANGUAGE QuasiQuotes, DeriveAnyClass, DeriveGeneric, ScopedTypeVariables #-}

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
import Text.Printf

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
  returned :: [PG.Only Int] <- withDbConnection $ \conn -> PG.query conn sqlQuery (username, email, hashedPassword)
  case returned of
    [PG.Only userId] -> return $ Just $ getUserFromStored $ StoredUser userId username email hashedPassword
    _ -> do
      Env.log $ printf "Tried to create account that aleady exists. Requested username %s email %s" (show username) (show email)
      return Nothing
  where
    sqlQuery =
      [sql|
        INSERT INTO users(username, email, password)
        VALUES(?, ?, ?)
        RETURNING id
      |]

getUserFromStored :: StoredUser -> User
getUserFromStored (StoredUser uid username email password) = User uid username email password