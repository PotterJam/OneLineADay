{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module User.Model where

import Data.Aeson.Types
import Data.Text
import GHC.Generics

data User = User {
    userid :: Int
    , username :: Text
    , email :: Text
    , password :: Text
} deriving (Eq, Show, Generic, FromJSON, ToJSON)
