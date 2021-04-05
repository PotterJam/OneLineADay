{-# LANGUAGE DeriveGeneric #-}

module User.Model where

import qualified Data.ByteString.Char8 as B
import Data.Text
import GHC.Generics

data User = User {
    userid :: Int
    , username :: Text
    , email :: Text
    , password :: B.ByteString
} deriving (Eq, Show, Generic)
