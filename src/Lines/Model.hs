{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Lines.Model where

import GHC.Generics (Generic)
import Data.DateTime

data Line = Line
  { id :: Int
  , body :: String
  , created :: DateTime
  }
  deriving (Eq, Show, Generic)