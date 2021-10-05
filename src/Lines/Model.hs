{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Lines.Model where

import GHC.Generics (Generic)
import Data.DateTime

data Line = Line
  { lineId :: Int
  , lineBody :: String
  , lineCreated :: DateTime
  }
  deriving (Eq, Show, Generic)