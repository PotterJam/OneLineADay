module Config where

import Data.Text (Text)
import GHC.Word (Word16)
import qualified Dhall

data Config = Config
    { cfgDbPort      :: Word16
    , cfgDbName      :: String
    , cfgDbUser      :: String
    , cfgDbPass      :: String
    }
    deriving Show

instance Dhall.FromDhall Config where
  autoWith _ = Dhall.record $
    Config
      <$> Dhall.field "dbPort" Dhall.word16
      <*> Dhall.field "dbName" Dhall.string
      <*> Dhall.field "dbUser" Dhall.string
      <*> Dhall.field "dbPass" Dhall.string

loadConfig :: Text -> IO Config
loadConfig file =
  Dhall.input Dhall.auto file