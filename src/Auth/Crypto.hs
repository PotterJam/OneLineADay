module Auth.Crypto where

import qualified Data.ByteString.Char8 as B
import qualified Crypto.KDF.BCrypt as BCrypt
import Data.Text.Encoding
import Data.Text

hashCostParam :: Int
hashCostParam = 12

hashPassword :: Text -> IO B.ByteString
hashPassword password = BCrypt.hashPassword hashCostParam (encodeUtf8 password)

validatePassword :: Text -> B.ByteString -> Bool
validatePassword password storedPass = BCrypt.validatePassword (encodeUtf8 password) storedPass