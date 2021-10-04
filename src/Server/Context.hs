{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Server.Context where

import Control.Monad.Reader
import Control.Monad.Except
import Servant
import Env

newtype AppM a = App { unAppM :: ExceptT ServerError (ReaderT Env IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadError ServerError)