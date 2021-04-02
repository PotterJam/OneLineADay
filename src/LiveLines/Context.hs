{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LiveLines.Context where

import Control.Monad.Reader
import Env

newtype AppM a = App { unAppM :: ReaderT Env IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)