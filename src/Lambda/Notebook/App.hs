{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Lambda.Notebook.App where

import Control.Monad.Except
  ( MonadError (catchError),
    MonadIO (..),
  )
import Control.Monad.Reader (MonadReader (ask), ReaderT (..), asks)
import Control.Monad.State (MonadState (get, put))
import Data.IORef (IORef, modifyIORef', readIORef)
import qualified Data.Time as T
import qualified Data.UUID as U
import qualified Data.UUID.V4 as U
import Lambda.Notebook.Dependencies
  ( HasM (..),
  )
import Lambda.Notebook.Kernel.Model (Register)
import Servant (throwError)

-- application environment ----------------------------------------------------

newtype Env = Env
  { kernels :: IORef Register
  }

newtype AppT m a = AppT {runAppT :: ReaderT Env m a}
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadIO)

-- haskell API ----------------------------------------------------------------

instance MonadError e m => MonadError e (AppT m) where
  throwError e = AppT (throwError e)
  catchError a b = AppT $ catchError (runAppT a) (runAppT . b)

-- XXX is this safe when multiple threads are used
instance MonadIO m => MonadState Register (AppT m) where
  get = asks kernels >>= liftIO . readIORef
  put s = do
    env <- ask
    liftIO $ modifyIORef' (kernels env) (const s)

-- notebook framework API -----------------------------------------------------

instance MonadIO m => HasM T.UTCTime (AppT m) where
  getM = liftIO T.getCurrentTime

instance MonadIO m => HasM U.UUID (AppT m) where
  getM = liftIO U.nextRandom
