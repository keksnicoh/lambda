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

-- XXX very prototypal stuff in here ...
module Lambda.Notebook.Runtime where

import Control.Monad.Except
  ( ExceptT,
    MonadError (throwError),
    MonadIO (..),
    runExceptT,
  )
import Control.Monad.Reader (MonadReader (ask), ReaderT (..), asks)
import Control.Monad.State (MonadState (get, put))
import Control.Monad.Writer
  ( MonadWriter (listen, pass, tell),
  )
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Lambda.Lib.Language (Scope, Statement, eval, handler)
import Lambda.Notebook.Data.Kernel (Kernel (scope))

data Runtime = Runtime
  { stdout :: IORef [String],
    runtimeScope :: IORef Scope
  }

newtype RuntimeT a = RuntimeT
  { runRuntimeT :: ReaderT Runtime (ExceptT String IO) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Runtime,
      MonadIO,
      MonadError String
    )

-- XXX highly experimantal
instance MonadWriter [String] RuntimeT where
  tell msg = do
    env <- ask
    liftIO $ modifyIORef' (stdout env) (<> msg)
  listen action = do
    env <- ask
    tempStdOut <- liftIO $ newIORef []
    let runtime' = env {stdout = tempStdOut}
        foo = runExceptT (runReaderT (runRuntimeT action) runtime')
    result <- liftIO foo
    case result of
      Right res -> do
        written <- liftIO $ readIORef tempStdOut
        pure (res, written)
      Left err -> throwError err

  pass action = do
    env <- ask
    tempStdOut <- liftIO $ newIORef []
    let runtime' = env {stdout = tempStdOut}
        foo = runExceptT (runReaderT (runRuntimeT action) runtime')
    res <- liftIO foo
    case res of
      Right (result, f) -> do
        written <- f <$> liftIO (readIORef tempStdOut)
        liftIO $ modifyIORef' (stdout env) (<> written)
        pure result
      Left err -> throwError err

runner :: RuntimeT a -> Runtime -> IO (Either String a)
runner evalM runtime = runExceptT (runReaderT (runRuntimeT evalM) runtime)

instance MonadState Scope RuntimeT where
  get = asks runtimeScope >>= liftIO . readIORef
  put s = do
    env <- ask
    liftIO $ modifyIORef' (runtimeScope env) (const s)

execute :: Kernel -> Statement -> IO (Runtime, Maybe String)
execute kernel statement = do
  do
    runtime <- Runtime <$> newIORef [] <*> newIORef (scope kernel)
    (,) runtime . projResult <$> runner (eval handler statement) runtime
  where
    projResult (Left err) = Just err
    projResult _ = Nothing
