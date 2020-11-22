{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Lambda.Notebook.Kernel.Runtime (execute, RuntimeT (..)) where

import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.Reader (MonadReader (ask), ReaderT (..))
import Control.Monad.State (MonadIO (..), MonadState (get, put))
import Data.Conduit (ConduitT, transPipe)
import Data.Conduit.Lift (runExceptC, runReaderC)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Lambda.Lib.Language (Scope, Statement, eval, handler)

-- statement evaluation runtime -----------------------------------------------

newtype RuntimeT m a = RuntimeT
  { runRuntimeT :: ReaderT (IORef Scope) (ExceptT String m) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader (IORef Scope),
      MonadIO,
      MonadError String
    )

runRuntimeC ::
  (MonadIO m) =>
  IORef Scope ->
  ConduitT () String (RuntimeT m) a ->
  ConduitT () String m (Either String a)
runRuntimeC runtime = runExceptC . runReaderC runtime . transPipe runRuntimeT

-- haskell API ----------------------------------------------------------------

instance MonadIO m => MonadState Scope (RuntimeT m) where
  get = ask >>= liftIO . readIORef
  put s = do
    scope <- ask
    liftIO $ modifyIORef' scope (const s)

-- action ---------------------------------------------------------------------

execute ::
  MonadIO m =>
  Scope ->
  Statement ->
  ConduitT () String m (Either String Scope)
execute scope statement = do
  runtime <- liftIO $ newIORef scope
  result <- runRuntimeC runtime (eval handler statement)
  case result of
    Right () -> Right <$> liftIO (readIORef runtime)
    Left e -> pure (Left e)
