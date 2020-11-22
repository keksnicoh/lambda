{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Lambda.Notebook.Kernel.Action.Run where

import Conduit (MonadIO (..), yield)
import Control.Monad (when)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State (MonadState, gets)
import Data.Aeson (FromJSON)
import Data.IORef (IORef, modifyIORef', readIORef)
import Data.List (intercalate)
import qualified Data.Map as M
import qualified Data.Time as T
import qualified Data.UUID as U
import GHC.Generics (Generic)
import Lambda.Lib.Language (Statement, StdOut, parse)
import Lambda.Notebook.Dependencies (HasM)
import Lambda.Notebook.Error (getOr)
import Lambda.Notebook.Kernel.Model
  ( Kernel (execution, invoked, scope, status),
    KernelStatus (Running, RuntimeError, Success),
    Register,
  )
import Lambda.Notebook.Kernel.Runtime (execute)
import Servant (SourceIO, ToSourceIO (..))

-- demo endpoint --------------------------------------------------------------

data ExecuteReqBody = ExecuteReqBody
  { code :: String,
    block :: Int
  }
  deriving (Generic, FromJSON)

data RunError = UUIDNotFound | SyntaxError String | KernelIsRunning Kernel
  deriving (Show)

runService ::
  ( MonadState Register m,
    HasM T.UTCTime m,
    MonadIO m,
    MonadError RunError m
  ) =>
  U.UUID ->
  ExecuteReqBody ->
  m (SourceIO String)
runService uuid req = do
  kernelIORef <- gets (M.lookup uuid) >>= getKernel

  -- kernel must not be running
  kernel <- liftIO $ readIORef kernelIORef
  ensureNotRunning kernel

  statement <- getStatement (parse (code req))

  pure . toSourceIO $ runKernel @IO kernelIORef kernel statement
  where
    getStatement = getOr (throwError . SyntaxError . show)
    getKernel = getOr (throwError UUIDNotFound)
    ensureNotRunning kernel =
      when (status kernel == Just Running) $
        throwError $ KernelIsRunning kernel

runKernel :: MonadIO m => IORef Kernel -> Kernel -> Statement -> StdOut m ()
runKernel kernelIORef kernel statement = do
  liftIO $ updateKernelRunning kernelIORef kernel
  execute (scope kernel) statement >>= \case
    Right newScope -> do
      yieldResult newScope
      updateKernel kernelIORef kernel {scope = newScope, status = Just Success}
    Left runtimeError -> do
      yieldError runtimeError
      updateKernel kernelIORef kernel {status = Just RuntimeError}

yieldResult :: Monad m => M.Map String a -> StdOut m ()
yieldResult runtimeScope = do
  yield $ "(exit ok; scope: " ++ intercalate ", " (M.keys runtimeScope) ++ ")"

yieldError :: Monad m => String -> StdOut m ()
yieldError runtimeError = do
  yield "================================================"
  yield " Runtime Error"
  yield "================================================"
  yield ""
  yield runtimeError

updateKernelRunning :: MonadIO m => IORef Kernel -> Kernel -> m ()
updateKernelRunning kernelIORef kernel = do
  now <- liftIO T.getCurrentTime
  updateKernel kernelIORef $
    kernel
      { invoked = Just now,
        execution = execution kernel + 1,
        status = Just Running
      }

updateKernel :: MonadIO m => IORef Kernel -> Kernel -> m ()
updateKernel kernelIORef kernel = do
  liftIO $ modifyIORef' kernelIORef (const kernel)
