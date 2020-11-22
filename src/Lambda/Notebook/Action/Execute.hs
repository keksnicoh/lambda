{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Lambda.Notebook.Action.Execute where

import Conduit (MonadIO (..), yield)
import Control.Monad (when)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State (MonadState, gets)
import Data.IORef (IORef, readIORef)
import Data.List (intercalate)
import qualified Data.Map as M
import qualified Data.Time as T
import qualified Data.UUID as U
import Lambda.Lib.Language (Statement, StdOut, parse)
import Lambda.Notebook.Data.Error (getOr)
import Lambda.Notebook.Data.Kernel
  ( Kernel,
    KernelStatus (..),
    Register,
    scope,
    status,
    updateKernel,
    updateKernelRunning,
  )
import Lambda.Notebook.Dependencies (HasM)
import Lambda.Notebook.Runtime (execute)
import Servant (SourceIO, ToSourceIO (..))

-- demo endpoint --------------------------------------------------------------

data RunError = UUIDNotFound | SyntaxError String | KernelIsRunning Kernel
  deriving (Show)

runService ::
  ( MonadState Register m,
    HasM T.UTCTime m,
    MonadIO m,
    MonadError RunError m
  ) =>
  U.UUID ->
  String ->
  m (SourceIO String)
runService uuid code = do
  kernelIORef <- gets (M.lookup uuid) >>= getKernel

  -- kernel must not be running
  kernel <- liftIO $ readIORef kernelIORef
  ensureNotRunning kernel

  statement <- getStatement (parse code)

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

-- demo endpoint --------------------------------------------------------------

demoEndpoint ::
  (MonadState Register m, HasM T.UTCTime m, MonadIO m) =>
  m (SourceIO String)
demoEndpoint = do
  let code =
        "a=bc;hnfPrintSteps[20, (\\xy.x)(\\xy.y)(\\xy.x)(\\xy.x)(\\xy.y)"
          ++ "(\\xy.x)(\\xy.x)(\\xy.y)(\\xy.x)(\\xy.x)(\\xy.y)(\\xy.x)];"
      Right statement = parse code
  pure $ toSourceIO (execute @IO M.empty statement >>= yield . show)
