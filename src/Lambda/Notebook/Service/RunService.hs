{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Lambda.Notebook.Service.RunService where

import Control.Monad.Except
  ( MonadError (throwError),
    MonadIO (..),
    when,
  )
import Control.Monad.State
  ( MonadState,
    gets,
  )
import Data.IORef (readIORef)
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Maybe (isNothing)
import qualified Data.Time as T
import qualified Data.UUID as U
import Lambda.Lib.Language (parse)
import Lambda.Notebook.Data.Error
  ( HandleError (..),
    errorWithCode,
    errorWithMessage,
    getOr,
  )
import Lambda.Notebook.Data.Kernel
  ( Kernel (..),
    Register,
    updateKernelInvoked,
  )
import Lambda.Notebook.Data.Result (Result (Result))
import Lambda.Notebook.Dependencies (HasM)
import Lambda.Notebook.Runtime
  ( Runtime (..),
    execute,
  )
import Servant (err400, err404)

data RunError = UUIDNotFound | SyntaxError String
  deriving (Show)

instance HandleError RunError where
  errorToResponse = \case
    SyntaxError err -> errorWithMessage err400 "could not parse given statement." err
    UUIDNotFound -> errorWithCode err404 "kernel does not exist."

runService ::
  (MonadState Register m, HasM T.UTCTime m, MonadIO m, MonadError RunError m) =>
  U.UUID ->
  String ->
  m Result
runService uuid code = do
  kernel <- gets (M.lookup uuid) >>= getKernel
  statement <- getStatement (parse code)

  (runtime, runtimeError) <- liftIO $ execute kernel statement
  when (isNothing runtimeError) $
    updateKernel kernel runtime

  renderResult runtime runtimeError
  where
    updateKernel kernel runtime = do
      newScope <- liftIO $ readIORef (runtimeScope runtime)
      updateKernelInvoked uuid kernel {scope = newScope}

    renderResult runtime runtimeError = do
      output <- liftIO $ readIORef (stdout runtime)
      pure $ Result (intercalate "\n" output) runtimeError

    getStatement = getOr (throwError . SyntaxError . show)
    getKernel = getOr (throwError UUIDNotFound)