{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Lambda.Notebook.Kernel.Action.Run where

import Conduit (yield)
import Control.Monad (when)
import Control.Monad.Except (MonadError (throwError))
import Data.Aeson (FromJSON)
import Data.List (intercalate)
import qualified Data.Map as M
import qualified Data.Time as T
import qualified Data.UUID as U
import GHC.Generics (Generic)
import Lambda.Lib.Language (StdOut, parse)
import Lambda.Notebook.Dependencies (HasM, getM)
import Lambda.Notebook.Error (getOr)
import Lambda.Notebook.Kernel.Header (LookupKernelHandleM, RunStatementM)
import Lambda.Notebook.Kernel.Model
  ( Kernel (..),
    KernelStatus (Running),
    kernelRunning,
    kernelRuntimeError,
    kernelSucces,
  )
import Lambda.Notebook.Storage (ModifyHandleM, ReadHandleM)
import Servant (SourceIO, ToSourceIO (..))

-- run endpoint ---------------------------------------------------------------

newtype ExecuteReqBody = ExecuteReqBody
  { code :: String
  }
  deriving (Generic, FromJSON)

data RunError = UUIDNotFound | SyntaxError String | KernelIsRunning Kernel
  deriving (Show)

runAction ::
  (HasM T.UTCTime m, MonadError RunError m) =>
  -- get kernel information
  LookupKernelHandleM h m ->
  ReadHandleM h Kernel m ->
  -- ConduitT / IO actions
  RunStatementM IO ->
  ModifyHandleM h Kernel (StdOut IO) ->
  -- uuid + statement
  U.UUID ->
  ExecuteReqBody ->
  m (SourceIO String)
runAction
  lookupKernelHandle
  readKernelHandle
  runStatement
  modifyKernelHandle
  uuid
  req = do
    kernelHandle <- lookupKernelHandle uuid >>= getKernelHandle
    currentScope <- scope <$> getKernel kernelHandle
    statement <- getStatement (parse (code req))
    now <- getM @T.UTCTime

    let updateKernel = modifyKernelHandle kernelHandle
    (pure . toSourceIO) do
      updateKernel (kernelRunning now)
      result <- runStatement currentScope statement
      yieldResult result
      updateKernel . kernelMapper $ result
    where
      getStatement = getOr (throwError . SyntaxError . show)
      getKernelHandle = getOr (throwError UUIDNotFound)

      getKernel kernelHandle = do
        kernel <- readKernelHandle kernelHandle
        when (status kernel == Just Running) do
          throwError $ KernelIsRunning kernel
        pure kernel

      kernelMapper (Right newScope) = kernelSucces newScope
      kernelMapper (Left _) = kernelRuntimeError

      yieldResult (Right newScope) = yieldSuccess newScope
      yieldResult (Left runtimeError) = yieldError runtimeError

yieldSuccess :: Monad m => M.Map String a -> StdOut m ()
yieldSuccess runtimeScope = do
  yield $ "(exit ok; scope: " ++ intercalate ", " (M.keys runtimeScope) ++ ")"

yieldError :: Monad m => String -> StdOut m ()
yieldError runtimeError = do
  yield "================================================"
  yield " Runtime Error"
  yield "================================================"
  yield ""
  yield runtimeError
