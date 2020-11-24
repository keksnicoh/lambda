{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Lambda.Notebook.Kernel.API where

import Control.Monad.Except (MonadError)
import Data.IORef (IORef)
import qualified Data.UUID as U
import Lambda.Notebook.App (AppT)
import Lambda.Notebook.Error
  ( err423,
    errorWithCode,
    errorWithMessage,
    withErrorHandling0,
    withErrorHandling1,
    withErrorHandling2,
  )
import Lambda.Notebook.Kernel.Action.Create
  ( CreateKernelError (..),
    createKernelAction,
  )
import Lambda.Notebook.Kernel.Action.Run
  ( ExecuteReqBody,
    RunError (..),
    runAction,
  )
import Lambda.Notebook.Kernel.Action.Status
  ( KernelStatusError (..),
    kernelByUUIDAction,
  )
import Lambda.Notebook.Kernel.Model (Kernel, Register, UUIDContainer)
import Lambda.Notebook.Kernel.Runtime (runStatement)
import Lambda.Notebook.Storage
  ( lookupIORefMap,
    modifyHandleIORef,
    readHandleIORef,
  )
import Servant
  ( Capture,
    Get,
    Handler,
    JSON,
    NewlineFraming,
    PlainText,
    Put,
    ReqBody,
    ServerError,
    SourceIO,
    StreamPost,
    err400,
    err403,
    err404,
    type (:<|>) (..),
    type (:>),
  )

-- handler --------------------------------------------------------------------

type KernelAPI =
  CreateKernelEndpoint
    :<|> KernelStatusEndpoint
    :<|> ExecuteStatementEndpoint

kernelHandler ::
  IORef Register ->
  AppT Handler (UUIDContainer Kernel)
    :<|> (U.UUID -> AppT Handler Kernel)
    :<|> (U.UUID -> ExecuteReqBody -> AppT Handler (SourceIO String))
kernelHandler ioRefRegister =
  let createHandler =
        withErrorHandling0
          handleCreateKernelError
          createKernelAction

      statusHandler =
        withErrorHandling1
          handleKernelStatusError
          ( kernelByUUIDAction
              (lookupIORefMap ioRefRegister)
              readHandleIORef
          )

      runHandler =
        withErrorHandling2
          handleRunError
          ( runAction
              (lookupIORefMap ioRefRegister)
              readHandleIORef
              runStatement
              modifyHandleIORef
          )
   in createHandler :<|> statusHandler :<|> runHandler

-- create kernel endpoint -----------------------------------------------------

type CreateKernelEndpoint = Put '[JSON] (UUIDContainer Kernel)

handleCreateKernelError :: MonadError ServerError m => CreateKernelError -> m r
handleCreateKernelError = \case
  TooManyKernelsError -> errorWithCode err403 "too many kernels running."

-- kernel status endpoint -----------------------------------------------------

type KernelStatusEndpoint = Capture "uuid" U.UUID :> Get '[JSON] Kernel

handleKernelStatusError :: MonadError ServerError m => KernelStatusError -> m r
handleKernelStatusError = \case
  NotFound -> errorWithCode err404 "kernel does not exist."

-- execute endpoint -----------------------------------------------------------

handleRunError :: MonadError ServerError m => RunError -> m r
handleRunError = \case
  SyntaxError err ->
    errorWithMessage err400 "could not parse given statement." err
  UUIDNotFound -> errorWithCode err404 "kernel does not exist."
  KernelIsRunning kernel ->
    errorWithMessage err423 "kernel is already running" kernel

type ExecuteStatementEndpoint =
  Capture "uuid" U.UUID
    :> ReqBody '[JSON] ExecuteReqBody
    :> StreamPost NewlineFraming PlainText (SourceIO String)
