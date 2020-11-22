{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Lambda.Notebook.Kernel.API where

import Control.Monad.Except (MonadError)
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
    runService,
  )
import Lambda.Notebook.Kernel.Action.Status
  ( KernelStatusError (..),
    kernelByUUIDAction,
  )
import Lambda.Notebook.Kernel.Model (Kernel, UUIDContainer)
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
  AppT Handler (UUIDContainer Kernel)
    :<|> ((U.UUID -> AppT Handler Kernel) :<|> (U.UUID -> ExecuteReqBody -> AppT Handler (SourceIO String)))
kernelHandler =
  withErrorHandling0 createKernelEndpoint createKernelAction
    :<|> withErrorHandling1 kernelStatusEndpoint kernelByUUIDAction
    :<|> withErrorHandling2 executeStatementEndoint runService

-- create kernel endpoint -----------------------------------------------------

type CreateKernelEndpoint = Put '[JSON] (UUIDContainer Kernel)

createKernelEndpoint :: MonadError ServerError m => CreateKernelError -> m r
createKernelEndpoint = \case
  TooManyKernelsError -> errorWithCode err403 "too many kernels running."

-- kernel status endpoint -----------------------------------------------------

type KernelStatusEndpoint = Capture "uuid" U.UUID :> Get '[JSON] Kernel

kernelStatusEndpoint :: MonadError ServerError m => KernelStatusError -> m r
kernelStatusEndpoint = \case
  NotFound -> errorWithCode err404 "kernel does not exist."

-- execute endpoint -----------------------------------------------------------

executeStatementEndoint :: MonadError ServerError m => RunError -> m r
executeStatementEndoint = \case
  SyntaxError err ->
    errorWithMessage err400 "could not parse given statement." err
  UUIDNotFound -> errorWithCode err404 "kernel does not exist."
  KernelIsRunning kernel ->
    errorWithMessage err423 "kernel is already running" kernel

type ExecuteStatementEndpoint =
  Capture "uuid" U.UUID
    :> ReqBody '[JSON] ExecuteReqBody
    :> StreamPost NewlineFraming PlainText (SourceIO String)
