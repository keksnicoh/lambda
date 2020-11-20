{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Lambda.Notebook.Controller.Kernel where

import Control.Monad.Except (MonadError)
import qualified Data.UUID as U
import Lambda.Notebook.Action.Kernel
  ( CreateKernelError (..),
    KernelStatusError (..),
    createKernelAction,
    kernelByUUIDAction,
  )
import Lambda.Notebook.App (AppT)
import Lambda.Notebook.Data.Error
  ( errorWithCode,
    withErrorHandling0,
    withErrorHandling1,
  )
import Lambda.Notebook.Data.Kernel (UUIDContainer, Kernel)
import Servant
  ( Capture,
    Get,
    JSON,
    Put,
    type (:<|>) (..),
    type (:>),
  )
import Servant.Server (Handler, ServerError, err403, err404)

-- handler --------------------------------------------------------------------

type KernelAPI = CreateKernelEndpoint :<|> KernelStatusEndpoint

kernelHandler :: AppT Handler (UUIDContainer Kernel) :<|> (U.UUID -> AppT Handler Kernel)
kernelHandler =
  withErrorHandling0 createKernelEndpoint createKernelAction
    :<|> withErrorHandling1 kernelStatusEndpoint kernelByUUIDAction

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
