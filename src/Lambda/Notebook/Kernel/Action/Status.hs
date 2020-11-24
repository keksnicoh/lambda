{-# LANGUAGE FlexibleContexts #-}

module Lambda.Notebook.Kernel.Action.Status where

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.UUID as U
import Lambda.Notebook.Error (getOr)
import Lambda.Notebook.Kernel.Header (LookupKernelHandleM)
import Lambda.Notebook.Kernel.Model (Kernel)
import Lambda.Notebook.Storage (ReadHandleM)

data KernelStatusError = NotFound
  deriving (Show)

kernelByUUIDAction ::
  (MonadError KernelStatusError m, MonadIO m) =>
  LookupKernelHandleM h m ->
  ReadHandleM h Kernel m ->
  U.UUID ->
  m Kernel
kernelByUUIDAction lookupKernelHandle readKernelHandle pk = do
  handle <- lookupKernelHandle pk >>= getOr (throwError NotFound)
  readKernelHandle handle
