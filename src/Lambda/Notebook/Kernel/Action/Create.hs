{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Lambda.Notebook.Kernel.Action.Create where

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Reader (MonadIO, MonadReader, asks, when)
import qualified Data.Map as M
import qualified Data.Time as T
import qualified Data.UUID as U
import Lambda.Notebook.Dependencies (HasM (..))
import Lambda.Notebook.Kernel.Header
  ( CreateKernelHandleM,
    HasMaxNumberOfKernels (..),
    ReadRegisterM,
    RegisterKernelHandleM,
  )
import Lambda.Notebook.Kernel.Model
  ( Kernel (..),
    UUIDContainer (..),
    kernelEmpty,
  )

data CreateKernelError = TooManyKernelsError

createKernelAction ::
  ( HasM T.UTCTime m,
    HasM U.UUID m,
    MonadError CreateKernelError m,
    MonadReader e m,
    HasMaxNumberOfKernels e,
    MonadIO m
  ) =>
  -- access kernel handle register
  ReadRegisterM h m ->
  RegisterKernelHandleM h m ->
  -- new kernel handles are created
  CreateKernelHandleM h m ->
  -- result
  m (UUIDContainer Kernel)
createKernelAction readRegister registerKernel createHandle = do
  nKernels <- M.size <$> readRegister
  maxNumberOfKernels <- asks getMaxNumberOfKernels
  when (nKernels > maxNumberOfKernels) do
    throwError TooManyKernelsError

  kernel <- kernelEmpty <$> getM @T.UTCTime
  handle <- createHandle kernel

  pk <- getM @U.UUID
  registerKernel pk handle

  pure
    UUIDContainer
      { uuid = pk,
        value = kernel
      }
