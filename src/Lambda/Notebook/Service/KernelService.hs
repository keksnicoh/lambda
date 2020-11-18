{-# LANGUAGE FlexibleContexts #-}

module Lambda.Notebook.Service.KernelService where

import Control.Monad.Except (MonadError (throwError), when)
import Control.Monad.State (MonadState, gets, modify)
import qualified Data.Map as M
import qualified Data.Time as T
import qualified Data.UUID as U
import Lambda.Notebook.Data.Error
  ( HandleError (..),
    errorWithCode,
    getOr,
  )
import Lambda.Notebook.Data.Kernel
  ( Kernel (Kernel, created, execution, invoked, scope),
    Register,
  )
import Lambda.Notebook.Dependencies
  ( HasM (..),
  )
import Servant (err403, err404)

-- create kernel  -------------------------------------------------------------

data CreateKernelError = TooManyKernelsError

instance HandleError CreateKernelError where
  errorToResponse TooManyKernelsError =
    errorWithCode err403 "too many kernels running."

createKernel ::
  ( MonadState Register m,
    HasM T.UTCTime m,
    HasM U.UUID m,
    MonadError CreateKernelError m
  ) =>
  m U.UUID
createKernel = do
  nKernels <- gets M.size
  when (nKernels > 10) $ throwError TooManyKernelsError

  uuid <- getM
  currentTime <- getM
  let kernel =
        Kernel
          { execution = 0,
            scope = M.empty,
            created = currentTime,
            invoked = Nothing
          }
  modify $ M.insert uuid kernel
  pure uuid

-- kernel status --------------------------------------------------------------

data KernelStatusError = NotFound
  deriving (Show)

instance HandleError KernelStatusError where
  errorToResponse NotFound = errorWithCode err404 "kernel does not exist."

kernelStatus ::
  (MonadState Register m, MonadError KernelStatusError m) =>
  U.UUID ->
  m Kernel
kernelStatus uuid = do
  gets (M.lookup uuid) >>= getOr (throwError NotFound)
