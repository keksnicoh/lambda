{-# LANGUAGE FlexibleContexts #-}

module Lambda.Notebook.Action.Kernel where

import Control.Monad.Except (MonadError (throwError), when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State (MonadState, gets, modify)
import Data.IORef (readIORef)
import qualified Data.Map as M
import qualified Data.Time as T
import qualified Data.UUID as U
import Lambda.Notebook.Data.Error (getOr)
import Lambda.Notebook.Data.Kernel (Register, createKernelIoRef, Kernel)
import Lambda.Notebook.Dependencies (HasM (..))

-- create kernel  -------------------------------------------------------------

data CreateKernelError = TooManyKernelsError

createKernelAction ::
  ( MonadState Register m,
    HasM T.UTCTime m,
    HasM U.UUID m,
    MonadError CreateKernelError m,
    MonadIO m
  ) =>
  m U.UUID
createKernelAction = do
  nKernels <- gets M.size

  when (nKernels > 10) $ throwError TooManyKernelsError

  uuid <- getM
  currentTime <- getM

  ioRef <- liftIO $ createKernelIoRef currentTime

  modify $ M.insert uuid ioRef
  pure uuid

-- kernel status --------------------------------------------------------------

data KernelStatusError = NotFound
  deriving (Show)

kernelByUUIDAction ::
  (MonadState Register m, MonadError KernelStatusError m, MonadIO m) =>
  U.UUID ->
  m Kernel
kernelByUUIDAction uuid = do
  ioRef <- gets (M.lookup uuid) >>= getOr (throwError NotFound)
  liftIO $ readIORef ioRef
