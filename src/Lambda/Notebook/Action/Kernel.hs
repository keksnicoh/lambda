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
import Lambda.Notebook.Data.Kernel (Kernel, Register, UUIDContainer (..), createKernelIoRef)
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
  m (UUIDContainer Kernel)
createKernelAction = do
  nKernels <- gets M.size

  when (nKernels > 10) $ throwError TooManyKernelsError

  pk <- getM
  currentTime <- getM

  (ioRef, kernel) <- liftIO $ createKernelIoRef currentTime

  modify $ M.insert pk ioRef
  pure $ UUIDContainer {uuid = pk, value = kernel}

-- kernel status --------------------------------------------------------------

data KernelStatusError = NotFound
  deriving (Show)

kernelByUUIDAction ::
  (MonadState Register m, MonadError KernelStatusError m, MonadIO m) =>
  U.UUID ->
  m Kernel
kernelByUUIDAction pk = do
  ioRef <- gets (M.lookup pk) >>= getOr (throwError NotFound)
  liftIO $ readIORef ioRef
