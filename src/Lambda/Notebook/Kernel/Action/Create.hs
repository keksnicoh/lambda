{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Lambda.Notebook.Kernel.Action.Create where

import Control.Monad.Except (MonadError (throwError), when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State (MonadState, gets, modify)
import Data.IORef (IORef, newIORef)
import qualified Data.Map as M
import qualified Data.Time as T
import qualified Data.UUID as U
import Lambda.Notebook.Dependencies (HasM (..))
import Lambda.Notebook.Kernel.Model (Kernel (..), Register, UUIDContainer (..), kernelEmpty)

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

  currentTime <- getM
  (ioRef, kernel) <- liftIO $ createKernelIoRef currentTime

  pk <- getM
  modify $ M.insert pk ioRef
  pure $ UUIDContainer {uuid = pk, value = kernel}

createKernelIoRef :: T.UTCTime -> IO (IORef Kernel, Kernel)
createKernelIoRef currentTime =
  (,kernel) <$> newIORef kernel
  where
    kernel = kernelEmpty currentTime