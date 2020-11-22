{-# LANGUAGE FlexibleContexts #-}

module Lambda.Notebook.Kernel.Action.Status where

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State (MonadState, gets)
import Data.IORef (readIORef)
import qualified Data.Map as M
import qualified Data.UUID as U
import Lambda.Notebook.Error (getOr)
import Lambda.Notebook.Kernel.Model (Kernel, Register)

data KernelStatusError = NotFound
  deriving (Show)

kernelByUUIDAction ::
  (MonadState Register m, MonadError KernelStatusError m, MonadIO m) =>
  U.UUID ->
  m Kernel
kernelByUUIDAction pk = do
  ioRef <- gets (M.lookup pk) >>= getOr (throwError NotFound)
  liftIO $ readIORef ioRef
