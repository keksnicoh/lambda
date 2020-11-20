{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Lambda.Notebook.Data.Kernel
  ( Kernel (..),
    KernelStatus (..),
    UUIDContainer (..),
    Register,
    createKernelIoRef,
    updateKernelRunning,
    updateKernel,
  )
where

import Conduit (MonadIO (..))
import Data.Aeson (ToJSON)
import Data.IORef (IORef, modifyIORef', newIORef)
import qualified Data.Map as M
import qualified Data.Time as T
import qualified Data.UUID as U
import GHC.Generics (Generic)
import Lambda.Lib.Language (Scope)

-- data -----------------------------------------------------------------------

data UUIDContainer a = UUIDContainer {uuid :: U.UUID, value :: a}
  deriving (Generic, ToJSON, Show)

data KernelStatus = Running | Success | RuntimeError
  deriving (Generic, ToJSON, Eq, Show)

data Kernel = Kernel
  { execution :: Int,
    scope :: Scope,
    created :: T.UTCTime,
    invoked :: Maybe T.UTCTime,
    status :: Maybe KernelStatus
  }
  deriving (Generic, ToJSON, Show)

type Register = M.Map U.UUID (IORef Kernel)

-- actions --------------------------------------------------------------------

createKernelIoRef :: T.UTCTime -> IO (IORef Kernel, Kernel)
createKernelIoRef currentTime =
  (,kernel) <$> newIORef kernel
  where
    kernel =
      Kernel
        { execution = 0,
          scope = M.empty,
          created = currentTime,
          status = Nothing,
          invoked = Nothing
        }

updateKernelRunning :: MonadIO m => IORef Kernel -> Kernel -> m ()
updateKernelRunning kernelIORef kernel = do
  now <- liftIO T.getCurrentTime
  updateKernel kernelIORef $
    kernel
      { invoked = Just now,
        execution = execution kernel + 1,
        status = Just Running
      }

updateKernel :: MonadIO m => IORef Kernel -> Kernel -> m ()
updateKernel kernelIORef kernel = do
  liftIO $ modifyIORef' kernelIORef (const kernel)
