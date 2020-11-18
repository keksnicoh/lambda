{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Lambda.Notebook.Data.Kernel
  ( Kernel (..),
    Register,
    createKernelIoRef,
    runKernel,
    updateKernel,
    yieldResult,
    yieldError,
  )
where

import Conduit (ConduitT, MonadIO (..), yield)
import Data.Aeson (ToJSON)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (intercalate)
import qualified Data.Map as M
import qualified Data.Time as T
import qualified Data.UUID as U
import GHC.Generics (Generic)
import Lambda.Lib.Language (Scope, Statement, StdOut)
import Lambda.Notebook.Runtime (execute)

-- data -----------------------------------------------------------------------

data Kernel = Kernel
  { execution :: Int,
    scope :: Scope,
    created :: T.UTCTime,
    invoked :: Maybe T.UTCTime
  }
  deriving (Generic, ToJSON)

type Register = M.Map U.UUID (IORef Kernel)

-- actions --------------------------------------------------------------------

createKernelIoRef :: T.UTCTime -> IO (IORef Kernel)
createKernelIoRef currentTime =
  newIORef $
    Kernel
      { execution = 0,
        scope = M.empty,
        created = currentTime,
        invoked = Nothing
      }

updateKernel :: MonadIO m => IORef Kernel -> Kernel -> m ()
updateKernel kernelIORef kernel = do
  now <- liftIO T.getCurrentTime
  let newKernel =
        kernel
          { invoked = Just now,
            execution = execution kernel + 1
          }
  liftIO $ modifyIORef' kernelIORef (const newKernel)

-- Conduit --------------------------------------------------------------------

runKernel :: MonadIO m => IORef Kernel -> Statement -> StdOut m ()
runKernel kernelIORef statement = do
  kernel <- liftIO $ readIORef kernelIORef
  execute (scope kernel) statement >>= \case
    Right newScope -> do
      yieldResult newScope
      updateKernel kernelIORef kernel {scope = newScope}
    Left runtimeError -> yieldError runtimeError

yieldResult :: Monad m => M.Map String a -> ConduitT i String m ()
yieldResult runtimeScope = do
  yield $ "(exit ok, scope " ++ intercalate "," (M.keys runtimeScope) ++ ")"

yieldError :: Monad m => String -> ConduitT i String m ()
yieldError runtimeError = do
  yield "================================================"
  yield " Runtime Error"
  yield "================================================"
  yield ""
  yield runtimeError
