{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Lambda.Notebook.Action.Execute where

import Conduit (MonadIO (..), yield)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State (MonadState, gets)
import Data.IORef
import qualified Data.Map as M
import qualified Data.Time as T
import qualified Data.UUID as U
import Lambda.Lib.Language (Statement, StdOut, parse)
import Lambda.Notebook.Data.Error (getOr)
import Lambda.Notebook.Data.Kernel (Kernel, Register, scope, updateKernel, yieldError, yieldResult)
import Lambda.Notebook.Dependencies (HasM)
import Lambda.Notebook.Runtime (execute)
import Servant (SourceIO, ToSourceIO (..))

-- demo endpoint --------------------------------------------------------------

data RunError = UUIDNotFound | SyntaxError String
  deriving (Show)

runService ::
  ( MonadState Register m,
    HasM T.UTCTime m,
    MonadIO m,
    MonadError RunError m
  ) =>
  U.UUID ->
  String ->
  m (SourceIO String)
runService uuid code = do
  kernelIORef <- gets (M.lookup uuid) >>= getKernel
  statement <- getStatement (parse code)
  pure . toSourceIO $ runKernel @IO kernelIORef statement
  where
    getStatement = getOr (throwError . SyntaxError . show)
    getKernel = getOr (throwError UUIDNotFound)

runKernel :: MonadIO m => IORef Kernel -> Statement -> StdOut m ()
runKernel kernelIORef statement = do
  kernel <- liftIO $ readIORef kernelIORef
  execute (scope kernel) statement >>= \case
    Right newScope -> do
      yieldResult newScope
      updateKernel kernelIORef kernel {scope = newScope}
    Left runtimeError -> yieldError runtimeError

-- demo endpoint --------------------------------------------------------------

demoEndpoint ::
  (MonadState Register m, HasM T.UTCTime m, MonadIO m) =>
  m (SourceIO String)
demoEndpoint = do
  let code =
        "a=bc;hnfPrintSteps[20, (\\xy.x)(\\xy.y)(\\xy.x)(\\xy.x)(\\xy.y)"
          ++ "(\\xy.x)(\\xy.x)(\\xy.y)(\\xy.x)(\\xy.x)(\\xy.y)(\\xy.x)];"
      Right statement = parse code
  pure $ toSourceIO (execute @IO M.empty statement >>= yield . show)
