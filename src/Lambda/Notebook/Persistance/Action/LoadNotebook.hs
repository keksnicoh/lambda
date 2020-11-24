{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}

module Lambda.Notebook.Persistance.Action.LoadNotebook where

import Control.Monad (when)
import Control.Monad.Except (MonadError (throwError))
import Data.Maybe (isNothing)
import qualified Data.UUID as U
import Lambda.Notebook.Error (GetOr (getOr))
import Lambda.Notebook.Kernel.Model (Kernel)
import Lambda.Notebook.Persistance.Header (LoadNotebookM)
import Lambda.Notebook.Persistance.Model (Notebook (..), emptyNotebook)
import Lambda.Notebook.Storage (LookupM)

data LoadNotebookError
  = LoadNotebookKernelNotFound

loadNotebook ::
  (MonadError LoadNotebookError m) =>
  LookupM U.UUID (h Kernel) m ->
  LoadNotebookM m ->
  U.UUID ->
  m Notebook
loadNotebook lookupKernelIoRef lookupNotebook uuid = do
  kernelIoRef <- lookupKernelIoRef uuid
  when (isNothing kernelIoRef) do
    throwError LoadNotebookKernelNotFound

  lookupNotebook uuid >>= getOr (pure emptyNotebook)
