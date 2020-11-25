{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Lambda.Notebook.Persistance.Action.LoadNotebook where

import Control.Monad (forM)
import Control.Monad.Except (MonadError (throwError))
import qualified Data.UUID as U
import Lambda.Notebook.Error (GetOr (getOr))
import Lambda.Notebook.Kernel.Header
import Lambda.Notebook.Persistance.Header (LookupNotebookM)
import Lambda.Notebook.Persistance.Model (Notebook (..))

data LoadNotebookError
  = LoadNotebookNotFound

loadNotebook ::
  (MonadError LoadNotebookError m) =>
  LookupKernelHandleM h m ->
  LookupNotebookM m ->
  U.UUID ->
  m Notebook
loadNotebook lookupKernel lookupNotebook uuid = do
  -- notebook or fail
  notebook <- lookupNotebook uuid >>= getOr (throwError LoadNotebookNotFound)

  -- check whether kernel exists
  validKernelPk <- forM (kernelPk notebook) \pk ->
    pk <$ lookupKernel pk

  pure
    notebook
      { kernelPk = validKernelPk
      }