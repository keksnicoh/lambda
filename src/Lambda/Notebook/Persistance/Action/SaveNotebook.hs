{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}

module Lambda.Notebook.Persistance.Action.SaveNotebook where

import Control.Monad (when)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Reader (MonadReader, asks)
import Data.IORef (IORef)
import Data.List (find)
import Data.Maybe (isJust, isNothing)
import qualified Data.UUID as U
import Lambda.Notebook.Dependencies
  ( HasNotebookMaxBlocks (..),
    HasNotebookMaxCodeSize (..),
  )
import Lambda.Notebook.Kernel.Model (Kernel)
import Lambda.Notebook.Persistance.Model (Block (..), Notebook (..))
import Lambda.Notebook.Storage (LookupM)
import Lambda.Notebook.Persistance.Header ( SaveNotebookM )


data SaveNotebookError
  = SaveNotbookKernelNotFound
  | TooManyBlocks
  | MaxCodeLength

saveNotebook ::
  ( MonadError SaveNotebookError m,
    MonadReader e m,
    HasNotebookMaxBlocks e,
    HasNotebookMaxCodeSize e
  ) =>
  LookupM U.UUID (IORef Kernel) m ->
  SaveNotebookM m ->
  U.UUID ->
  Notebook ->
  m ()
saveNotebook lookupKernel persistHandler uuid notebook = do
  kernel <- lookupKernel uuid
  when (isNothing kernel) do
    throwError SaveNotbookKernelNotFound

  notebookMaxBlocks <- asks getNotebookMaxBlocks
  when (length (blocks notebook) > notebookMaxBlocks) do
    throwError TooManyBlocks

  notebookMaxCodeSize <- asks getNotebookMaxCodeSize
  let tooLongCode = (> notebookMaxCodeSize) . length . code
  when (isJust (find tooLongCode (blocks notebook))) do
    throwError MaxCodeLength

  persistHandler uuid notebook
