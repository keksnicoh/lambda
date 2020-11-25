{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Lambda.Notebook.Persistance.Action.SaveNotebook where

import Control.Monad (forM_, void, when, (>=>))
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Reader (MonadReader, asks)
import Data.List (find)
import Data.Maybe (isJust)
import qualified Data.UUID as U
import Lambda.Notebook.Error (getOr)
import Lambda.Notebook.Kernel.Header (LookupKernelHandleM)
import Lambda.Notebook.Persistance.Header
  ( HasNotebookMaxBlocks (..),
    HasNotebookMaxCodeSize (..),
    LookupNotebookM,
    SaveNotebookM,
  )
import Lambda.Notebook.Persistance.Model (Block (..), Notebook (..))

-- action ---------------------------------------------------------------------

data SaveNotebookError
  = TooManyBlocks
  | MaxCodeLength
  | NotebookNotFound
  | KernelDoesNotExist

saveNotebook ::
  ( MonadError SaveNotebookError m,
    MonadReader e m,
    HasNotebookMaxBlocks e,
    HasNotebookMaxCodeSize e
  ) =>
  LookupNotebookM m ->
  LookupKernelHandleM h m ->
  SaveNotebookM m ->
  U.UUID ->
  Notebook ->
  m ()
saveNotebook lookupNotebook lookupKernel persistNotebook uuid notebook = do
  -- check whether notebook exists
  void $
    lookupNotebook uuid >>= getOr do
      throwError NotebookNotFound

  -- check whether kernel exists
  forM_ (kernelPk notebook) do
    lookupKernel >=> getOr do
      throwError KernelDoesNotExist

  -- validate max number of blocks
  notebookMaxBlocks <- asks getNotebookMaxBlocks
  when (length (blocks notebook) > notebookMaxBlocks) do
    throwError TooManyBlocks

  -- validate max size of code per block
  notebookMaxCodeSize <- asks getNotebookMaxCodeSize
  let tooLongCode = (> notebookMaxCodeSize) . length . code
  when (isJust (find tooLongCode (blocks notebook))) do
    throwError MaxCodeLength

  persistNotebook uuid notebook
