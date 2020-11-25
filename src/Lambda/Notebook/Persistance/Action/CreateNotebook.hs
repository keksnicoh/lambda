{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}

module Lambda.Notebook.Persistance.Action.CreateNotebook where

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Reader (MonadReader, asks, when)
import qualified Data.UUID as U
import Lambda.Notebook.Dependencies (HasM (..))
import Lambda.Notebook.Persistance.Header
  ( HasMaxNotebookNumber (..),
    NumberOfNotebooksM,
    SaveNotebookM,
  )
import Lambda.Notebook.Persistance.Model
  ( Notebook (..),
    emptyNotebook,
  )
import Lambda.Notebook.Storage (IdentifiedValue (..))

-- action ---------------------------------------------------------------------

newtype CreateNotebookError
  = TooManyNotebooks Int

createNotebook ::
  ( MonadError CreateNotebookError m,
    MonadReader e m,
    HasMaxNotebookNumber e,
    HasM U.UUID m
  ) =>
  NumberOfNotebooksM m ->
  SaveNotebookM m ->
  m (IdentifiedValue U.UUID Notebook)
createNotebook getNumberOfNotebooks persistHandler = do
  -- validate number of notebook threshold
  numberOfNotebooks <- getNumberOfNotebooks
  maxNumberOfNotebook <- asks getMaxNotebookNumber
  when (numberOfNotebooks >= maxNumberOfNotebook) do
    throwError $ TooManyNotebooks maxNumberOfNotebook

  -- create & persist empty notebook
  uuid <- getM
  persistHandler uuid emptyNotebook

  -- combine result
  pure
    IdentifiedValue
      { pk = uuid,
        value = emptyNotebook
      }
