{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Lambda.Notebook.Persistance.Action.ListNotebooks where

import qualified Data.UUID as U
import Lambda.Notebook.Persistance.Header (ListNotebooksM)
import Lambda.Notebook.Persistance.Model (Notebook (..))
import Lambda.Notebook.Storage
  ( IdentifiedValue (IdentifiedValue),
  )

-- action ---------------------------------------------------------------------

listNotebooks ::
  Functor m =>
  ListNotebooksM m ->
  m [IdentifiedValue U.UUID String]
listNotebooks readNotebooks =
  fmap f <$> readNotebooks
  where
    f (uuid, notebook) = IdentifiedValue uuid (title notebook)
