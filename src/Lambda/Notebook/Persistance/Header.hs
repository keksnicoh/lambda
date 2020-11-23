module Lambda.Notebook.Persistance.Header where

import qualified Data.Map as M
import qualified Data.UUID as U
import Lambda.Notebook.Persistance.Model (Notebook)
import Lambda.Notebook.Storage (InsertM)

type NotebookStorage = M.Map U.UUID Notebook

type LoadNotebookM m = U.UUID -> m (Maybe Notebook)

type SaveNotebookM m = InsertM U.UUID Notebook m
