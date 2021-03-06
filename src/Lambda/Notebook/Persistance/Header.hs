module Lambda.Notebook.Persistance.Header where

import qualified Data.Map as M
import qualified Data.UUID as U
import Lambda.Notebook.Persistance.Model (Notebook)
import Lambda.Notebook.Storage (ReadM, InsertM)

-- api ------------------------------------------------------------------------

-- | notebooks are stored within a simple map
type NotebookStorage = M.Map U.UUID Notebook

-- | returns the number of notebooks in storage
type NumberOfNotebooksM m = m Int

-- | get notebook by uuid
type LookupNotebookM m = U.UUID -> m (Maybe Notebook)

-- | persist notebook under certain uuid
type SaveNotebookM m = InsertM U.UUID Notebook m

-- | list of available notebooks
type ListNotebooksM m = ReadM [(U.UUID, Notebook)] m

-- type classes ---------------------------------------------------------------

class HasNotebookMaxBlocks e where
  getNotebookMaxBlocks :: e -> Int

class HasNotebookMaxCodeSize e where
  getNotebookMaxCodeSize :: e -> Int

class HasMaxNotebookNumber e where
  getMaxNotebookNumber :: e -> Int