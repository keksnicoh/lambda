module Lambda.Notebook.Persistance.Header where

import qualified Data.Map as M
import qualified Data.UUID as U
import Lambda.Notebook.Persistance.Model (Notebook)
import Lambda.Notebook.Storage (InsertM)

-- api ------------------------------------------------------------------------

-- | notebooks are stored within a simple map
type NotebookStorage = M.Map U.UUID Notebook

-- | get notebook by uuid
type LoadNotebookM m = U.UUID -> m (Maybe Notebook)

-- | persist notebook under certain uuid
type SaveNotebookM m = InsertM U.UUID Notebook m

-- type classes ---------------------------------------------------------------

class HasNotebookMaxBlocks e where
  getNotebookMaxBlocks :: e -> Int

class HasNotebookMaxCodeSize e where
  getNotebookMaxCodeSize :: e -> Int