{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}

module Lambda.Notebook.Persistance.Action.Tutorial where

import Control.Monad (forM_)
import qualified Data.UUID as U
import Lambda.Notebook.Dependencies (HasM (..))
import Lambda.Notebook.Persistance.Header
  ( SaveNotebookM,
  )
import Lambda.Notebook.Persistance.Model
  ( Block (..),
    Notebook (..),
    emptyBlock,
    emptyNotebook,
  )

-- action ---------------------------------------------------------------------

newtype CreateNotebookError
  = TooManyNotebooks Int

createTutorialNotebooks ::
  ( Monad m,
    HasM U.UUID m
  ) =>
  SaveNotebookM m ->
  m ()
createTutorialNotebooks persistHandler = do
  forM_
    [ basicsTutorial,
      commandsTutorial
    ]
    \notebook -> getM >>= flip persistHandler notebook

basicsTutorial :: Notebook
basicsTutorial =
  emptyNotebook
    { title = "Basics",
      blocks =
        [ emptyBlock
            { code = "contract[(λx.x)a];"
            },
          emptyBlock
            { code = "(λx.gx) [(λxy.y) / g];"
            }
        ]
    }

commandsTutorial :: Notebook
commandsTutorial =
  emptyNotebook
    { title = "Available Commands",
      blocks =
        [ emptyBlock
            { code = "free[λxy.y];"
            },
          emptyBlock
            { code = "bound[λxy.y];"
            }
        ]
    }
