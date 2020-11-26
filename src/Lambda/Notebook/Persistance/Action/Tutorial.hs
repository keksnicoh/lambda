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
            { code = "- a variable\n;" ++ "a;"
            },
          emptyBlock
            { code = "- an application\n;" ++ "ab;"
            },
            emptyBlock
            { code = "- a lambda function\n;" ++ "λx.x;"
            },
          emptyBlock
            { code = "- default application precedence\n;" ++ "(((ab)c)d);"
            },
          emptyBlock
            { code = "a(b(cd));"
            },
          emptyBlock
            { code = "\\x.x;"
            },
          emptyBlock
            { code = "abc [b/a];"
            },
          emptyBlock
            { code = "contract[(λx.x)y];"
            },
          emptyBlock
            { code = "free[(λx.xy)];"
            },
          emptyBlock
            { code = "bound[(λx.xy)];"
            },
          emptyBlock
            { code = "(λx.xy) [(λxy.x)/y];"
            },
          emptyBlock
            { code = "T=λxy.x;\nF=λxy.y;\n\nresolve[TFT];"
            },
          emptyBlock
            { code = "contract[resolve[TFT]];"
            },
          emptyBlock
            { code = "contract[2, resolve[TFT]];"
            },
          emptyBlock
            { code = "contract[3,resolve[TFT]];"
            }
        ]
    }

commandsTutorial :: Notebook
commandsTutorial =
  emptyNotebook
    { title = "Available Commands",
      blocks =
        [ emptyBlock
            { code = "free[λx.xy];"
            },
          emptyBlock
            { code = "bound[λxy.y];"
            }
        ]
    }
