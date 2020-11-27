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
            { code =
                "-- variables have one character\n"
                  ++ "x"
            },
          emptyBlock
            { code =
                "-- expressions are applied with left precedence\n"
                  ++ "(ab)c\n"
                  ++ "a(bc)"
            },
          emptyBlock
            { code =
                "-- subsitution is the only transformation in lambda calculus"
                  ++ ", here we substitute e for b in the expression abc:\n"
                  ++ "abc [e/b]\n"
                  ++ "-- multiple substitutions:\n"
                  ++ "abc [e/b][a/c]"
            },
          emptyBlock
            { code =
                "-- variables are bound within lambda functions "
                  ++ "λ<argument>.<expression>\n"
                  ++ "λx.ax"
            },
          emptyBlock
            { code =
                "-- variables which are not bound to a lambda function are"
                  ++ "\"free\"\n"
                  ++ "free[λx.ax]\n\n"
                  ++ "--here, x is bound\n"
                  ++ "bound[λx.ax]"
            },
          emptyBlock
            { code =
                "-- any expression can be applied to a lambda function\n"
                  ++ "(λx.ax)b\n\n"
                  ++ "-- beta reduction contracts an expression\n"
                  ++ "contract[(λx.ax)b]"
            },
          emptyBlock
            { code =
                "-- functions of multiple arguments are implemented "
                  ++ "through currying\n"
                  ++ "λx.λy.λz.zyx"
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
