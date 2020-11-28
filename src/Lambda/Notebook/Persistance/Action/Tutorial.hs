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
    { title = "Tutorial 1 - Basics",
      blocks =
        [ emptyBlock
            { code =
                "-- a variable is given by a single character\n"
                  ++ "x"
            },
          emptyBlock
            { code =
                "-- variables can be applied to each other.\n"
                  ++ "-- here the variable b is applied to a\n"
                  ++ "ab\n"
                  ++ "-- whitespaces are allowed\n"
                  ++ "  a   b"
            },
          emptyBlock
            { code =
                "-- application is left associative\n"
                  ++ "(ab)c\n"
                  ++ "a(bc)"
            },
          emptyBlock
            { code =
                "-- subsitution is the only available transformation.\n"
                  ++ "Substitute e for b in the expression abc:\n"
                  ++ "abc [e/b]\n"
                  ++ "-- multiple substitutions:\n"
                  ++ "abc [e/b][a/c]"
            },
          emptyBlock
            { code =
                "-- The λ sign denotes a lambda abstraction. "
                  ++ "λ<argument>.<expression>\n"
                  ++ "λx.ax\n"
                  ++ "--for convenience one can also use backslash\n"
                  ++ "\\x.ax"
            },
          emptyBlock
            { code =
                "-- variables which are not bound to a lambda abstraction are"
                  ++ "\"free\"\n"
                  ++ "free[λx.ax]\n\n"
                  ++ "-- in contrast, bound variables are\n"
                  ++ "bound[λx.ax]"
            },
          emptyBlock
            { code =
                "-- contraction of abstractions is done through substitution:\n"
                  ++ "contract[(λx.ax)b]\n"
                  ++ "-- which is equal to\n"
                  ++ "ax[b/x]"
            },
          emptyBlock
            { code =
                "-- multiple arguments are implemented by chaining lambda \n"
                  ++ "functions (currying)\n"
                  ++ "λx.λy.λz.zyx"
            },
          emptyBlock
            { code =
                "-- any result can be assigned to variables to resolve"
                  ++ " free variables\n"
                  ++ "a=b\n"
            },
          emptyBlock
            { code =
                "-- free variables can be resolved by the "
                  ++ "existing scope \n"
                  ++ "resolve[aca]"
            }
        ]
    }

commandsTutorial :: Notebook
commandsTutorial =
  emptyNotebook
    { title = "Available Commands",
      blocks =
        [ emptyBlock
            { code =
                "-- contract[]\n"
                  ++ "--\n"
                  ++ "-- performs beta reduction on the given expression\n"
                  ++ "contract[(λx.x)(λx.x)a]\n\n"
                  ++ "-- multiple contractions\n"
                  ++ "contract[2, (λx.x)(λx.x)a]\n"
            },
          emptyBlock
            { code =
                "-- resolve[]\n"
                  ++ "--\n"
                  ++ "-- satisfies free variable with existing scope.\n"
                  ++ "a=λx.x\n"
                  ++ "resolve[ab]"
            },
          emptyBlock
            { code = "hnfPrintSteps[λxy.y]"
            },
          emptyBlock
            { code = "scope[λxy.y]"
            },
          emptyBlock
            { code = "free[λxy.y]"
            }
        ]
    }
