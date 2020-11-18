{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LangT where

import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.State (MonadState, StateT (StateT))
import Control.Monad.Writer
  ( MonadIO,
    MonadWriter,
    WriterT (WriterT),
  )
import Lambda.Lib.Language (Scope)

-- xxx will be removed soon
newtype LangT a = LangT
  { runLangT ::  StateT Scope (ExceptT String IO) a
  }
  deriving
    ( Functor,
      Monad,
      Applicative,
      MonadIO,
      MonadState Scope,
      MonadError String
    )