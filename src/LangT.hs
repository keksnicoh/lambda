{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LangT where

import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.State (MonadState, StateT (StateT))
import Control.Monad.Writer
  ( MonadIO,
    MonadWriter,
    WriterT (WriterT),
  )
import Language (Scope)

newtype LangT a = LangT
  { runLangT :: WriterT [String] (StateT Scope (ExceptT String IO)) a
  }
  deriving
    ( Functor,
      Monad,
      Applicative,
      MonadIO,
      MonadState Scope,
      MonadError String,
      MonadWriter [String]
    )