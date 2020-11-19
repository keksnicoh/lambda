{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Lambda.Notebook.Controller.Execute where

import Control.Monad.Except (MonadError)
import qualified Data.UUID as U
import Lambda.Notebook.Action.Execute (RunError (..), demoEndpoint, runService)
import Lambda.Notebook.App (AppT)
import Lambda.Notebook.Data.Error
  ( err423,
    errorWithCode,
    errorWithMessage,
    withErrorHandling2,
  )
import Servant
  ( Capture,
    Handler,
    NewlineFraming,
    PlainText,
    ReqBody,
    ServerError,
    SourceIO,
    StreamGet,
    StreamPost,
    err400,
    err404,
    type (:<|>) (..),
    type (:>),
  )

-- handler --------------------------------------------------------------------

type ExecuteAPI = ExecuteStatementEndpoint :<|> ("demo" :> ExecuteDemoEndpoint)

executeHandler ::
  (U.UUID -> String -> AppT Handler (SourceIO String))
    :<|> AppT Handler (SourceIO String)
executeHandler =
  withErrorHandling2 executeStatementEndoint runService
    :<|> demoEndpoint

-- demo endpoint --------------------------------------------------------------

type ExecuteDemoEndpoint = StreamGet NewlineFraming PlainText (SourceIO String)

-- execute endpoint -----------------------------------------------------------

executeStatementEndoint :: MonadError ServerError m => RunError -> m r
executeStatementEndoint = \case
  SyntaxError err ->
    errorWithMessage err400 "could not parse given statement." err
  UUIDNotFound -> errorWithCode err404 "kernel does not exist."
  KernelIsRunning kernel -> errorWithMessage err423 "kernel is already running" kernel

type ExecuteStatementEndpoint =
  Capture "uuid" U.UUID
    :> ReqBody '[PlainText] String
    :> StreamPost NewlineFraming PlainText (SourceIO String)
