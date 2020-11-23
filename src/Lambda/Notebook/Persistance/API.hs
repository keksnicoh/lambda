{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Lambda.Notebook.Persistance.API where

import Control.Monad.Except (MonadError)
import Data.IORef (IORef)
import qualified Data.UUID as U
import Lambda.Notebook.App (AppT)
import Lambda.Notebook.Error
  ( errorWithCode,
    withErrorHandling1,
    withErrorHandling2,
  )
import Lambda.Notebook.Kernel.Model (Register)
import Lambda.Notebook.Persistance.Action.LoadNotebook
  ( LoadNotebookError (..),
    loadNotebook,
  )
import Lambda.Notebook.Persistance.Action.SaveNotebook
  ( SaveNotebookError (..),
    saveNotebook,
  )
import Lambda.Notebook.Persistance.Header (NotebookStorage)
import Lambda.Notebook.Persistance.Model (Notebook)
import Lambda.Notebook.Storage (inserIORefMap, lookupIORefMap)
import Servant
  ( Capture,
    Get,
    Handler,
    JSON,
    Post,
    ReqBody,
    ServerError,
    err400,
    err404,
    type (:<|>) (..),
    type (:>),
  )

-- handler --------------------------------------------------------------------

type PersistanceAPI =
  SaveNotebookEndpoint :<|> LoadNotebookEndpoint

persistanceHandler ::
  IORef Register ->
  IORef NotebookStorage ->
  (U.UUID -> Notebook -> AppT Handler ())
    :<|> (U.UUID -> AppT Handler Notebook)
persistanceHandler kernelStorage notebookStorage =
  withErrorHandling2
    saveEndpointErrorHandler
    ( saveNotebook
        (lookupIORefMap kernelStorage)
        (inserIORefMap notebookStorage)
    )
    :<|> withErrorHandling1
      loadNotebookErrorHandler
      ( loadNotebook
          (lookupIORefMap kernelStorage)
          (lookupIORefMap notebookStorage)
      )

-- save notebook --------------------------------------------------------------

type SaveNotebookEndpoint =
  Capture "uuid" U.UUID
    :> ReqBody '[JSON] Notebook
    :> Post '[JSON] ()

saveEndpointErrorHandler ::
  MonadError ServerError m =>
  SaveNotebookError ->
  m r
saveEndpointErrorHandler = \case
  SaveNotbookKernelNotFound -> errorWithCode err404 "kernel does not exist."
  TooManyBlocks -> errorWithCode err400 "number of blocks reached limit"
  MaxCodeLength -> errorWithCode err400 "code length limit reched"

-- load notebook --------------------------------------------------------------

type LoadNotebookEndpoint =
  Capture "uuid" U.UUID :> Get '[JSON] Notebook

loadNotebookErrorHandler ::
  MonadError ServerError m =>
  LoadNotebookError ->
  m r
loadNotebookErrorHandler = \case
  LoadNotebookKernelNotFound -> errorWithCode err404 "kernel does not exist."
