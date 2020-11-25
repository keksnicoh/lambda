{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Lambda.Notebook.Persistance.API where

import Control.Monad.Except (MonadError)
import Data.IORef (IORef)
import qualified Data.Map as M
import qualified Data.UUID as U
import Lambda.Notebook.App (AppT)
import Lambda.Notebook.Error
  ( errorWithCode,
    errorWithMessage,
    withErrorHandling0,
    withErrorHandling1,
    withErrorHandling2,
  )
import Lambda.Notebook.Kernel.Model (Register)
import Lambda.Notebook.Persistance.Action.CreateNotebook
  ( CreateNotebookError (..),
    createNotebook,
  )
import Lambda.Notebook.Persistance.Action.ListNotebooks (listNotebooks)
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
import Lambda.Notebook.Storage
  ( IdentifiedValue,
    insertIORefMap,
    lookupIORefMap,
    readHandleIORef,
  )
import Servant
  ( Capture,
    Get,
    Handler,
    JSON,
    Post,
    Put,
    ReqBody,
    ServerError,
    err400,
    err404,
    err409,
    type (:<|>) (..),
    type (:>),
  )

-- handler --------------------------------------------------------------------

type PersistanceAPI =
  ListNotebooksEndpoint :<|> CreateNotebookEndpoint :<|> SaveNotebookEndpoint :<|> LoadNotebookEndpoint

persistanceHandler ::
  IORef (Register IORef) ->
  IORef NotebookStorage ->
  AppT Handler [IdentifiedValue U.UUID String]
    :<|> AppT Handler (IdentifiedValue U.UUID Notebook)
    :<|> (U.UUID -> Notebook -> AppT Handler ())
    :<|> (U.UUID -> AppT Handler Notebook)
persistanceHandler kernelStorage notebookStorage =
  listNotebookHandler :<|> createNotebookHandler :<|> saveNotebookHandler :<|> loadNotebookHandler
  where
    listNotebookHandler = listNotebooks (M.toList <$> readHandleIORef notebookStorage)
    createNotebookHandler =
      withErrorHandling0
        createNotebookErrorHandler
        ( createNotebook
            (M.size <$> readHandleIORef notebookStorage)
            (insertIORefMap notebookStorage)
        )

    saveNotebookHandler =
      withErrorHandling2
        saveEndpointErrorHandler
        ( saveNotebook
            (lookupIORefMap notebookStorage)
            (lookupIORefMap kernelStorage)
            (insertIORefMap notebookStorage)
        )

    loadNotebookHandler =
      withErrorHandling1
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
  TooManyBlocks -> errorWithCode err400 "number of blocks reached limit"
  MaxCodeLength -> errorWithCode err400 "code length limit reched"
  NotebookNotFound -> errorWithCode err404 "notebook does not exist"
  KernelDoesNotExist -> errorWithCode err400 "kernel does not exist"

-- load notebook --------------------------------------------------------------

type LoadNotebookEndpoint =
  Capture "uuid" U.UUID :> Get '[JSON] Notebook

loadNotebookErrorHandler ::
  MonadError ServerError m =>
  LoadNotebookError ->
  m r
loadNotebookErrorHandler = \case
  LoadNotebookNotFound -> errorWithCode err404 "kernel does not exist."

-- create notebook ------------------------------------------------------------

type CreateNotebookEndpoint = Put '[JSON] (IdentifiedValue U.UUID Notebook)

createNotebookErrorHandler ::
  MonadError ServerError m =>
  CreateNotebookError ->
  m r
createNotebookErrorHandler = \case
  TooManyNotebooks n -> errorWithMessage err409 "too many notebooks exist" n

-- list notebooks -------------------------------------------------------------

type ListNotebooksEndpoint = Get '[JSON] [IdentifiedValue U.UUID String]