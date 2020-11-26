{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Lambda.Notebook.Server where

import Control.Monad.Reader (ReaderT (runReaderT))
import Data.IORef (IORef, newIORef)
import qualified Data.Map as M
import Lambda.Notebook.App (AppT (runAppT), Env (..))
import Lambda.Notebook.Kernel.API (KernelAPI, kernelHandler)
import Lambda.Notebook.Kernel.Model (Register)
import Lambda.Notebook.Persistance.API
  ( PersistanceAPI,
    persistanceHandler,
  )
import Lambda.Notebook.Persistance.Action.Tutorial
  ( createTutorialNotebooks,
  )
import Lambda.Notebook.Persistance.Header (NotebookStorage)
import Lambda.Notebook.Persistance.Model ()
import Lambda.Notebook.Storage (insertIORefMap)
import Network.Wai.Handler.Warp (Port, run)
import Servant
  ( Application,
    Proxy (..),
    hoistServer,
    serve,
    type (:<|>) (..),
    type (:>),
  )

-- main api -------------------------------------------------------------------

type API =
  "v1" :> (("kernel" :> KernelAPI) :<|> "notebook" :> PersistanceAPI)

-- server----------------------------------------------------------------------

notebookApp :: Env -> Application
notebookApp s =
  serve notebookApi do
    hoistServer notebookApi (nt s) do
      kernelHandler (kernels s)
        :<|> persistanceHandler (kernels s) (notebookStorage s)
  where
    nt s' x = runReaderT (runAppT x) s'
    notebookApi = Proxy @API

notebookServer :: Port -> IO ()
notebookServer port = do
  -- bootstrap application environment
  env <- createEnvironment

  -- create some tutorial notebooks
  createTutorialNotebooks (insertIORefMap (notebookStorage env))

  -- warpppp )))) ))   )  )
  Network.Wai.Handler.Warp.run port (notebookApp env)

createEnvironment :: IO Env
createEnvironment = do
  let register = M.empty :: (Register IORef)
      notebook = M.empty :: NotebookStorage
  ioRef <- newIORef register
  ioRefNotebook <- newIORef notebook

  pure $
    Env
      { kernels = ioRef,
        notebookMaxBlocks = 10, -- XXX read from env
        notebookMaxCodeSize = 10000, -- XXX read from env
        notebookStorage = ioRefNotebook,
        maxNumberOfKernels = 10, -- XXX read from env
        notebookMaxNumber = 10
      }