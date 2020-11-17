{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Lambda.Notebook.Server where

import Control.Monad.Reader (ReaderT (runReaderT))
import Data.IORef (newIORef)
import qualified Data.Map as M
import qualified Data.UUID as U
import Lambda.Notebook.App (AppT (runAppT), Env (..))
import Lambda.Notebook.Data.Error (withErrorHandling0, withErrorHandling1, withErrorHandling2)
import Lambda.Notebook.Data.Kernel (Kernel, Register)
import Lambda.Notebook.Data.Result (Result)
import Lambda.Notebook.Service.KernelService
import Lambda.Notebook.Service.RunService (runService)
import Network.Wai.Handler.Warp (Port, run)
import Servant
  ( Application,
    Capture,
    Get,
    JSON,
    PlainText,
    Post,
    Proxy (..),
    Put,
    ReqBody,
    hoistServer,
    serve,
    type (:<|>) (..),
    type (:>),
  )

type CreateKernelEndpoint = Put '[JSON] U.UUID

type KernelStatusEndpoint = Capture "uuid" U.UUID :> Get '[JSON] Kernel

type ExecuteStatement =
  Capture "uuid" U.UUID
    :> ReqBody '[PlainText] String
    :> Post '[JSON] Result

type API =
  "v1" :> (("kernel" :> (CreateKernelEndpoint :<|> KernelStatusEndpoint)) :<|> ("exec" :> ExecuteStatement))

notebookApp :: Env -> Application
notebookApp s =
  serve notebookApi $
    hoistServer notebookApi (nt s) $
      (withErrorHandling0 createKernel :<|> withErrorHandling1 kernelStatus)
        :<|> withErrorHandling2 runService
  where
    nt s' x = runReaderT (runAppT x) s'
    notebookApi = Proxy @API

notebookServer :: Port -> IO ()
notebookServer port = do
  let register = M.empty :: Register
  ioRef <- newIORef register
  let env = Env ioRef "foo"
  Network.Wai.Handler.Warp.run port (notebookApp env)
