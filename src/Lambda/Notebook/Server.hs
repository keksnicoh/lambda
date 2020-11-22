{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Lambda.Notebook.Server where

import Control.Monad.Reader (ReaderT (runReaderT))
import Data.IORef (newIORef)
import qualified Data.Map as M
import Lambda.Notebook.App (AppT (runAppT), Env (..))
import Lambda.Notebook.Kernel.API (KernelAPI, kernelHandler)
import Lambda.Notebook.Kernel.Model (Register)
import Network.Wai.Handler.Warp (Port, run)
import Servant
  ( Application,
    Proxy (..),
    hoistServer,
    serve,
    type (:>),
  )

-- main api -------------------------------------------------------------------

type API = "v1" :> ("kernel" :> KernelAPI)

-- server----------------------------------------------------------------------

notebookApp :: Env -> Application
notebookApp s =
  serve notebookApi $
    hoistServer notebookApi (nt s) $ kernelHandler
  where
    nt s' x = runReaderT (runAppT x) s'
    notebookApi = Proxy @API

notebookServer :: Port -> IO ()
notebookServer port = do
  let register = M.empty :: Register
  ioRef <- newIORef register
  Network.Wai.Handler.Warp.run port (notebookApp (Env ioRef))
