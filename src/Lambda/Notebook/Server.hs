{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Lambda.Notebook.Server where

import Control.Monad.Reader (ReaderT (runReaderT))
import Data.IORef (newIORef)
import qualified Data.Map as M
import Lambda.Notebook.App (AppT (runAppT), Env (..))
import Lambda.Notebook.Controller.Execute
  ( ExecuteAPI,
    executeHandler,
  )
import Lambda.Notebook.Controller.Kernel
  ( KernelAPI,
    kernelHandler,
  )
import Lambda.Notebook.Data.Kernel (Register)
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

type API = "v1" :> (("kernel" :> KernelAPI) :<|> ("execute" :> ExecuteAPI))

-- server----------------------------------------------------------------------

notebookApp :: Env -> Application
notebookApp s =
  serve notebookApi $
    hoistServer notebookApi (nt s) $
      kernelHandler :<|> executeHandler
  where
    nt s' x = runReaderT (runAppT x) s'
    notebookApi = Proxy @API

notebookServer :: Port -> IO ()
notebookServer port = do
  let register = M.empty :: Register
  ioRef <- newIORef register
  let env = Env ioRef
  Network.Wai.Handler.Warp.run port (notebookApp env)
