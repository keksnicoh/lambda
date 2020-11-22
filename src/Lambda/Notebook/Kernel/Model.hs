{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Lambda.Notebook.Kernel.Model
  ( Kernel (..),
    KernelStatus (..),
    UUIDContainer (..),
    Register,
    kernelEmpty,
    kernelExecute
  )
where

import Data.Aeson (ToJSON)
import Data.IORef (IORef)
import qualified Data.Map as M
import qualified Data.Time as T
import qualified Data.UUID as U
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Lambda.Lib.Language (Scope)

-- models ---------------------------------------------------------------------

data UUIDContainer a = UUIDContainer {uuid :: U.UUID, value :: a}
  deriving (Generic, ToJSON, Show)

data KernelStatus = Running | Success | RuntimeError
  deriving (Generic, ToJSON, Eq, Show)

data Kernel = Kernel
  { execution :: Int,
    scope :: Scope,
    created :: T.UTCTime,
    invoked :: Maybe T.UTCTime,
    status :: Maybe KernelStatus,
    blocks :: V.Vector String
  }
  deriving (Generic, ToJSON, Show)

type Register = M.Map U.UUID (IORef Kernel)

-- mapper ---------------------------------------------------------------------

kernelExecute :: Kernel -> T.UTCTime -> Kernel
kernelExecute kernel now =
  kernel
    { invoked = Just now,
      execution = execution kernel + 1,
      status = Just Running
    }

kernelEmpty :: T.UTCTime -> Kernel
kernelEmpty now =
  Kernel
    { execution = 0,
      scope = M.empty,
      created = now,
      status = Nothing,
      invoked = Nothing,
      blocks = V.replicate 100 ""
    }
