{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Lambda.Notebook.Kernel.Model
  ( Kernel (..),
    KernelStatus (..),
    UUIDContainer (..),
    Register,
    kernelEmpty,
    kernelExecute,
    kernelSucces,
    kernelRuntimeError,
    kernelRunning,
  )
where

import Data.Aeson (ToJSON)
import qualified Data.Map as M
import qualified Data.Time as T
import qualified Data.UUID as U
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
    status :: Maybe KernelStatus
  }
  deriving (Generic, ToJSON, Show)

type Register h = M.Map U.UUID (h Kernel)

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
      invoked = Nothing
    }

kernelSucces :: Scope -> Kernel -> Kernel
kernelSucces newScope kernel =
  kernel
    { scope = newScope,
      status = Just Success
    }

kernelRuntimeError :: Kernel -> Kernel
kernelRuntimeError kernel =
  kernel
    { status = Just RuntimeError
    }

kernelRunning :: T.UTCTime -> Kernel -> Kernel
kernelRunning now kernel =
  kernel
    { invoked = Just now,
      execution = execution kernel + 1,
      status = Just Running
    }