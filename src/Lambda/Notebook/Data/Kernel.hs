{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Lambda.Notebook.Data.Kernel where

import Control.Monad.State (MonadState, modify)
import Data.Aeson (ToJSON (toJSON))
import qualified Data.Map as M
import qualified Data.Time as T
import qualified Data.UUID as U
import GHC.Generics (Generic)
import Lambda.Lib.Lambda (Exp)
import Lambda.Lib.Language (Scope)
import Lambda.Notebook.Dependencies (HasM, getM)

data Kernel = Kernel
  { execution :: Int,
    scope :: Scope,
    created :: T.UTCTime,
    invoked :: Maybe T.UTCTime
  }
  deriving (Generic, ToJSON)

instance ToJSON Exp where
  toJSON p = toJSON $ show p

type Register = M.Map U.UUID Kernel

-- update kernel --------------------------------------------------------------

updateKernelInvoked :: (HasM T.UTCTime m, MonadState Register m) => U.UUID -> Kernel -> m ()
updateKernelInvoked uuid kernel = do
  time <- getM @T.UTCTime
  modify $
    M.insert uuid $
      kernel
        { invoked = Just time,
          execution = 1 + execution kernel
        }
