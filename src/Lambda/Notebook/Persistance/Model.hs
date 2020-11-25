{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Lambda.Notebook.Persistance.Model where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.UUID as U
import GHC.Generics (Generic)

data Block = Block
  { code :: String,
    execution :: Maybe Int
  }
  deriving (Generic, ToJSON, FromJSON)

data Notebook = Notebook
  { kernelPk :: Maybe U.UUID,
    blocks :: [Block]
  }
  deriving (Generic, ToJSON, FromJSON)

emptyNotebook :: Notebook
emptyNotebook = Notebook Nothing []