{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Lambda.Notebook.Persistance.Model where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data Block = Block
  { code :: String,
    execution :: Int
  }
  deriving (Generic, ToJSON, FromJSON)

newtype Notebook = Notebook
  { blocks :: [Block]
  }
  deriving (Generic, ToJSON, FromJSON)

emptyNotebook :: Notebook
emptyNotebook = Notebook []