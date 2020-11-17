{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Lambda.Notebook.Data.Result where

import Data.Aeson (ToJSON (..))
import GHC.Generics (Generic)

data Result = Result
  { output :: String,
    failure :: Maybe String
  }
  deriving (Generic, ToJSON)
