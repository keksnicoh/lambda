{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Lambda.Notebook.Dependencies where

import Conduit (ConduitT)
import Control.Monad.Except (ExceptT (..))
import Data.Conduit.Internal
  ( ConduitT (ConduitT),
    Pipe (Done, HaveOutput, Leftover, NeedInput, PipeM),
  )
import Servant (ToSourceIO (..))
import qualified Servant.Types.SourceT as S

class HasM t m where
  getM :: m t

instance (Functor m, HasM a m) => HasM a (ExceptT e m) where
  getM = ExceptT (Right <$> getM)

instance ToSourceIO o (ConduitT i o IO ()) where
  toSourceIO (ConduitT con) = S.SourceT ($ go (con Done))
    where
      go (Done ()) = S.Stop
      --go (HaveOutput p o) = S.Effect $ S.Yield o (go p) <$ threadDelay 250000
      go (HaveOutput p o) = S.Effect $ pure $ S.Yield o (go p)
      go (NeedInput _ip up) = S.Skip (go (up ()))
      go (PipeM m) = S.Effect $ fmap go m
      go (Leftover p _l) = S.Skip (go p)

class HasNotebookMaxBlocks e where
  getNotebookMaxBlocks :: e -> Int

class HasNotebookMaxCodeSize e where
  getNotebookMaxCodeSize :: e -> Int