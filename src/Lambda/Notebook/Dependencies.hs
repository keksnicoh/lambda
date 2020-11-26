{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Lambda.Notebook.Dependencies where

import Conduit (ConduitT)
import Control.Monad.Except (ExceptT (..))
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Data.Conduit.Internal
  ( ConduitT (ConduitT),
    Pipe (Done, HaveOutput, Leftover, NeedInput, PipeM),
  )
import qualified Data.UUID as U
import qualified Data.UUID.V4 as U
import Servant (ToSourceIO (..))
import qualified Servant.Types.SourceT as S

--import Control.Concurrent

class HasM t m where
  getM :: m t

instance (Functor m, HasM a m) => HasM a (ExceptT e m) where
  getM = ExceptT (Right <$> getM)

instance HasM U.UUID IO where
  getM = liftIO U.nextRandom

instance ToSourceIO o (ConduitT i o IO ()) where
  toSourceIO (ConduitT con) = S.SourceT ($ go (con Done))
    where
      go (Done ()) = S.Stop
      --go (HaveOutput p o) = S.Effect $ S.Yield o (go p) <$ threadDelay 250000
      go (HaveOutput p o) = S.Effect $ pure $ S.Yield o (go p)
      go (NeedInput _ip up) = S.Skip (go (up ()))
      go (PipeM m) = S.Effect $ fmap go m
      go (Leftover p _l) = S.Skip (go p)
