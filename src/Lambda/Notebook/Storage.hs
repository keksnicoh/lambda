{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Lambda.Notebook.Storage where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import qualified Data.Map as M
import GHC.Generics (Generic)

--

data IdentifiedValue k v = IdentifiedValue
  { pk :: k,
    value :: v
  }
  deriving (Generic, ToJSON, FromJSON)

-- basic api ------------------------------------------------------------------

type LookupM k v m = k -> m (Maybe v)

type InsertM k v m = k -> v -> m ()

type CreateHandleM k v m = v -> m (k v)

type ModifyHandleM h v m = h v -> ModifyM v m

type ReadHandleM h v m = h v -> ReadM v m

type ReadM v m = m v

type ModifyM v m = (v -> v) -> m ()

-- IORef Map -------------------------------------------------------------------

type IORefMap k v = IORef (M.Map k v)

insertIORefMap :: (Ord k, MonadIO m) => IORefMap k v -> InsertM k v m
insertIORefMap ioRef k v = liftIO $ modifyIORef' ioRef (M.insert k v)

lookupIORefMap :: (Ord k, MonadIO m) => IORefMap k v -> LookupM k v m
lookupIORefMap ioRef k = M.lookup k <$> liftIO (readIORef ioRef)

-- IORef Handle ---------------------------------------------------------------

createHandleIORef :: MonadIO m => CreateHandleM IORef v m
createHandleIORef v = liftIO $ newIORef v

readHandleIORef :: MonadIO m => ReadHandleM IORef v m
readHandleIORef h = liftIO $ readIORef h

modifyHandleIORef :: MonadIO m => ModifyHandleM IORef v m
modifyHandleIORef h f = liftIO $ modifyIORef' h f
