module Lambda.Notebook.Storage where

import Control.Monad.IO.Class (MonadIO (..))
import Data.IORef (IORef, modifyIORef', readIORef)
import qualified Data.Map as M

-- basic api ------------------------------------------------------------------

type LookupM k v m = k -> m (Maybe v)

type InsertM k v m = k -> v -> m ()

type ModifyHandleM h v m = h v -> (v -> v) -> m ()

type ReadHandleM h v m = h v -> m v

-- IORef Map -------------------------------------------------------------------

type IORefMap k v = IORef (M.Map k v)

inserIORefMap :: (Ord k, MonadIO m) => IORefMap k v -> k -> v -> m ()
inserIORefMap ioRef k v = liftIO $ modifyIORef' ioRef (M.insert k v)

lookupIORefMap :: (Ord k, MonadIO m) => IORefMap k v -> k -> m (Maybe v)
lookupIORefMap ioRef k = M.lookup k <$> liftIO (readIORef ioRef)

-- IORef Handle ---------------------------------------------------------------

readHandleIORef :: MonadIO m => ReadHandleM IORef v m
readHandleIORef h = liftIO $ readIORef h

modifyHandleIORef :: MonadIO m => ModifyHandleM IORef v m
modifyHandleIORef h f = liftIO $ modifyIORef' h f
