{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Lambda.Notebook.Data.Error where

import Control.Monad.Except (ExceptT (..), MonadError (throwError), runExceptT)
import Data.Aeson (ToJSON (..), encode)
import Data.Kind (Type)
import GHC.Generics (Generic)
import Lambda.Notebook.Dependencies
  ( HasM (..),
  )
import Servant (ServerError (errBody, errHeaders))

instance (Functor m, HasM a m) => HasM a (ExceptT e m) where
  getM = ExceptT (Right <$> getM)

data Error a = Error
  { error :: String,
    message :: a
  }
  deriving (Generic, ToJSON)

errorMessage :: (MonadError ServerError m, ToJSON a) => ServerError -> String -> a -> m r
errorMessage serverError error body =
  throwError $
    serverError
      { errHeaders = [("Content-Type", "application/json;charset=utf-8")],
        errBody = encode $ Error error body
      }

errorResponse :: (MonadError ServerError m) => ServerError -> String -> m r
errorResponse serverError error =
  throwError $
    serverError
      { errHeaders = [("Content-Type", "application/json;charset=utf-8")],
        errBody = encode $ Error error (Nothing :: Maybe ())
      }

-- evaluate errors using HandleError ------------------------------------------
-- XXX is there a more general way using type-level programming?

withErrorHandling0 ::
  (HandleError e, MonadError ServerError m) =>
  ExceptT e m r ->
  m r
withErrorHandling0 acc =
  runExceptT acc >>= \case
    Right res -> pure res
    Left e -> errorToResponse e

withErrorHandling1 ::
  (HandleError e, MonadError ServerError m) =>
  (a -> ExceptT e m r) ->
  (a -> m r)
withErrorHandling1 acc a =
  runExceptT (acc a) >>= \case
    Right res -> pure res
    Left e -> errorToResponse e

-- XXX generalize this
withErrorHandling2 ::
  (HandleError e, MonadError ServerError m) =>
  (a -> b -> ExceptT e m r) ->
  (a -> b -> m r)
withErrorHandling2 acc a b =
  runExceptT (acc a b) >>= \case
    Right res -> pure res
    Left e -> errorToResponse e

-- Extracting from Either / Maybe =============================================
-- XXX I'm pretty sure there is some canonical solution to this.. check it..
class HandleError e where
  errorToResponse :: MonadError ServerError m => e -> m a

class GetOr (f :: Type -> Type) (a :: Type) (m :: Type -> Type) where
  type T f a m
  getOr :: MonadError e m => T f a m -> f a -> m a

instance GetOr Maybe a m where
  type T Maybe a m = m a
  getOr e = maybe e pure

instance GetOr (Either e) a m where
  type T (Either e) a m = e -> m a
  getOr e = either e pure
