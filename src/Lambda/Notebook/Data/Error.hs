{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Lambda.Notebook.Data.Error where

import Control.Monad.Except (ExceptT (..), MonadError (throwError), runExceptT)
import Data.Aeson (ToJSON (..), encode)
import Data.Kind (Type)
import GHC.Generics (Generic)
import Lambda.Notebook.Dependencies ( HasM(..) )
import Servant (ServerError (errBody, errHeaders))

-- notebook framework api -----------------------------------------------------

instance (Functor m, HasM a m) => HasM a (ExceptT e m) where
  getM = ExceptT (Right <$> getM)

-- implicit error handling ----------------------------------------------------

class HandleError e where
  errorToResponse :: MonadError ServerError m => e -> m a

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
withErrorHandling1 acc a = withErrorHandling0 (acc a)

withErrorHandling2 ::
  (HandleError e, MonadError ServerError m) =>
  (a -> b -> ExceptT e m r) ->
  (a -> b -> m r)
withErrorHandling2 acc a b = withErrorHandling0 (acc a b)

-- servant error handling helpers ---------------------------------------------

data Error a = Error
  { code :: String,
    message :: a
  }
  deriving (Generic, ToJSON)

errorWithMessage ::
  (MonadError ServerError m, ToJSON a) =>
  ServerError ->
  String ->
  a ->
  m r
errorWithMessage serverError error body =
  errorResponse serverError (Error error body)

errorWithCode :: (MonadError ServerError m) => ServerError -> String -> m r
errorWithCode serverError error =
  errorResponse serverError (Error error (Nothing :: Maybe ()))

errorResponse ::
  (MonadError ServerError m, ToJSON a) =>
  ServerError ->
  Error a ->
  m r
errorResponse serverError error =
  throwError $
    serverError
      { errHeaders = [("Content-Type", "application/json;charset=utf-8")],
        errBody = encode error
      }

-- MonadError based extraction of values --------------------------------------
-- XXX I'm pretty sure there is some canonical solution to this.. check it..

class GetOr (f :: Type -> Type) (a :: Type) (m :: Type -> Type) where
  type T f a m
  getOr :: MonadError e m => T f a m -> f a -> m a

instance GetOr Maybe a m where
  type T Maybe a m = m a
  getOr e = maybe e pure

instance GetOr (Either e) a m where
  type T (Either e) a m = e -> m a
  getOr e = either e pure
