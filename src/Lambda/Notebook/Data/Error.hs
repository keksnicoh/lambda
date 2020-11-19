{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Lambda.Notebook.Data.Error where

import Control.Monad.Except (ExceptT (..), MonadError (throwError), runExceptT)
import Data.Aeson (ToJSON (..), encode)
import Data.Kind (Type)
import GHC.Generics (Generic)
import Servant (ServerError (..))

-- XXX is there a more general way using type-level programming?
withErrorHandling0 ::
  (MonadError ServerError m) =>
  (e -> m r) ->
  ExceptT e m r ->
  m r
withErrorHandling0 handler acc =
  runExceptT acc >>= getOr handler

withErrorHandling1 ::
  (MonadError ServerError m) =>
  (e -> m r) ->
  (a -> ExceptT e m r) ->
  (a -> m r)
withErrorHandling1 handler acc a = withErrorHandling0 handler (acc a)

withErrorHandling2 ::
  (MonadError ServerError m) =>
  (e -> m r) ->
  (a -> b -> ExceptT e m r) ->
  (a -> b -> m r)
withErrorHandling2 handler acc a b = withErrorHandling0 handler (acc a b)

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
errorWithMessage serverError errorCoce body =
  errorResponse serverError (Error errorCoce body)

errorWithCode :: (MonadError ServerError m) => ServerError -> String -> m r
errorWithCode serverError errorCoce =
  errorResponse serverError (Error errorCoce (Nothing :: Maybe ()))

errorResponse ::
  (MonadError ServerError m, ToJSON a) =>
  ServerError ->
  Error a ->
  m r
errorResponse serverError errorCoce =
  throwError $
    serverError
      { errHeaders = [("Content-Type", "application/json;charset=utf-8")],
        errBody = encode errorCoce
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

-- error which are not predefined by servant ----------------------------------

err423 :: ServerError
err423 =
  ServerError
    { errHTTPCode = 423,
      errReasonPhrase = "The resource that is being accessed is locked.",
      errBody = "",
      errHeaders = []
    }
