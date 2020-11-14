{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Util where

import Data.Kind ( Type )

data HList (k :: [Type]) where
  (:@:) :: k -> HList ks -> HList (k ': ks)
  HNil :: HList '[]

infixr 1 :@: