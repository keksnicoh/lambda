{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Dynamic where

import Data.Data (type (:~:) (Refl))
import Data.Kind (Type)
import Data.Singletons.Decide (Decision (Disproved, Proved), SDecide (..))
import Data.Singletons.Prelude
  ( Map,
    Proxy,
    SList (SCons),
    SingI (..),
    SingKind (fromSing),
  )
import Data.Singletons.Sigma (Sigma (..))
import Data.Singletons.TH (genDefunSymbols, genSingletons)
import qualified Lambda as L
import Util (HList (..))

-- Dynamic Runtime Type -------------------------------------------------------

-- |  possible dynamic runtime types
data DType
  = DInt
  | DIdentifier
  | DExp
  | DList DType
  deriving (Show, Eq)

$(genSingletons [''DType])

-- | identifies haskell type with dynamic runtime type
type family TypeX (k :: DType) where
  TypeX 'DInt = Int
  TypeX 'DIdentifier = L.Identifier
  TypeX 'DExp = L.Exp
  TypeX ( 'DList a) = [TypeX a]

$(genDefunSymbols [''TypeX])

-- |  identifies dynamic runtime type with haskell type
type family DTypeX (k :: Type) where
  DTypeX Int = 'DInt
  DTypeX L.Identifier = 'DIdentifier
  DTypeX L.Exp = 'DExp
  DTypeX [a] = 'DList (DTypeX a)

$(genDefunSymbols [''DTypeX])

type TypeIso a = TypeX (DTypeX a)

type MappedTypeIso a = Map TypeXSym0 (Map DTypeXSym0 a)

instance SDecide DType where
  SDInt %~ SDInt = Proved Refl
  SDIdentifier %~ SDIdentifier = Proved Refl
  SDExp %~ SDExp = Proved Refl
  SDList a %~ SDList b = case a %~ b of
    Proved Refl -> Proved Refl
    Disproved _ -> Disproved $ const undefined
  _ %~ _ = Disproved $ const undefined

type ValueΣ = Sigma DType TypeXSym0

-- |  lifts a value into corresponding valueΣ
valueΣ :: forall a. (SingI (DTypeX a), a ~ TypeIso a) => a -> ValueΣ
valueΣ = (:&:) (sing @(DTypeX a))

-- XXX find a better API to deal with lists
pureListΣ :: ValueΣ -> ValueΣ
pureListΣ (a :&: b) = SDList a :&: [b]

toListΣ :: ValueΣ -> [ValueΣ] -> Maybe ValueΣ
toListΣ a [] = Just a
toListΣ (ast :&: as) ((at :&: a) : cs) =
  case ast %~ SDList at of
    Proved Refl -> toListΣ (ast :&: (a : as)) cs
    Disproved _ -> Nothing

type DTypeHList (ts :: [DType]) = HList (Map TypeXSym0 ts)

$(genDefunSymbols [''DTypeHList])

type ArgumentsΣ = Sigma [DType] DTypeHListSym0

-- |  cons valueΣ to argumentsΣ
consArgument :: ValueΣ -> ArgumentsΣ -> ArgumentsΣ
consArgument (vt :&: v) (at :&: a) = SCons vt at :&: (v :@: a)

-- Dynamic Function -----------------------------------------------------------

type family Function (k :: [Type]) (r :: Type) where
  Function '[] r = r
  Function (a ': as) r = a -> Function as r

type family FuncArguments (k :: Type) where
  FuncArguments (a -> b) = a ': FuncArguments b
  FuncArguments _ = '[]

type family FuncReturn (k :: Type) where
  FuncReturn (_ -> b) = FuncReturn b
  FuncReturn e = e

type FunctionΣ e m = ArgumentsΣ -> Either e (m ValueΣ)

-- |  applies a HList over function iff function signature matches hlist kind
--
--   ## Examples
--
--       > functionHList (\x y -> show x ++ show y) (123 :@: True :@: HNil)
--       > "123True"
functionHList :: forall (ts :: [Type]) (r :: Type). Function ts r -> HList ts -> r
functionHList f HNil = f
functionHList f (a :@: l) = functionHList (f a) l

type Signature = ([DType], DType)

functionΣ ::
  forall a.
  ( SingI (Map DTypeXSym0 (FuncArguments a)),
    SingI (DTypeX (FuncReturn a)),
    Function (MappedTypeIso (FuncArguments a)) (TypeIso (FuncReturn a)) ~ a
  ) =>
  a ->
  (ArgumentsΣ -> Either Signature ValueΣ)
functionΣ f (a :&: args) =
  case a %~ argS of
    Proved Refl ->
      let apply = Right . (:&:) retS . functionHList f
       in apply args
    Disproved _ -> Left (fromSing argS, fromSing retS)
  where
    argS = sing @(Map DTypeXSym0 (FuncArguments a))
    retS = sing @(DTypeX (FuncReturn a))

-- XXX try to remove the proxies:
--     was not easy to reflect ts by the given function as it was done in
--     functionΣ. how to translate (a -> b -> m c) into  [a, b], m, c
--     without ambiguous types?
kleisliΣ ::
  forall m ts r.
  ( Functor m,
    SingI (Map DTypeXSym0 ts),
    SingI (DTypeX r),
    ts ~ MappedTypeIso ts,
    r ~ TypeIso r
  ) =>
  Proxy ts ->
  Proxy r ->
  Function ts (m r) ->
  (ArgumentsΣ -> Either Signature (m ValueΣ))
kleisliΣ _ _ f (a :&: b) = case a %~ sing @(Map DTypeXSym0 ts) of
  Proved Refl -> Right $ (:&:) (sing @(DTypeX r)) <$> functionHList f b
  Disproved _ -> Left (fromSing argS, fromSing retS)
  where
    argS = sing @(Map DTypeXSym0 ts)
    retS = sing @(DTypeX r)
