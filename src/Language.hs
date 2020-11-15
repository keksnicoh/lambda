{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Language where

import Control.Applicative (Alternative ((<|>)), liftA2)
import Control.Monad (join)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State (MonadState, gets, modify)
import Control.Monad.Writer (MonadWriter (tell))
import Data.Data (type (:~:) (Refl))
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Singletons (Proxy (..), Sing, SingKind (fromSing), withSomeSing)
import Data.Singletons.Decide (Decision (Disproved, Proved), SDecide (..))
import Data.Singletons.Prelude (SList (SNil))
import Data.Singletons.Sigma (Sigma (..), fstSigma)
import qualified Data.Text as T
import Dynamic
  ( DType (..),
    FunctionΣ,
    SDType (SDExp, SDIdentifier, SDInt, SDList),
    Signature,
    TypeX,
    ValueΣ,
    consArgument,
    functionΣ,
    kleisliΣ,
    pureListΣ,
    toListΣ,
    valueΣ,
  )
import qualified Lambda as L
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)
import Util (HList (HNil))

-- Language AST ===============================================================

data Expression
  = Function String [Expression]
  | Number Int
  | List [Expression] (Maybe DType)
  | Lambda L.Exp
  | Identifier L.Identifier
  deriving (Show, Eq)

data Statement
  = Expression Expression Statement
  | Assign String Expression Statement
  | End
  deriving (Show, Eq)

-- haskell API ----------------------------------------------------------------

instance Semigroup Statement where
  (Expression a l1) <> l2 = Expression a (l1 <> l2)
  (Assign a b l1) <> l2 = Assign a b (l1 <> l2)
  End <> l2 = l2

instance Monoid Statement where
  mempty = End

-- Runtime ====================================================================

type Scope = M.Map L.Identifier L.Exp

resolve :: MonadState Scope m => L.Exp -> m L.Exp
resolve expr = do
  state <- gets M.toList
  pure (go state expr)
  where
    go [] e = e
    go ((k, v) : ss) e = go ss $ L.sub k e v

repeatExpM :: (Monad m, Eq a) => (a -> m a) -> a -> m a
repeatExpM f a = do
  next <- f a
  if next == a
    then return a
    else repeatExpM f next

inFreeVars :: L.Exp -> L.Identifier -> b -> Bool
inFreeVars expr k = const $ k `elem` L.free expr

betan :: Int -> L.Exp -> L.Exp
betan 0 expr = expr
betan n expr = betan (n - 1) $ L.β expr

applyScope :: MonadState Scope m => L.Exp -> m L.Exp
applyScope expr = do
  scope <- gets (M.filterWithKey (inFreeVars expr))
  return $ applyScope' scope expr
  where
    applyScope' scope (L.Var n) = case M.lookup n scope of
      Just a -> a
      Nothing -> L.Var n
    applyScope' scope (L.App e1 e2) =
      L.App (applyScope' scope e1) (applyScope' scope e2)
    applyScope' scope (L.Lam n e) =
      L.Lam n $ applyScope' (M.filterWithKey (const . (/=) n) scope) e

hnfPrintSteps :: MonadWriter [String] m => Int -> L.Exp -> m L.Exp
hnfPrintSteps l = hnfPrintSteps' l
  where
    hnfPrintSteps' 0 expr = do
      tell ["maximum attamepts reached ..."]
      pure expr
    hnfPrintSteps' n expr =
      let next = L.β expr
       in if next /= expr
            then do
              tell [show (l - n + 1) ++ ": " ++ show next]
              hnfPrintSteps' (n - 1) next
            else pure next

-- evaluator ------------------------------------------------------------------

-- | evaluates a statement
eval ::
  (MonadState Scope m, MonadError String m, MonadWriter [String] m) =>
  (String -> FunctionΣ String m) ->
  Statement ->
  m ()
eval cont (Assign name stmt n) = do
  expressionΣ cont stmt >>= \case
    (SDExp :&: expr) -> do
      modify (M.insert name expr)
      eval cont n
    _ -> error "can only assign lambda expressions to variables"
eval cont (Expression stmt n) = do
  result <- expressionΣ cont stmt
  tell [render result]
  eval cont n
eval _ End = pure ()

-- | creates a runtime value from an expression
expressionΣ ::
  forall m.
  (Monad m, MonadError String m) =>
  (String -> FunctionΣ String m) ->
  (Expression -> m ValueΣ)
expressionΣ handle = \case
  Lambda c -> pureΣ c
  Number c -> pureΣ c
  Identifier c -> pureΣ c
  List items dtypeOpt -> expressionListΣ (expressionΣ handle) items dtypeOpt
  Function name exprL -> do
    argumentsΣ <- valuesToArgumentsΣ <$> traverse (expressionΣ handle) exprL
    case handle name argumentsΣ of
      Right v -> v
      Left err -> throwError err
  where
    valuesToArgumentsΣ = foldr consArgument (SNil :&: HNil)
    pureΣ x = pure (valueΣ x)

-- | creates a runtime value from a list of expressions
expressionListΣ ::
  MonadError String m =>
  (Expression -> m ValueΣ) ->
  [Expression] ->
  Maybe DType ->
  m ValueΣ
expressionListΣ cont = \case
  [] -> \case
    -- empty list must have type specified, otherwise the "ad-hoc polymorphism"
    -- of the handlers could lead to undecidable situations.
    Just dtype ->
      pure $
        withSomeSing dtype $ \(dtypeSing :: Sing dtype) ->
          SDList dtypeSing :&: ([] :: [TypeX dtype])
    Nothing -> throwError errorEmptyList
  (c : cs) ->
    \dtypeOpt -> do
      first <- cont c
      others <- traverse cont cs
      case toListΣ (pureListΣ first) others of
        Just result@(inferredType :&: _) -> case dtypeOpt of
          Nothing -> pure result
          -- if type was specified and non-empty list was evaluated, then we
          -- will prove that inferred type matches specified type
          Just dtype ->
            withSomeSing dtype $ \dtypeSing ->
              case inferredType %~ SDList dtypeSing of
                Proved Refl -> pure result
                Disproved _ ->
                  throwError $ errorInferredTypeDiffers dtype inferredType
        Nothing -> throwError $ errorInhomogeneousList (first : others)
  where
    errorInhomogeneousList lst =
      join
        [ "list contains inhomogeneous items\n\n",
          "    " ++ show ((\(a :&: _) -> renderDtype (fromSing a)) <$> lst)
        ]
    errorInferredTypeDiffers a lt =
      join
        [ "list contains items of different type than specified.\n\n",
          "    reason: type " ++ renderDtype a ++ " is specified ",
          "but item type inferres to " ++ renderDtype (fromSing lt) ++ " \n"
        ]
    errorEmptyList =
      join
        [ "ambigious type due to construction of empty list {}.\n\n",
          "    hint: specify list type (only required for empty lists).\n\n",
          "    here are some examples:\n\n",
          "      * Exp{}        - empty list of lambda expresssions\n",
          "      * Int{}        - empty list of integer\n",
          "      * Identifier{} - empty list of identifiers\n",
          "      * Int{}{}      - empty list of list of integers\n",
          "      * ...\n"
        ]

renderDtype :: DType -> String
renderDtype DInt = "Int"
renderDtype DIdentifier = "Identifier"
renderDtype DExp = "Exp"
renderDtype (DList a) = renderDtype a ++ "{}"

-- handler --------------------------------------------------------------------

createHandler ::
  M.Map String (FunctionΣ [Signature] m) ->
  (String -> FunctionΣ String m)
createHandler register fname args =
  case M.lookup fname register of
    Just f -> case f args of
      Right r -> Right r
      Left err -> Left (adHocErr err)
    Nothing -> Left functionErr
  where
    errorBody headline suggestion items =
      let func = renderFunction fname (fstSigma args)
          subline = "  " ++ suggestion
          functions = (++) "\n    * " <$> items
          parts = [headline, "\n\n    > ", func, "\n\n", subline, ":\n"]
       in join $ parts ++ functions ++ ["\n"]

    functionErr =
      errorBody
        "invocation of undefined function."
        "The following functions are available"
        (M.keys register)

    adHocErr err =
      errorBody
        "invocation of undefined ad-hoc polymorphic variant."
        "The following variants exist"
        (renderFunction fname . fst <$> err)

renderArguments :: [DType] -> String
renderArguments args = intercalate ", " $ renderDtype <$> args

renderFunction :: String -> [DType] -> String
renderFunction name args = name ++ "[" ++ renderArguments args ++ "]"

handler ::
  (Monad m, MonadState Scope m, MonadWriter [String] m) =>
  String ->
  FunctionΣ String m
handler =
  createHandler $
    M.fromList
      -- XXX this should look more clean
      [ ("contract", wrap (pured . functionΣ L.β) `polymorph` wrap (pured . functionΣ betan)),
        ("hnfPrintSteps", wrap (kleisliΣ (Proxy @'[Int, L.Exp]) (Proxy @L.Exp) hnfPrintSteps)),
        ("free", wrap (pured . functionΣ L.free)),
        ("scope", wrap (kleisliΣ (Proxy @'[L.Exp]) (Proxy @L.Exp) applyScope)),
        ("resolve", wrap (kleisliΣ (Proxy @'[L.Exp]) (Proxy @L.Exp) (repeatExpM resolve)))
      ]
  where
    pured = fmap pure
    polymorph =
      liftA2
        ( \x y -> case (x, y) of -- XXX omg, arghh make this nice plz...
            (Right v, _) -> Right v
            (_, Right w) -> Right w
            (Left e1, Left e2) -> Left (e1 ++ e2)
        )
    wrap f x = case f x of
      Right v -> Right v
      Left e -> Left [e]

render :: ValueΣ -> String
render (SDIdentifier :&: s) = s
render (SDInt :&: s) = show s
render (SDExp :&: s) = show s
render ((SDList a) :&: s) = show $ render . (:&:) a <$> s

-- parser =====================================================================

parse :: String -> Either P.ParseError Statement
parse = P.parse langP ""

-- statement parsers ----------------------------------------------------------

langP :: Parser Statement
langP = P.try evalP <|> P.try assignP <|> eofP

evalP :: Parser Statement
evalP = do
  stmt <- expressionP
  spacedP (P.oneOf ";")
  Expression stmt <$> langP

assignP :: Parser Statement
assignP = do
  name <- L.identifierP
  spacedP (P.oneOf "=")
  stmt <- expressionP
  spacedP (P.oneOf ";")
  Assign name stmt <$> langP

eofP :: Parser Statement
eofP = do
  foo <- P.many P.anyChar
  if T.strip (T.pack foo) == ""
    then return End
    else P.parserFail $ "derp: " ++ foo

-- expression parsers ---------------------------------------------------------

lambdaP :: Parser Expression
lambdaP = Lambda <$> L.expP

intP :: Parser Expression
intP = Number . read <$> P.many1 P.digit

-- | parse list expressions
--
-- ## h2 examples
--
-- - list of lambda expressions: {λx.x, abc}
-- - list with explict type: Int{1, 2}
listP :: Parser Expression
listP = do
  tname <- P.option Nothing (P.try typeP)
  P.spaces <* P.char '{'
  cont tname
  where
    -- specific dtype might nested or followed by items
    cont (Just dtype) = P.try (nestedP dtype) <|> itemsP (Just dtype)
    -- has no specific type, only items
    cont Nothing = itemsP Nothing

    nestedP dtype =
      P.spaces <* wrappedChar '}' '{' P.spaces >> cont (Just $ DList dtype)

    itemsP dtype = do
      elements <- P.chainr (pure <$> expressionP) commaP []
      P.spaces <* P.char '}'
      return $ List elements dtype

    typeP =
      P.many (P.oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") >>= \case
        "Int" -> pure $ Just DInt
        "Identifier" -> pure $ Just DIdentifier
        "Exp" -> pure $ Just DExp
        x -> fail $ "unknown type: " ++ x

functionP :: Parser Expression
functionP =
  Function
    <$> funcNameP
    <*> wrapped (spacedP (P.char '[')) (spacedP (P.char ']')) funcArgumentListP
  where
    funcNameP = P.many (P.oneOf validFuncChars)
    funcArgumentListP = P.chainr1 (pure <$> expressionP) commaP
    validFuncChars =
      "abcdefghijklmnopqrstuvwxyz"
        ++ "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_"

expressionP :: Parser Expression
expressionP =
  P.try functionP
    <|> P.try listP
    <|> P.try lambdaP
    <|> P.try identifierExpP
    <|> intP

identifierExpP :: Parser Expression
identifierExpP = Identifier <$> wrappedChar '"' '"' L.identifierP

-- utility parsers ------------------------------------------------------------

commaP :: Parser ([a] -> [a] -> [a])
commaP = do
  spacedP (P.oneOf ",") >> pure (++)

wrappedChar :: Char -> Char -> Parser a -> Parser a
wrappedChar start end = wrapped (P.char start) (P.char end)

wrapped :: Parser x -> Parser y -> Parser a -> Parser a
wrapped start end p = start *> p <* end

spacedP :: Parser a -> Parser ()
spacedP p = P.spaces *> p *> P.spaces

spaceP :: Parser String
spaceP = P.many $ P.oneOf " "
