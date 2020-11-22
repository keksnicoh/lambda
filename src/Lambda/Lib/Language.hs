{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Lambda.Lib.Language where

import Conduit (ConduitT, yield)
import Control.Applicative (Alternative ((<|>)), liftA2)
import Control.Monad (join, void)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State (MonadState, gets, modify)
import Data.Data (type (:~:) (Refl))
import Data.Functor (($>))
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Singletons (Proxy (..), Sing, SingKind (fromSing), withSomeSing)
import Data.Singletons.Decide (Decision (Disproved, Proved), SDecide (..))
import Data.Singletons.Prelude (SList (SNil))
import Data.Singletons.Sigma (Sigma (..), fstSigma)
import qualified Data.Text as T
import Lambda.Lib.Dynamic
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
import qualified Lambda.Lib.Lambda as L
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
  | Substitution Expression L.Exp L.Identifier
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

type StdOut = ConduitT () String

class MonadStdOut m where
  stdout :: String -> m ()

instance Monad m => MonadStdOut (StdOut m) where
  stdout = yield

type Scope = M.Map L.Identifier L.Exp

-- handler --------------------------------------------------------------------

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

inFreeVars :: L.Exp -> L.Identifier -> Bool
inFreeVars expr k = k `elem` L.free expr

betan :: Int -> L.Exp -> L.Exp
betan 0 expr = expr
betan n expr = betan (n - 1) $ L.β expr

applyScope ::
  (MonadState Scope m) =>
  (L.Identifier -> Bool) ->
  L.Exp ->
  m L.Exp
applyScope f expr = do
  scope <- gets (M.filterWithKey (\x _ -> f x))
  return $ go scope expr
  where
    go scope (L.Var n) =
      case M.lookup n scope of
        Just a -> a
        Nothing -> L.Var n
    go scope (L.App e1 e2) = L.App (go scope e1) (go scope e2)
    go scope (L.Lam n e) =
      L.Lam n $ go (M.filterWithKey (const . (/=) n) scope) e

hnfPrintSteps :: (Monad m, MonadStdOut m) => Int -> L.Exp -> m L.Exp
hnfPrintSteps l = hnfPrintSteps' l
  where
    hnfPrintSteps' 0 expr = do
      stdout "maximum attempts reached ..."
      pure expr
    hnfPrintSteps' n expr =
      let next = L.β expr
       in if next /= expr
            then do
              stdout $ show (l - n + 1) ++ ": " ++ show next
              hnfPrintSteps' (n - 1) next
            else pure next

-- evaluator ------------------------------------------------------------------

-- | evaluates a statement
eval ::
  (MonadState Scope m, MonadStdOut m, MonadError String m) =>
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
  stdout $ render result
  eval cont n
eval _ End = pure ()

-- | creates a runtime value from an expression
expressionΣ ::
  forall m.
  (Monad m, MonadStdOut m, MonadError String m) =>
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
  Substitution x1 expr identifier -> do
    expressionΣ handle x1 >>= \case
      (SDExp :&: e1) -> pureΣ $ L.sub identifier e1 expr
      (t :&: _) ->
        throwError $
          "type error: cannot substitute <"
            ++ show (fromSing t)
            ++ ">["
            ++ show expr
            ++ "/"
            ++ identifier
            ++ "]"
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
        Just result@(inferredType :&: _) ->
          case dtypeOpt of
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
  (Monad m, MonadState Scope m, MonadError String m) =>
  String ->
  FunctionΣ String (StdOut m)
handler =
  createHandler $
    M.fromList
      -- XXX this should look more clean
      [ ( "contract",
          wrap (pured . functionΣ L.β)
            `polymorph` wrap (pured . functionΣ betan)
        ),
        ( "hnfPrintSteps",
          wrap (kleisliΣ (Proxy @'[Int, L.Exp]) (Proxy @L.Exp) hnfPrintSteps)
        ),
        ( "free",
          wrap (pured . functionΣ L.free)
        ),
        ( "scope",
          wrap
            ( kleisliΣ
                (Proxy @'[L.Exp])
                (Proxy @L.Exp)
                (\expr -> applyScope (inFreeVars expr) expr)
            )
            `polymorph` wrap
              ( kleisliΣ
                  (Proxy @'[L.Identifier, L.Exp])
                  (Proxy @L.Exp)
                  (\identifier -> applyScope (identifier ==))
              )
            `polymorph` wrap
              ( kleisliΣ
                  (Proxy @'[[L.Identifier], L.Exp])
                  (Proxy @L.Exp)
                  (\identifiers -> applyScope (`elem` identifiers))
              )
        ),
        ( "resolve",
          wrap (kleisliΣ (Proxy @'[L.Exp]) (Proxy @L.Exp) (repeatExpM resolve))
        )
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
render ((SDList a) :&: s) =
  case s of
    [] -> renderDtype (fromSing a) ++ "{}"
    sl -> join ["{", intercalate ", " (render . (:&:) a <$> sl), "}"]

-- parser =====================================================================

parse :: String -> Either P.ParseError Statement
parse = P.parse langP ""

-- statement parsers ----------------------------------------------------------

langP :: Parser Statement
langP = stmtP <*> contP <|> eofP
  where
    stmtP = P.try assignP <|> P.try evalP
    contP = spacedP (P.oneOf ";") *> langP

evalP :: Parser (Statement -> Statement)
evalP =
  Expression <$> expressionP

assignP :: Parser (Statement -> Statement)
assignP =
  Assign
    <$> (L.identifierP <* spacedP (P.oneOf "="))
    <*> expressionP

eofP :: Parser Statement
eofP = do
  foo <- P.many P.anyChar
  if T.strip (T.pack foo) == ""
    then return End
    else P.parserFail $ "found: " ++ foo

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
      P.spaces <* wrappedCharP '}' '{' P.spaces >> cont (Just $ DList dtype)

    itemsP dtype = do
      elements <- P.chainr (pure <$> expressionP) commaP []
      P.spaces <* P.char '}'
      return $ List elements dtype

    typeP =
      P.many (P.oneOf allowedTypeChars) >>= \case
        "Int" -> pure $ Just DInt
        "Identifier" -> pure $ Just DIdentifier
        "Exp" -> pure $ Just DExp
        x -> fail $ "unknown type: " ++ x

    allowedTypeChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

functionP :: Parser Expression
functionP =
  Function
    <$> nameP
    <*> wrappedP (spacedP (P.char '[')) (spacedP (P.char ']')) argumentsP
  where
    nameP = P.many (P.oneOf validFuncChars)
    argumentsP = P.chainr1 (pure <$> expressionP) commaP
    validFuncChars =
      "abcdefghijklmnopqrstuvwxyz"
        ++ "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_"

-- | parse epxressions
-- <expression>
-- <expression>[a/b]
-- <expression>[a/b][c/d]
-- ...
expressionP :: Parser Expression
expressionP =
  foldr (\(a, b) y -> Substitution y a b) <$> (expP <* P.spaces) <*> P.try subListP
  where
    expP =
      P.try functionP
        <|> P.try listP
        <|> P.try lambdaP
        <|> P.try identifierExpP
        <|> intP

    -- [a/b], [a/b][c/d], ...
    subListP :: Parser [(L.Exp, L.Identifier)]
    subListP = P.chainl ((: []) <$> (subP <* P.spaces)) (P.spaces $> (++)) []

    -- [a/b]
    subP =
      wrappedP (P.char '[') (P.char ']') $
        spacedP $
          (,)
            <$> (L.expP <* spacedP (P.oneOf "/"))
            <*> L.identifierP

identifierExpP :: Parser Expression
identifierExpP = Identifier <$> beginChar '#' L.identifierP

-- utility parsers ------------------------------------------------------------

commaP :: Parser ([a] -> [a] -> [a])
commaP = spacedP (P.oneOf ",") >> pure (++)

wrappedCharP :: Char -> Char -> Parser a -> Parser a
wrappedCharP start end = wrappedP (P.char start) (P.char end)

wrappedP :: Parser x -> Parser y -> Parser a -> Parser a
wrappedP start end p = start *> p <* end

beginChar :: Char -> Parser a -> Parser a
beginChar start p = P.char start *> p

spacedP :: Parser a -> Parser a
spacedP = wrappedP P.spaces P.spaces

spaceP :: Parser ()
spaceP = void P.spaces
