{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Language where

import Control.Applicative (Alternative ((<|>)), liftA2)
import Control.Monad (join)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State (MonadState, gets, modify)
import Control.Monad.Writer (MonadWriter (tell))
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Singletons (Proxy (..))
import Data.Singletons.Prelude (SList (SNil))
import Data.Singletons.Sigma (Sigma (..), fstSigma)
import qualified Data.Text as T
import Dynamic
  ( DType,
    FunctionΣ,
    SDType (SDExp, SDIdentifier, SDInt, SDList),
    Signature,
    ValueΣ,
    consArgument,
    functionΣ,
    kleisliΣ,
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
  | List [Expression]
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
resolve exp = do
  state <- gets M.toList
  pure (go state exp)
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
inFreeVars exp k = const $ k `elem` L.free exp

betan :: Int -> L.Exp -> L.Exp
betan 0 exp = exp
betan n exp = betan (n - 1) $ L.β exp

scope :: MonadState Scope m => L.Exp -> m L.Exp
scope exp = do
  scope <- gets (M.filterWithKey (inFreeVars exp))
  return $ applyScope scope exp
  where
    applyScope scope (L.Var n) = case M.lookup n scope of
      Just a -> a
      Nothing -> L.Var n
    applyScope scope (L.App e1 e2) =
      L.App (applyScope scope e1) (applyScope scope e2)
    applyScope scope (L.Lam n e) =
      L.Lam n $ applyScope (M.filterWithKey (const . (/=) n) scope) e

hnfPrintSteps :: MonadWriter [String] m => Int -> L.Exp -> m L.Exp
hnfPrintSteps l = hnfPrintSteps' l
  where
    hnfPrintSteps' 0 exp = do
      tell ["maximum attamepts reached ..."]
      pure exp
    hnfPrintSteps' n exp =
      let next = L.β exp
       in if next /= exp
            then do
              tell [show (l - n + 1) ++ ": " ++ show next]
              hnfPrintSteps' (n - 1) next
            else pure next

-- evaluator ------------------------------------------------------------------

-- | evaluates a statement
eval :: (MonadState Scope m, MonadError String m, MonadWriter [String] m) => (String -> FunctionΣ String m) -> Statement -> m ()
eval handler (Assign name stmt n) = do
  expressionΣ handler stmt >>= \case
    (SDExp :&: exp) -> do
      modify (M.insert name exp)
      eval handler n
    _ -> error "can only assign lambda expressions to variables"
eval handler (Expression stmt n) = do
  result <- expressionΣ handler stmt
  tell [render result]
  eval handler n
eval _ End = pure ()

expressionΣ ::
  forall m.
  (Monad m, MonadError String m) =>
  (String -> FunctionΣ String m) ->
  (Expression -> m ValueΣ)
expressionΣ handle = \case
  Lambda c -> pureΣ c
  Number c -> pureΣ c
  Identifier c -> pureΣ c
  Function name exprL -> do
    argumentsΣ <- valuesToArgumentsΣ <$> traverse (expressionΣ handle) exprL
    case handle name argumentsΣ of
      Right v -> v
      Left err -> throwError err
  where
    valuesToArgumentsΣ = foldr consArgument (SNil :&: HNil)
    pureΣ x = pure (valueΣ x)

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
      let renderedFunc = renderFunction fname (fstSigma args)
          subline = "  " ++ suggestion
          functions = (++) "\n    * " <$> items
       in join $ [headline, "\n\n    > ", renderedFunc, "\n\n", subline, ":\n"] ++ functions ++ ["\n"]

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
renderArguments args = intercalate ", " $ show <$> args

renderFunction :: String -> [DType] -> String
renderFunction name args = name ++ "[" ++ renderArguments args ++ "]"

handler :: (Monad m, MonadState Scope m, MonadWriter [String] m) => String -> FunctionΣ String m
handler =
  createHandler $
    M.fromList
      [ ("contract", wrap (pured . functionΣ L.β) `polymorph` wrap (pured . functionΣ betan)),
        ("hnfPrintSteps", wrap (kleisliΣ (Proxy @'[Int, L.Exp]) (Proxy @L.Exp) hnfPrintSteps)),
        ("free", wrap (pured . functionΣ L.free)),
        ("scope", wrap (kleisliΣ (Proxy @'[L.Exp]) (Proxy @L.Exp) scope)),
        ("resolve", wrap (kleisliΣ (Proxy @'[L.Exp]) (Proxy @L.Exp) (repeatExpM resolve)))
      ]
  where
    pured = fmap pure
    polymorph =
      liftA2
        ( \x y -> case (x, y) of
            (Right x, _) -> Right x
            (_, Right y) -> Right y
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

lambdaP :: Parser Expression
lambdaP = Lambda <$> L.expP

intP :: Parser Expression
intP = Number . read <$> P.many1 P.digit

listP :: Parser Expression
listP = do
  P.char '{'
  content <- List <$> P.chainr (pure <$> expressionP) commaP []
  P.char '}'
  return content

functionP :: Parser Expression
functionP = do
  name <- P.many $ P.oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_"
  P.spaces
  P.char '['
  P.spaces
  donk <- P.chainr1 (pure <$> expressionP) commaP
  P.spaces
  P.char ']'
  return $ Function name donk

expressionP :: Parser Expression
expressionP =
  P.try functionP
    <|> P.try listP
    <|> P.try lambdaP
    <|> P.try identifierExpP
    <|> intP

commaP :: Parser ([Expression] -> [Expression] -> [Expression])
commaP = do
  P.spaces
  P.oneOf ","
  P.spaces >> pure (++)

identifierExpP :: Parser Expression
identifierExpP = do
  P.oneOf "\""
  name <- P.oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMOPQRSTUVWXYZβλΣσϕφ∧∨¬Φ"
  tail <- P.many P.digit
  P.oneOf "\""
  return $ Identifier (name : tail)

evalP :: Parser Statement
evalP = do
  stmt <- expressionP
  P.spaces
  P.oneOf ";"
  P.spaces
  Expression stmt <$> langP

langP :: Parser Statement
langP = do
  P.try evalP <|> P.try assignP <|> eofP

assignP :: Parser Statement
assignP = do
  name <- P.oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMOPQRSTUVWXYZβλΣσϕφ∧∨¬Φ"
  tail <- P.many P.digit
  spaceP
  P.oneOf "="
  spaceP
  stmt <- expressionP
  P.spaces
  P.oneOf ";"
  P.spaces

  Assign (name : tail) stmt <$> langP

spaceP :: Parser String
spaceP = P.many $ P.oneOf " "

eofP = do
  foo <- P.many P.anyChar
  if T.strip (T.pack foo) == ""
    then return End
    else P.parserFail $ "derp: " ++ foo
