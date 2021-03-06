{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Lambda.Lib.Lambda
  ( β,
    beta,
    sub,
    Exp (..),
    Identifier,
    (@:),
    (>:),
    free,
    hnf,
    alpha,
    parse,
    expP,
    identifierP,
    bound,
    equal,
  )
where

import Control.Applicative (many, (<|>))
import Data.Aeson (ToJSON (toJSON))
import Data.List (find, intersect, nub, sort)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.String (IsString (..))
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)

-- ADT representing lambda expressions ----------------------------------------

{-
<expression> := <identifier> | <lambda abstraction> | <application>
<lambda abstraction> := λ <identifier>.<expression>
<application> := <expression><expression>
<identifier> = String
-}

type Identifier = String

data Exp
  = Var Identifier
  | Lam Identifier Exp
  | App Exp Exp

(@:) :: Exp -> Exp -> Exp
(@:) = App

infixl 9 @:

(>:) :: Identifier -> Exp -> Exp
(>:) = Lam

infixr 8 >:

-- haskell API ----------------------------------------------------------------

instance Show Exp where
  show (Var n) = case n of
    [c] -> [c]
    s -> "_" ++ s ++ " "
  show (Lam n e) = "λ" ++ go n e
    where
      go sn (Lam m se) = sn ++ go m se
      go sn se = sn ++ "." ++ show se
  show (App (Var n1) (Var n2)) = renderVar n1 ++ renderVar n2
  show (App (Var n1) l) = renderVar n1 ++ "(" ++ show l ++ ")"
  show (App l1@(Lam _ _) (Var n1)) = "(" ++ show l1 ++ ")" ++ renderVar n1
  show (App l1@(Lam _ _) e2) = "(" ++ show l1 ++ ")" ++ "(" ++ show e2 ++ ")"
  show (App e1 l2@(Lam _ _)) = show e1 ++ "(" ++ show l2 ++ ")"
  show (App x y) = show x ++ show y

renderVar :: String -> String
renderVar [c] = [c]
renderVar s = "_" ++ s ++ " "

instance IsString Exp where
  fromString = Var

instance ToJSON Exp where
  toJSON p = toJSON $ show p

instance Eq Exp where
  (==) = equal

-- operations and objects from lambda calculus --------------------------------

-- | single beta-reduction step (normal order reduction)
--
--  ## Random Example (without α-conversion)
--
--    >    β.β.β (λx.(λy.y)xux)(λu.u(λv.u))
--    > -> β.β   (λy.y)(λu.u(λv.u))u(λu.u(λv.u))
--    > -> β     (λu.u(λv.u))u(λu.u(λv.u))
--    > ->       u(λv.u)(λu.u(λv.u))
--
--  ## Head Normal Form
--
--    > β (λx.xx)(λx.xx) -> (λx.xx)(λx.xx)
--
--  ## Normal Order Reduction
--
--    > β (λx.z)((λw.www)(λw.www)) == z
β :: Exp -> Exp
β (Var x) = Var x -- β[x] -> x
β (Lam x t) = Lam x (β t) -- β[λx.t] = λx.β[t]
β (App (Lam x t) u) = sub x t u -- β[(λx.t)u] = t[x <- u]
β (App f g) = App (β f) (β g) -- β[fg] = β[f]β[g]

beta :: Exp -> Exp
beta = β

hnf :: Exp -> Exp
hnf e1 = case β e1 of
  e2
    | e1 == e2 -> e1
    | otherwise -> hnf e2

-- | substitution: f[x <- T]
--
-- ## Examples
--
--    > sub "x" ("x" @: "y") "u"               == "u" @: "y"
--    > sub "y" ("x" @: "y") "u"               == "x" @: "u"
--    > sub "z" ("x" @: "y") "u"               == "x" @: "y"
--    > sub "x" ("u" >: "x" @: "y" @: "u") "u" == "z" >: "u" @: "y" @: "z"
sub :: Identifier -> Exp -> Exp -> Exp
sub x (Var y) s
  | x == y = s -- x[x <- s] = s
  | otherwise = Var y -- y[x <- s] = y
sub x l@(Lam y e1) s
  -- (λx.e1)[x <- s] = λx.e1
  | x == y = Lam y e1
  -- (λy.e1)[x <- s] = λy.(e1[x <- s])
  --    IF FV[e1] disjunct from B[λy.e1] (freshness condition)
  --      or x is not a free variable of the term e1
  | x /= y && freshness || x `notElem` freeTerm = Lam y $ sub x e1 s
  -- we use α-conversion to replace function variable @y@ by @rn@
  | otherwise =
    let rn = fromMaybe tabulated prefered
     in Lam rn $ sub x (alpha y (Var rn) e1) s
  where
    freshness = null (free s `intersect` bound l)
    freeTerm = free e1 ++ bound l
    notFreeVar = flip notElem freeTerm
    prefered = find notFreeVar ["x", "y", "z", "s", "u", "v", "w"]
    tabulated = findInf (0 :: Int)
    findInf i =
      let n = y ++ show i
       in if notFreeVar n then n else findInf (i + 1)
-- (fg)[x <- s] = (f[x <- s])(g[x <- s])
sub x (App e1 e2) s = App (sub x e1 s) (sub x e2 s)

-- | returns an alphanumerically sorted list of unique identifiers from an
--   expression which **are not bound** to a lambda.
--
-- ## Examples
--
--    > free "a" == ["a"]
--    > free ("a" >: "a") = []
--    > free (("a" >: "a" @: ("b" >: "b" @: "e")) @: "c") == ["c","e"]
free :: Exp -> [Identifier]
free = sort . nub . go
  where
    go = \case
      Var x -> [x] -- FV[x] = {}
      App e1 e2 -> go e1 ++ go e2 -- FV[e1e2] = FV[e1] U FV[e2]
      Lam x e -> filter (not . (==) x) (go e) -- FV[λx.t] = FV[t] \ {x}

-- | returns an alphanumerically sorted list of unique identifiers from an
--   expression which **are bound** to a lambda.
--
-- ## Examples
--
--    > bound "a" == []
--    > bound ("a" >: "a") == ["a"]
--    > bound (("a" >: "a" @: ("b" >: "b" @: "e")) @: "c") == ["a","b"]
bound :: Exp -> [Identifier]
bound = sort . nub . go
  where
    go = \case
      Var _ -> [] -- B[x] = {}
      App e1 e2 -> go e1 ++ go e2 -- B[e1e2] = B[e1] U B[e2]
      Lam x e -> x : go e -- B[λx.t] = {x} U

-- | α-conversion
--
-- ## Examples
--
--    > alpha g y g(λx.gx) = y(λx.yx)
--    > alpha g y λx.gx(λgf.gxy) = λx.gx(λgf.gxy)
alpha :: Identifier -> Exp -> Exp -> Exp
alpha x n = \case
  Var y
    | y == x -> n -- x[x := N] = N
    | otherwise -> Var y -- y[x := N] = y, if x ≠ y
  Lam y m
    -- (λy.M)[x := N] = λy.(M[x := N]), if x ≠ y and y ∉ FV(N)
    | x /= y && y `notElem` free n -> Lam y (alpha x n m)
    | otherwise -> Lam x m -- (λx.M)[x := N] = λx.M

  -- (M1 M2)[x := N] = (M1[x := N]) (M2[x := N])
  App m1 m2 -> App (alpha x n m1) (alpha x n m2)

equal :: Exp -> Exp -> Bool
equal = go 0 M.empty
  where
    go :: Int -> M.Map Identifier Int -> Exp -> Exp -> Bool
    go _ scope (Var n1) (Var n2)
      | n1 == n2 = True
      | otherwise = getOrElse False $ (==) <$> M.lookup n1 scope <*> M.lookup n2 scope
    go n scope (App e11 e12) (App e21 e22) =
      go n scope e11 e21 && go n scope e12 e22
    go n scope (Lam x1 e1) (Lam x2 e2) =
      go (n + 1) (M.insert x1 n $ M.insert x2 n scope) e1 e2
    go _ _ _ _ = False

getOrElse :: p -> Maybe p -> p
getOrElse _ (Just a) = a
getOrElse b _ = b

-- parser ---------------------------------------------------------------------

parse :: String -> Either P.ParseError Exp
parse = P.parse expP ""

identifierP :: Parser Identifier
identifierP = P.try singleCharIdentifierP <|> mutlipleCharIdentifierP
  where
    mutlipleCharIdentifierP =
      P.char '_'
        *> P.many1 (P.oneOf validIdentifierChars <|> P.digit)
    singleCharIdentifierP =
      (:)
        <$> P.oneOf validIdentifierChars
        <*> P.many P.digit

validIdentifierChars :: [Char]
validIdentifierChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZβλΣσϕφ∧∨¬Φ"

varP :: Parser Exp
varP = Var <$> identifierP

expP :: Parser Exp
expP = P.chainl1 lamVarP operatorP
  where
    operatorP = pure App
    lamVarP =
      paranthesesP (expP <* P.skipMany (P.oneOf " "))
        <|> (lamP <* P.skipMany (P.oneOf " "))
        <|> (varP <* P.skipMany (P.oneOf " "))
    paranthesesP = P.between (P.char '(' <* P.skipMany (P.oneOf " ")) (P.char ')')

lamP :: Parser Exp
lamP = do
  name <- lambdaP *> identifierP
  nestedNames <- P.many identifierP
  nestLambda name nestedNames <$> (dotP *> expP)
  where
    lambdaP = P.oneOf "λ\\"
    nestLambda name [] expr = Lam name expr
    nestLambda name (x : xs) expr = Lam name (nestLambda x xs expr)
    spaceP = many $ P.oneOf " "
    dotP = spaceP *> P.oneOf "." *> spaceP
