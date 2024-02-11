module BoundedSPCF (Term (..), Type (..), Value (..), Environment, eval, add) where

import qualified Data.Map as Map

type Label = String

data Term
  = Literal Int
  | Variable Label
  | Lambda Label Type Term
  | Apply Term Term
  | Succ Term
  | Pred Term
  | YComb Term
  | If0 Term Term Term
  deriving
    ( -- | Error1
      -- | Error2
      -- | Catch Label
      Eq
    )

instance Show Term where
  show = beautify 0
    where
      beautify :: Int -> Term -> String
      beautify _ (Literal i) = show i
      beautify _ (Variable x) = show x
      beautify i (Lambda var t term) = if i /= 0 then "(" ++ s ++ ")" else s where s = "\\" ++ show var ++ ": " ++ show t ++ " . " ++ beautify 0 term
      beautify i (Apply lhs rhs) = if i == 2 then "(" ++ s ++ ")" else s where s = beautify 1 lhs ++ " " ++ beautify 2 rhs
      beautify i (Succ term) = if i /= 0 then "(" ++ s ++ ")" else s where s = "Succ " ++ beautify 0 term
      beautify i (Pred term) = if i /= 0 then "(" ++ s ++ ")" else s where s = "Pred " ++ beautify 0 term
      beautify i (YComb term) = if i /= 0 then "(" ++ s ++ ")" else s where s = "Y " ++ beautify 0 term
      beautify i (If0 cond lterm rterm) = if i == 2 then "(" ++ s ++ ")" else s where s = "If " ++ beautify 1 cond ++ " then " ++ beautify 1 lterm ++ " else " ++ beautify 2 rterm

-- beautify _ Error1 = "error1"
-- beautify _ Error2 = "error2"
-- beautify _ (Catch x) = x

-- In the bounded SPCF, the base type will also be bounded. E.g. Boolean or n natural ints

data Type
  = Base
  | (:->) Type Type
  deriving (Eq)

instance Show Type where
  show = beautify
    where
      beautify :: Type -> String
      beautify Base = "o"
      beautify (Base :-> rhs) = "o -> " ++ beautify rhs
      beautify (lhs :-> rhs) = "(" ++ beautify lhs ++ ") -> " ++ beautify rhs

-- A mapping of identifiers (labels) to closures. The closure to which an
--   environment E maps an identifier x is normally denoted by E[x].
type Environment = Map.Map Label Value

-- The result of evaluation of a closure.
--   Either a natural number or another closure.
data Value
  = Nat Int
  | Closure Environment Term
  deriving (Show, Eq)

-- Evaluation is commonly denoted by ⇓ and is sort of a decomposition of a
--   closure (a redex and an evaluation context) into a value.
--   If the term is of ground type then the result will be either a numeral,
--   a variable, or
--   (either a nautral number or a closure)
eval :: Value -> Either String Value
eval (Nat i) = Right (Nat i)
eval (Closure _ (Literal i)) = Right (Nat i)
eval (Closure env (Variable label)) = case Map.lookup label env of
  Just val -> Right val
  Nothing -> Left ("Undefined variable " ++ label)
eval abstraction@(Closure _ Lambda {}) = Right abstraction
eval (Closure env (Apply (Lambda label _ body) rterm)) = do
  -- Call by value because evaluating argument before application
  arg <- eval (Closure env rterm)
  eval (Closure (Map.insert label arg env) body)
eval (Closure _ (Apply lterm rterm)) =
  Left $
    "Error while evaluating application - "
      ++ "lhs of an application should always be an abstraction. "
      ++ "Specifically, "
      ++ show rterm
      ++ " cannot be applied to "
      ++ show lterm
eval (Closure env (Succ term)) = do
  val <- eval (Closure env term)
  case val of
    Nat i -> Right (Nat (i + 1))
    _ -> Left $ "Cannot apply successor to non natural number" ++ show term
eval (Closure env (Pred term)) = do
  val <- eval (Closure env term)
  case val of
    Nat 0 -> Right (Nat 0)
    Nat i -> Right (Nat (i - 1))
    _ -> Left $ "Cannot apply predeccessor to non natural number" ++ show term
eval (Closure env (If0 cond iftrue iffalse)) = do
  val <- eval (Closure env cond)
  case val of
    Nat 0 -> eval (Closure env iftrue)
    Nat _ -> eval (Closure env iffalse)
    _ ->
      Left $
        "Cannot check if non numericla value is 0."
          ++ "Specifically, "
          ++ show cond
          ++ "doesn't evaluate to a number."
-- Y f = f (Y f)
eval (Closure env (YComb term)) = do
  val <- eval (Closure env term)
  case val of
    Closure env' (Lambda label _ body) ->
      let innerBody = Closure env (YComb term)
          newEnv = Map.insert label innerBody env'
       in eval (Closure newEnv body)
    _ -> Left "Error, it is only possible to take a fixed point of a lambda abstraction in SPCF."

-- multerm :: Term -> Term -> Maybe Term
-- multerm (Literal l) (Literal r) = Just $ Literal (l * r)
-- multerm _ _ = Nothing

-- mulvals :: Value -> Value -> Either String Value
-- mulvals lhs rhs = do
--   lval <- eval lhs
--   rval <- eval rhs
--   case (lval, rval) of
--     ((Nat l), (Nat r)) -> Right (Nat (l * r))
--     _ -> Left ""

-- (a -> a) -> a
-- ((a -> a) -> (a -> a)) -> (a -> a)
fix :: (a -> a) -> a
fix f = x where x = f x

add' x y = (fix aux) x y
  where
    aux f 0 y = y
    aux f x y = f (x - 1) (y + 1)

fix' :: (a -> a -> a) -> a -> a
fix' f a = x where x = f x a

fact :: Int -> Int
fact = (fix fac)
  where
    fac _ 0 = 1
    fac f x = x * f (x - 1)

-- add :: Term -> Term -> Value
-- add lhs rhs = Closure Map.empty (YComb (aux lhs rhs))
--   where
--     aux :: (Term -> Term -> Term) -> Term -> Term -> Term
--     aux f lhs rhs = If0 rhs lhs (f (Succ rhs) (Pred lhs))

-- Definition for addition by recursing on the left term until 0.
-- f x y = (x + y) = | x > 0     f (x - 1) (y + 1)
--                   | otherwise y
addTerm :: Term
addTerm =
  YComb
    ( Lambda
        "f"
        ((Base :-> Base :-> Base) :-> Base :-> Base :-> Base)
        ( Lambda
            "x"
            (Base :-> Base :-> Base)
            ( Lambda
                "y"
                (Base :-> Base)
                ( If0
                    (Variable "x")
                    (Variable "y")
                    ( Succ
                        ( Apply
                            (Apply (Variable "f") (Pred (Variable "x")))
                            (Variable "y")
                        )
                    )
                )
            )
        )
    )

-- addTerm :: Term
-- addTerm =
--   YComb
--     ( Lambda
--         "x"
--         (Base :-> Base :-> Base)
--         ( Lambda
--             "y"
--             (Base :-> Base)
--             ( If0
--                 (Variable "x")
--                 (Variable "y")
--                 ( Succ
--                     ( Apply
--                         (Apply (Variable "f") (Pred (Variable "x")))
--                         (Variable "y")
--                     )
--                 )
--             )
--         )
--     )

add :: Term -> Term -> Term
add x y = Apply (Apply addTerm x) y

mulvals :: Value -> Value -> Either String Value
mulvals lhs rhs = do
  lval <- eval lhs
  rval <- eval rhs
  case (lval, rval) of
    ((Nat l), (Nat r)) -> Right (Nat (l * r))
    _ -> Left ""