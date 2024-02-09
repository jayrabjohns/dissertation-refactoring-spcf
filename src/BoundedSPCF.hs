module BoundedSPCF (Term (..), Type (..), Value (..), eval) where

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
      beautify _ (Variable x) = show x
      beautify i (Lambda var t term) = if i /= 0 then "(" ++ s ++ ")" else s where s = "\\" ++ show var ++ ": " ++ show t ++ " . " ++ beautify 0 term
      beautify i (Apply lhs rhs) = if i == 2 then "(" ++ s ++ ")" else s where s = beautify 1 lhs ++ " " ++ beautify 2 rhs

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
-- --   Using a stack because variables with lower scode tend to be used first,
-- --   (FIFO) as well as ease of implementation for variable shadowing.
type Environment = Map.Map Label Value

-- The result of evaluation of a closure.
--   Either a natural number or another closure.
-- data Value
--   = Nat Int
--   | Closure Environment Label Term
--   deriving (Show, Eq)

data Value
  = Nat Int
  | Closure Environment Term
  deriving (Show, Eq)

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
      ++ "rhs of an application should always be an abstraction. "
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
    Nat i ->
      if i > 0
        then Right (Nat (i - 1))
        else Right (Nat 0)
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
eval (Closure env (YComb term@(Lambda label _ body))) = do
  let innerBody = Closure env (YComb term)
  let newEnv = Map.insert label innerBody env
  eval (Closure newEnv body)
eval (Closure _ YComb {}) = Left "Error, It is only possible to take a fixed point of a lambda abstraction"

-- Evaluation is commonly denoted by ⇓ and is sort of a decomposition of a
--   closure (a redex and an evaluation context) into a value.
--   If the term is of ground type then the result will be either a numeral,
--   a variable, or
--   (either a nautral number or a closure)
-- eval :: Environment -> Term -> Either String Value
-- eval _ (Literal i) = Right (Nat i)
-- eval env (Variable label) = case Map.lookup label env of
--   Just val -> Right val
--   Nothing -> Left ("Undefined variable " ++ label)
-- eval env (Lambda label _ body) = Right (Closure env label body)
-- eval env (Apply lterm rterm) = do
--   lval <- eval env lterm
--   case lval of
--     Closure localEnv label body -> do
--       rval <- eval env rterm -- Call by value, evaluate argument before application
--       let updatedEnv = Map.insert label rval localEnv
--       eval updatedEnv body
--     _ ->
--       Left $
--         "Error while evaluating application"
--           ++ "cannot apply closures when lhs is not also a closure. "
--           ++ "Specifically, "
--           ++ show rterm
--           ++ " cannot be applied to "
--           ++ show lterm
-- eval env (Succ term) = do
--   res <- eval env term
--   case res of
--     Nat i -> Right (Nat (i + 1))
--     _ -> Left "Succ Error"
-- eval env (Pred term) = do
--   res <- eval env term
--   case res of
--     Nat i -> Right (Nat (i - 1))
--     _ -> Left "Pred Error"
-- eval env (If0 (Literal (i)) iftrue iffalse) = if i == 0 then Right iftrue else Right iffalse
-- eval env (If0 {}) = Left "error"
-- eval _ (YComb term) = Left ("")
