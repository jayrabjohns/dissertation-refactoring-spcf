module MonadicExample where

import qualified Data.Map as Map
import Data.Maybe (fromJust)

-- Inspired by 'Monad Transformers Step by Step'

type Label = String

-- A series of components of which programs will be constructed
data Expression
  = Literal Int
  | Variable Label
  | Plus Expression Expression
  | Abstract Label Expression
  | Apply Expression Expression
  deriving (Show, Eq)

-- The result of an evaluation
-- The env component of a Fun is the environment in whihc the corresponding
--   lambda expression is evaluated
data Value
  = Nat Int
  | Fun Env Label Expression
  deriving (Show, Eq)

-- A mapping from names to values
type Env = Map.Map Label Value

eval :: Env -> Expression -> Value
eval _ (Literal i) = Nat i
eval env (Variable label) = fromJust (Map.lookup label env)
eval env (Abstract label expression) = Fun env label expression
eval env (Apply e1 e2) =
  let val1 = eval env e1
      val2 = eval env e2
   in case val1 of
        Fun env' n body -> eval (Map.insert n val2 env') body
        Nat _ -> error $ "Error while evaluating - cannot apply an expression to a literal. Specifically, " ++ show e2 ++ " cannot be applied to " ++ show e1
eval env (Plus e1 e2) = Nat (lval + rval)
  where
    lval = case eval env e1 of
      Nat i -> i
      Fun {} -> error "Error while evaluating - cannot add expressions"
    rval = case eval env e2 of
      Nat i -> i
      Fun {} -> error "Error while evaluating - cannot add expressions"