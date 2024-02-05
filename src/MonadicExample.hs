module MonadicExample where

import Control.Monad.Except
import Control.Monad.Except (liftEither)
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as Map
import Data.Maybe

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

type Eval a = ExceptT String Identity a

runEval :: Eval a -> Either String a
runEval = runIdentity . runExceptT

eval :: Env -> Expression -> Eval Value
eval _ (Literal i) = return $ Nat i
eval env (Variable label) =
  liftEither
    ( case Map.lookup label env of
        Just val -> Right val
        Nothing -> Left $ "No variable defined as " ++ label
    )
eval env (Abstract label expression) = return $ Fun env label expression
eval env (Apply e1 e2) = do
  val1 <- eval env e1
  val2 <- eval env e2
  case val1 of
    Fun env' n body -> eval (Map.insert n val2 env') body
    _ -> throwError $ "Error while evaluating - cannot apply an expression to a literal. Specifically, " ++ show e2 ++ " cannot be applied to " ++ show e1
eval env (Plus e1 e2) = do
  val1 <- eval env e1
  val2 <- eval env e2
  case (val1, val2) of
    (Nat i1, Nat i2) -> return $ Nat (i1 + i2)
    _ -> throwError "Plus is only defined for natural numbers, not a mixture of expressions and natural numbers."