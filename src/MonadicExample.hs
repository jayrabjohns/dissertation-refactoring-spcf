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

type Eval a = ReaderT Env (ExceptT String Identity) a

runEval :: Env -> Eval a -> Either String a
runEval env ev = runIdentity (runExceptT (runReaderT ev env))

eval :: Expression -> Eval Value
eval (Literal i) = return $ Nat i
eval (Variable label) = do
  env <- ask
  case Map.lookup label env of
    Just val -> return val
    Nothing -> throwError $ "unbounded variable: " ++ label
eval (Abstract label expression) = do
  env <- ask
  return $ Fun env label expression
eval (Apply e1 e2) = do
  val1 <- eval e1
  val2 <- eval e2
  case val1 of
    Fun env' n body -> local (const (Map.insert n val2 env')) (eval body)
    _ -> throwError $ "Error while evaluating - cannot apply an expression to a literal. Specifically, " ++ show e2 ++ " cannot be applied to " ++ show e1
eval (Plus e1 e2) = do
  val1 <- eval e1
  val2 <- eval e2
  case (val1, val2) of
    (Nat i1, Nat i2) -> return $ Nat (i1 + i2)
    _ -> throwError "Plus is only defined for natural numbers, not a mixture of expressions and natural numbers."