module BoundedSPCF (Term (..), Type (..)) where

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
  | If0 Term
  -- | Error1
  -- | Error2
  -- | Catch Label
  deriving (Eq)

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
data Value
  = Nat Int
  | Closure Environment Label Term
  deriving (Show, Eq)

-- Evaluation is commonly denoted by ⇓ and is the act of evaluating a closure
--   (a pair of a term and an environment) into a value 
--   (either a nautral number or a closure)
eval :: Environment -> Term -> Either String Value
eval _ (Literal i) = Right (Nat i)
eval env (Variable label) = case Map.lookup label env of 
  Just val -> Right val
  Nothing -> Left ("Undefined variable " ++ label)
eval env (Lambda label _ body) = Right (Closure env label body)
eval env (Apply lterm rterm) = do
  lval <- eval env lterm
  case lval of
    Closure localEnv label body -> do 
      rval <- eval env rterm -- Call by value, evaluate argument before application
      let updatedEnv = Map.insert label rval localEnv
      eval updatedEnv body
    _ -> Left $ "Error while evaluating application" 
                ++ "cannot apply closures when lhs is not also a closure. "
                ++ "Specifically, " ++ show rterm ++ " cannot be applied to "
                ++ show lterm

