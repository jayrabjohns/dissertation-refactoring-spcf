module SPCF (Term (..), Type (..), Value (..), Label, Environment, eval, substitute) where

import Data.List
import qualified Data.Map as Map
import qualified Data.Map.Internal.Debug as Map.Debug
import Debug.Trace

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
  | Error1
  | Error2
  deriving (Eq)

-- \| Catch

instance Show Term where
  show = beautify 0
    where
      beautify :: Int -> Term -> String
      beautify _ (Literal i) = show i
      beautify _ (Variable x) = x
      beautify i (Lambda var _ term) = if i /= 0 then "(" ++ s ++ ")" else s where s = "\\" ++ show var ++ {-": " ++ show t ++ -} ". " ++ beautify 0 term
      beautify i (Apply lhs rhs) = if i == 2 then "(" ++ s ++ ")" else s where s = beautify 1 lhs ++ " " ++ beautify 2 rhs
      beautify i (Succ term) = if i /= 0 then "(" ++ s ++ ")" else s where s = "Succ " ++ beautify 2 term
      beautify i (Pred term) = if i /= 0 then "(" ++ s ++ ")" else s where s = "Pred " ++ beautify 2 term
      beautify i (YComb term) = if i /= 0 then "(" ++ s ++ ")" else s where s = "Y " ++ beautify 2 term
      beautify i (If0 cond lterm rterm) = if i == 2 then "(" ++ s ++ ")" else s where s = "If0 " ++ beautify 1 cond ++ " then " ++ beautify 1 lterm ++ " else " ++ beautify 1 rterm
      beautify _ Error1 = "error1"
      beautify _ Error2 = "error2"

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

alphabetLazyList :: [String]
alphabetLazyList = [[char] | char <- ['a' .. 'z']]

alphaNumsLazyList :: [String]
alphaNumsLazyList = [char : show i | i <- [1 :: Int ..], char <- ['a' .. 'z']]

labels :: [Label]
labels = alphabetLazyList ++ alphaNumsLazyList

fresh :: [Label] -> Label
fresh usedLabels = case (find (`notElem` usedLabels) labels) of
  Just label -> label
  Nothing -> error "Error, somehow all variable names have been used. This shouldn't be possible."

used :: Term -> [Label]
used (Literal _) = []
used (Variable label) = [label]
used (Lambda label _ term) = label : used term
used (Apply lhs rhs) = used lhs ++ used rhs
used (Succ body) = used body
used (Pred body) = used body
used (If0 cond iftrue iffalse) = used cond ++ used iftrue ++ used iffalse
used (YComb body) = used body
used Error1 = []
used Error2 = []

rename :: Label -> Label -> Term -> Term
rename _ _ literal@(Literal _) = literal
rename old new var@(Variable label)
  | label == old = Variable new
  | otherwise = var
rename old new abst@(Lambda label t body)
  | label == old = abst
  | otherwise = Lambda label t (rename old new body)
rename old new (Apply lhs rhs) = Apply (rename old new lhs) (rename old new rhs)
rename old new (Succ body) = Succ (rename old new body)
rename old new (Pred body) = Pred (rename old new body)
rename old new (If0 cond iftrue iffalse) =
  let r = rename old new
   in If0 (r cond) (r iftrue) (r iffalse)
rename old new (YComb body) = YComb (rename old new body)
rename _ _ Error1 = Error1
rename _ _ Error2 = Error2

substitute :: Label -> Term -> Term -> Term
substitute _ _ literal@Literal {} = literal
substitute old new var@(Variable label)
  | label == old = new
  | otherwise = var
substitute old new abst@(Lambda label t body)
  | label == old = abst
  | otherwise =
      let freshVar = fresh (used new ++ used body ++ [old, label])
       in Lambda freshVar t (substitute old new (rename label freshVar body))
substitute old new (Apply lhs rhs) =
  Apply (substitute old new lhs) (substitute old new rhs)
substitute old new (Succ body) = Succ (substitute old new body)
substitute old new (Pred body) = Pred (substitute old new body)
substitute old new (If0 cond iftrue iffalse) =
  let sub = substitute old new
   in (If0 (sub cond) (sub iftrue) (sub iffalse))
substitute old new (YComb body) = YComb (substitute old new body)
substitute _ _ Error1 = Error1
substitute _ _ Error2 = Error2

-- Evaluation is commonly denoted by ⇓ and is sort of a decomposition of a
--   closure (a redex and an evaluation context) into a value.
--   If the term is of ground type then the result will be either a numeral,
--   a variable, or
--   (either a nautral number or a closure)
eval :: Value -> Either String Value
eval (Nat i) = Right (Nat i)
eval (Closure _ (Literal i)) = Right (Nat i)
eval (Closure env (Variable label)) =
  trace
    ( "\nEvaluating "
        ++ label
        ++ " with environement:\n"
        ++ Map.Debug.showTreeWith (\k v -> show (k, v)) True False env
    )
    ( case Map.lookup label env of
        Just val -> Right val
        Nothing -> Left ("Undefined variable " ++ label)
    )
eval abstraction@(Closure _ Lambda {}) = Right abstraction
eval (Closure env (Apply lterm rterm)) = do
  -- Call by value because evaluating argument before application
  lval <- eval (Closure env lterm)
  case lval of
    (Closure env' (Lambda label _ body)) -> do
      arg <- eval (Closure env rterm)
      -- Taking the union of the newly constructed environment and the
      --   evironment stored with the closure, this has the effect of closures
      --   inheriting the environemnt in which they are applied, rather than
      --   where they are defined. This is more consistent with how pratical
      --   languages do things. E.g. when passing in an anonymous function,
      --   you would expect it to capture the environemnt where it is called.
      let newEnv = Map.unions [(Map.insert label arg env'), env]
      let result = eval (Closure newEnv body)
      trace
        ( "\nApplying argument "
            ++ show rterm
            ++ " to body "
            ++ show lterm
            ++ " with environment\n"
            ++ Map.Debug.showTreeWith (\k v -> show (k, v)) True False env'
        )
        result
    _ ->
      Left $
        "Error while evaluating application - "
          ++ "lhs of an application should always be an abstraction. "
          ++ "Specifically, "
          ++ show rterm
          ++ " cannot be applied to "
          ++ show lterm
          ++ ". \nEnv:\n"
          ++ Map.Debug.showTreeWith (\k x -> show (k, x)) True False env
eval (Closure env (Succ term)) = do
  val <-
    ( trace
        ( "\nFinding successor of "
            ++ show term
            ++ " with environemnt:\n"
            ++ Map.Debug.showTreeWith (\k v -> show (k, v)) True False env
        )
        (eval (Closure env term))
      )
  case val of
    Nat i -> Right (Nat (i + 1))
    _ -> Left $ "Cannot apply successor to non natural number" ++ show term
eval (Closure env (Pred term)) = do
  val <-
    ( trace
        ( "\nFinding predecessor of "
            ++ show term
            ++ " with environemnt:\n"
            ++ Map.Debug.showTreeWith (\k v -> show (k, v)) True False env
        )
        (eval (Closure env term))
      )
  case val of
    Nat 0 -> Right (Nat 0)
    Nat i -> Right (Nat (i - 1))
    _ -> Left $ "Cannot apply predeccessor to non natural number" ++ show term
eval (Closure env (If0 cond iftrue iffalse)) = do
  val <-
    ( trace
        ("if " ++ show cond ++ "\n then " ++ show iftrue ++ "\n else " ++ show iffalse)
        (eval (Closure env cond))
      )
  case val of
    Nat 0 -> eval (Closure env iftrue)
    Nat _ -> eval (Closure env iffalse)
    _ ->
      Left $
        "Cannot check if non numerical value is 0."
          ++ "Specifically, "
          ++ show cond
          ++ "doesn't evaluate to a number."
eval (Closure env (YComb term)) = do
  val <- eval (Closure env term)
  case val of
    Closure env' abst@(Lambda label _ body) -> do
      let selfApplication = (substitute label (YComb abst) body)
      eval (Closure env' selfApplication)
    _ -> Left "Error, it is only possible to take a fixed point of a lambda abstraction in SPCF."
eval (Closure _ Error1) = error "not implemented"
eval (Closure _ Error2) = error "not implemented"
