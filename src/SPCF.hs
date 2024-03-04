module SPCF (Term (..), Error (..), Type (..), Value (..), Label, Environment, eval, interpret, substitute) where

import Data.List
import qualified Data.Map as Map
import qualified Data.Map.Internal.Debug as Map.Debug
import Debug.Trace

type Label = String

type ArgumentIndex = Int

type Metadata = (Type, ArgumentIndex)

data Term
  = Literal Int
  | Variable Label
  | Lambda Label Metadata Term
  | Apply Term Term
  | Succ Term
  | Pred Term
  | YComb Term
  | If0 Term Term Term
  | Error Error
  | Catch Term
  deriving (Eq)

data Error
  = Error1
  | Error2
  deriving (Eq, Show)

instance Num Term where
  fromInteger = Literal . fromIntegral
  (+) = undefined
  (*) = undefined
  abs = undefined
  signum = undefined
  negate = undefined

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
      beautify _ (Error Error1) = "Error1"
      beautify _ (Error Error2) = "Error2"
      beautify i (Catch term) = if i /= 0 then "(" ++ s ++ ")" else s where s = "Catch " ++ beautify 2 term

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
  | Err Error
  | Closure Environment Term
  deriving (Eq)

instance Show Value where
  show (Nat i) = show i
  show (Err err) = show err
  show (Closure env term) = "Closure " ++ show env ++ show term

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
used (Error _) = []
used (Catch term) = used term

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
rename _ _ err@(Error _) = err
rename old new (Catch body) = Catch (rename old new body)

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
substitute _ _ err@(Error _) = err
substitute old new (Catch body) = Catch (substitute old new body)

mapOnce :: (Term -> Term) -> Term -> Term
mapOnce _ lit@(Literal _) = lit
mapOnce _ var@(Variable _) = var
mapOnce f (Lambda label typ body) = Lambda label typ (f body)
mapOnce f (Apply lhs rhs) = Apply (f lhs) (f rhs)
mapOnce f (Succ body) = Succ (f body)
mapOnce f (Pred body) = Pred (f body)
mapOnce f (If0 p l r) = If0 (f p) (f l) (f r)
mapOnce f (YComb body) = YComb (f body)
mapOnce _ err@(Error _) = err
mapOnce f (Catch body) = Catch (f body)

interpret :: Term -> Either String Value
interpret term = eval (Closure Map.empty term)

numParamsLabel :: String
numParamsLabel = "num_params"

-- Evaluation is commonly denoted by ⇓ and is sort of a decomposition of a
--   closure (a redex and an evaluation context) into a value.
--   If the term is of ground type then the result will be either a numeral,
--   a variable, or
--   (either a nautral number or a closure)
eval :: Value -> Either String Value
eval (Nat i) = Right (Nat i)
eval err@(Err _) = Right err
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
eval (Closure env (Lambda label (typ, _) body)) = do
  let (env', argumentIndex) = case Map.lookup numParamsLabel env of
        Just (Nat i) -> (Map.insert numParamsLabel (Nat (i + 1)) env, i + 1)
        _ -> (Map.insert numParamsLabel (Nat 1) env, 1)
  return $ Closure env' (Lambda label (typ, argumentIndex) body)
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
    err@(Err _) -> Right err
    _ ->
      Left $
        "Cannot check if non numerical value is 0. "
          ++ "Specifically, "
          ++ show cond
          ++ " doesn't evaluate to a number."
eval (Closure env (YComb term)) = do
  val <- eval (Closure env term)
  case val of
    Closure env' abst@(Lambda label _ body) -> do
      let selfApplication = (substitute label (YComb abst) body)
      eval (Closure env' selfApplication)
    _ -> Left "Error, it is only possible to take a fixed point of a lambda abstraction"
eval (Closure _ (Error err)) = Right (Err err)
-- Following Laird's definition of catch which has ground type rather than
--   Cartwreight & Fallensien's family of t -> o typed operators.
-- Need a way to track the index of each argument. Perhaps store a special variable in the context?
-- The alternative would be either re-writing the eval function inside of the case for catch,
-- or passing in a special parameter counting the index / having a custom monad tracking hte same state
eval (Closure env (Catch body)) = do
  trace ("Catching in " ++ show body)
    (case body of
      Variable label -> case Map.lookup numParamsLabel env of
        Just (Nat i) ->
          trace
            ("Caught variable " ++ label ++ " with index " ++ show i)
            return $ Nat i
        _ ->
          Left $
            "Error counting argument indices, somehow the special variable "
              ++ numParamsLabel
              ++ " exists in the context"
      term -> eval $ Closure env (mapOnce Catch term))
