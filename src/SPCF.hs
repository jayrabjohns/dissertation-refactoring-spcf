module SPCF (Term (..), Type (..), Value (..), Environment, eval, add) where

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
  deriving
    ( -- | Error1
      -- | Error2
      -- | Catch
      Eq
    )

instance Show Term where
  show = beautify 0
    where
      beautify :: Int -> Term -> String
      beautify _ (Literal i) = show i
      beautify _ (Variable x) = x
      beautify i (Lambda var t term) = if i /= 0 then "(" ++ s ++ ")" else s where s = "\\" ++ show var ++ {-": " ++ show t ++ -} ". " ++ beautify 0 term
      beautify i (Apply lhs rhs) = if i == 2 then "(" ++ s ++ ")" else s where s = beautify 1 lhs ++ " " ++ beautify 2 rhs
      beautify i (Succ term) = if i /= 0 then "(" ++ s ++ ")" else s where s = "Succ " ++ beautify 2 term
      beautify i (Pred term) = if i /= 0 then "(" ++ s ++ ")" else s where s = "Pred " ++ beautify 2 term
      beautify i (YComb term) = if i /= 0 then "(" ++ s ++ ")" else s where s = "Y " ++ beautify 2 term
      beautify i (If0 cond lterm rterm) = if i == 2 then "(" ++ s ++ ")" else s where s = "If0 " ++ beautify 1 cond ++ " then " ++ beautify 1 lterm ++ " else " ++ beautify 1 rterm

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

used :: Term -> [Label]
used (Variable lable) = [lable]
used (Lambda lable _ term) = lable : used term
used (Apply lhs rhs) = used lhs ++ used rhs

rename :: Label -> Label -> Term -> Term
rename old new (Variable x)
  | x == old = Variable new
  | otherwise = Variable x
rename old new (Lambda var t term)
  | var == old = Lambda var t term
  | otherwise = Lambda var t (rename old new term)
rename old new (Apply lhs rhs) = Apply (rename old new lhs) (rename old new rhs)

substitute :: Label -> Term -> Term -> Term
substitute old new (Variable x)
  | old == x = new
  | otherwise = Variable x
substitute old new (Lambda var t term)
  | old == var = Lambda var t term
  | otherwise = Lambda freshVar t (substitute old new (rename var freshVar term))
  where
    freshVar :: Label
    freshVar = fresh (used new `merge` used term `merge` [old, var])
substitute old new (Apply lhs rhs) = Apply (substitute old new lhs) (substitute old new rhs)


-- Todo: Substitution (capture avoiding ofc) when evaluating an abstraction

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
eval (Closure env (Apply lterm rterm)) = do
  -- Call by value because evaluating argument before application
  func <- eval (Closure env lterm)
  case func of
    (Closure env' (Lambda label _ body)) -> do
      arg <- eval (Closure env rterm)
      let innerEnv = (Map.insert label arg env')
      let result = eval (Closure innerEnv body)
      trace
        ( "\nApplying argument "
            ++ show rterm
            ++ " to body "
            ++ show lterm
            ++ " with environment\n"
            ++ Map.Debug.showTreeWith (\k x -> show (k, x)) True False innerEnv
        )
        result
    _ ->
      Left $
        "Error while evaluating application - "
          ++ "lhs of an application should always be an abstraction. "
          ++ "Specifically, "
          ++ show lterm
          ++ " cannot be applied to with "
          ++ show rterm
          ++ ". \nEnv:\n"
          ++ Map.Debug.showTreeWith (\k x -> show (k, x)) True False env
eval (Closure env (Succ term)) = do
  val <- eval (Closure env term)
  case val of
    Nat i -> Right (Nat (i + 1))
    _ -> Left $ "Cannot apply successor to non natural number" ++ show term
eval (Closure env (Pred term)) = do
  val <- eval (Closure env term)
  case val of
    Nat 0 -> Right (Nat 0) -- What is the pred of 0?
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