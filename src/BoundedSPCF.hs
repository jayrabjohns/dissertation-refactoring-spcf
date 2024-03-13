module BoundedSPCF (Term (..), Type (..), Value (..), Label, Environment, emptyEnv, emptyProduct, eval, interpret, interpretIO, substitute, runEval, runEvalIO) where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer
import Data.List
import qualified Data.Map as Map
import qualified Data.Map.Internal.Debug as Map.Debug

type Label = String

type Numeral = Int

type Product = [Term]

emptyProduct :: Term
emptyProduct = Product []

-- A family of n projections πᵢ: Tⁿ => T
-- π₀(t) = I where I is the empty product
projection :: Int -> Product -> Term
projection 0 _ = emptyProduct
projection i prod = Data.List.genericIndex prod (i - 1)

upperBound :: Numeral
upperBound = maxBound :: Numeral

data Term
  = Numeral Numeral
  | Variable Label
  | Lambda Label Type Term
  | Apply Term Term
  | Product Product -- <- sneakily my injection, uses the list constructor to convert it to the underlying datastructre
  | Case Term Term -- <- my projection, uses the index function of the underlying list to return a term
  | Catch Term
  deriving (Eq)

instance Num Term where
  fromInteger = Numeral . fromIntegral
  (+) = undefined
  (*) = undefined
  abs = undefined
  signum = undefined
  negate = undefined

instance Show Term where
  show = beautify 0
    where
      beautify :: Int -> Term -> String
      beautify _ (Numeral i) = show i
      beautify _ (Variable x) = x
      beautify i (Lambda var _ term) = if i /= 0 then "(" ++ s ++ ")" else s where s = "\\" ++ show var ++ {-": " ++ show t ++ -} ". " ++ beautify 0 term
      beautify i (Apply lhs rhs) = if i == 2 then "(" ++ s ++ ")" else s where s = beautify 1 lhs ++ " " ++ beautify 2 rhs
      beautify i (Product []) = if i /= 0 then "(" ++ s ++ ")" else s where s = "I"
      beautify i (Product prod) = if i /= 0 then "(" ++ s ++ ")" else s where s = (intercalate "x" (map (beautify 2) prod))
      beautify i (Case numeral prod) = if i /= 0 then "(" ++ s ++ ")" else s where s = "Case " ++ beautify 2 numeral ++ " " ++ beautify 2 prod
      beautify i (Catch term) = if i /= 0 then "(" ++ s ++ ")" else s where s = "Catch " ++ beautify 2 term

data Type
  = Base
  | (:->) Type Type
  | Empty
  | Cross Type Type -- <- do we need this?
  deriving (Eq)

instance Show Type where
  show = beautify
    where
      beautify :: Type -> String
      beautify Base = "o"
      beautify (Base :-> rhs) = "o -> " ++ beautify rhs
      beautify (lhs :-> rhs) = "(" ++ beautify lhs ++ ") -> " ++ beautify rhs
      beautify (Cross lhs rhs) = "(" ++ beautify lhs ++ ")x(" ++ beautify rhs ++ ")"
      beautify Empty = "0"

-- A mapping of identifiers (labels) to closures. The closure to which an
--   environment E maps an identifier x is normally denoted by E[x].
type Environment = Map.Map Label Value

emptyEnv :: Environment
emptyEnv = Map.empty

-- The result of evaluation of a closure.
--   Either a natural number or another closure.
data Value
  = Nat Int
  | Closure Environment Term
  deriving (Eq)

instance Show Value where
  show (Nat i) = show i
  show (Closure env term) = "Closure (" ++ show term ++ ") (" ++ show env ++ ")"

type Eval a = (ReaderT Environment (ExceptT String (WriterT [String] Identity))) a

runEval :: Eval a -> Environment -> (Either String a, [String])
runEval evl env = runIdentity . runWriterT . runExceptT $ runReaderT evl env

runEvalIO :: Eval a -> Environment -> IO a
runEvalIO evaluation env = do
  let (result, logs) = runEval evaluation env
  _ <- traverse putStrLn logs
  either fail return result

interpret :: Term -> Either String Value
interpret term = fst $ runEval (eval term) emptyEnv

interpretIO :: Term -> IO Value
interpretIO term = runEvalIO (eval term) emptyEnv

-- Evaluation is commonly denoted by ⇓ and is sort of a decomposition of a
--   closure (a redex and an evaluation context) into a value.
--   If the term is of ground type then the result will be either a numeral,
--   a variable, or
--   (either a nautral number or a closure)
eval :: Term -> Eval Value
eval (Numeral i) = return $ Nat i
eval (Variable label) = do
  env <- ask
  tell
    [ "\nEvaluating "
        ++ label
        ++ " with environement:\n"
        ++ Map.Debug.showTreeWith (\k v -> show (k, v)) True False env
    ]
  case Map.lookup label env of
    Just val -> return val
    Nothing -> throwError $ "Undefined variable " ++ label
eval lambda@Lambda {} = do
  env <- ask
  return $ Closure env lambda
eval (Apply lterm rterm) = do
  -- Call by value because evaluating argument before application
  arg <- eval rterm
  lval <- eval lterm
  case lval of
    (Closure env' (Lambda label _ body)) -> do
      env <- ask
      -- Taking the union of the newly constructed environment and the
      --   evironment stored with the closure, this has the effect of closures
      --   inheriting the environemnt in which they are applied, rather than
      --   where they are defined. This is more consistent with how pratical
      --   languages do things. E.g. when passing in an anonymous function,
      --   you would expect it to capture the environemnt where it is called.
      let newEnv = Map.unions [(Map.insert label arg env'), env]
      tell
        [ "\nApplying argument "
            ++ show rterm
            ++ " to body "
            ++ show lterm
            ++ " with environment\n"
            ++ Map.Debug.showTreeWith (\k v -> show (k, v)) True False env'
        ]
      local (const newEnv) (eval body)
    _ ->
      throwError $
        "Error while evaluating application - "
          ++ "lhs of an application should always be an abstraction. "
          ++ "Specifically, "
          ++ show rterm
          ++ " cannot be applied to "
          ++ show lterm
eval prod@(Product terms) = do
  env <- ask
  return $ Closure env prod
eval (Case num prod) = do
  nVal <- eval num
  pVal <- eval prod
  case (nVal, pVal) of
    ((Nat i), (Closure _ (Product p))) -> eval $ projection i p
    _ ->
      throwError $
        "Error while evaluating "
          ++ show (Case num prod)
          ++ "\n Either "
          ++ show num
          ++ " doesn't reduce to a numeral or "
          ++ show prod
          ++ "doesn't reduce to a product"
eval (Catch body) = do
  env <- ask
  let usedLabels = Map.keys env
  let catchResult = runCatch (catch body) usedLabels
  case catchResult of
    Constant i n -> return $ Nat (i + n)
    ArgumentIndex i -> return $ Nat i
    Diverge msg -> throwError msg

type Catch a = ReaderT [Label] Identity a

runCatch :: Catch a -> [Label] -> a
runCatch ctch args = runIdentity $ runReaderT ctch args

data CatchResult
  = Constant Int Int
  | ArgumentIndex Int
  | Diverge String

-- Following Laird's definition of catch which has ground type rather than
--   Cartwreight & Fallensien's family of t -> o typed operators.
-- Need a way to track the index of each argument. Perhaps store a special variable in the context?
-- The alternative would be either re-writing the eval function inside of the case for catch,
-- or passing in a special parameter counting the index / having a custom monad tracking hte same state

-- catch : (Tⁿ => 0) => n
-- It takes in a function that takes in a product and returns the empty type
--   and returns a numeral (the argument index).
catch :: Term -> Catch CatchResult
catch (Numeral i) = do
  args <- ask
  return $ Constant i (length args)
catch (Variable label) = do
  args <- ask
  return $ case elemIndex label args of
    Just i -> ArgumentIndex i
    Nothing ->
      Diverge $
        "Error, variable "
          ++ label
          ++ " is unbound and so cannot be caught. This program shouldn't "
          ++ "have correctly typed checked in the first place."
catch (Lambda label _ body) = do
  args <- ask
  let updatedArgs = (args ++ [label])
  local (const updatedArgs) (catch body) -- TODO: ensure variable shadowing works as expected
catch (Apply lhs rhs) = do
  rightResult <- catch rhs
  case rightResult of
    Constant {} -> catch lhs
    otherResult -> return otherResult
catch (Product prod) = undefined -- map (catch args) prod
catch (Case n prod) = undefined -- map (Case n) prod
catch (Catch body) = catch body

substitute :: Label -> Term -> Term -> Term
substitute _ _ literal@Numeral {} = literal
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
substitute old new (Product prod) = undefined
substitute old new (Case n prod) = undefined
substitute old new (Catch body) = Catch (substitute old new body)

rename :: Label -> Label -> Term -> Term
rename _ _ literal@(Numeral _) = literal
rename old new var@(Variable label)
  | label == old = Variable new
  | otherwise = var
rename old new abst@(Lambda label t body)
  | label == old = abst
  | otherwise = Lambda label t (rename old new body)
rename old new (Apply lhs rhs) = Apply (rename old new lhs) (rename old new rhs)
rename old new (Product prod) = undefined
rename old new (Case n prod) = undefined
rename old new (Catch body) = Catch (rename old new body)

fresh :: [Label] -> Label
fresh usedLabels = case (find (`notElem` usedLabels) labels) of
  Just label -> label
  Nothing -> error "Error, somehow all variable names have been used. This shouldn't be possible."

used :: Term -> [Label]
used (Numeral _) = []
used (Variable label) = [label]
used (Lambda label _ term) = label : used term
used (Apply lhs rhs) = used lhs ++ used rhs
used (Product prod) = prod >>= used
used (Case num prod) = used num ++ used prod
used (Catch term) = used term

labels :: [Label]
labels = alphabetLazyList ++ alphaNumsLazyList

alphabetLazyList :: [String]
alphabetLazyList = [[char] | char <- ['a' .. 'z']]

alphaNumsLazyList :: [String]
alphaNumsLazyList = [char : show i | i <- [1 :: Int ..], char <- ['a' .. 'z']]