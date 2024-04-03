{-# LANGUAGE DeriveFunctor #-}

module SPCF where -- (Term (..), Error (..), Type (..), Value (..), Label, Environment, termInfo, emptyEnv, eval, interpret, interpretIO, substitute, runEval, runEvalIO) where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer
import Data.List
import qualified Data.Map as Map
import qualified Data.Map.Internal.Debug as Map.Debug

type Label = String

data Term info
  = Numeral info Int
  | Variable info Label
  | Lambda info Label Type (Term info)
  | Apply info (Term info) (Term info)
  | Succ info (Term info)
  | Pred info (Term info)
  | YComb info (Term info)
  | If0 info (Term info) (Term info) (Term info)
  | Error info Error
  | Catch info (Term info)
  deriving (Eq, Functor)

termInfo :: Term info -> info
termInfo (Numeral inf _) = inf
termInfo (Variable inf _) = inf
termInfo (Lambda inf _ _ _) = inf
termInfo (Apply inf _ _) = inf
termInfo (If0 inf _ _ _) = inf
termInfo (Succ inf _) = inf
termInfo (Pred inf _) = inf
termInfo (YComb inf _) = inf
termInfo (Error inf _) = inf
termInfo (Catch inf _) = inf

data Command info
  = CBind info Label (Term info)
  | CEval info (Term info)
  deriving (Eq, Functor)

data Prog info = Prog
  { pinfo_of :: info,
    prog_of :: [Command info]
  }
  deriving (Eq, Functor)

type CommandResult info = Either String (Term info)

initState = (emptyEnv, [])

interpProg :: Prog info -> [CommandResult info]
interpProg = (`evalState` initState) . interpCommands . prog_of

type InterpState info = (Environment (Term info), [CommandResult info])
interpCommands :: [Command info] -> State (InterpState info) [CommandResult info]
interpCommands [] = do
  (_, results) <- get
  return results
interpCommands (command : cs) = do
  (env, results) <- get
  put $ case command of 
        (CBind _ label term) -> 
          let updatedEnv = Map.insert label term env 
          in (updatedEnv, results)
        (CEval _ term) -> do
          let result = fst $ runEval (eval term) env
          (env, results ++ [result])
  interpCommands cs
  -- let res = interpCommand env command
  -- put $ case res of
  --   Left (label, term) -> (Map.insert label (substEnv env term) env, results ++ [res])
  --   Right _ -> (env, results ++ [res])
  -- interpCommands cs

-- interpCommand :: Environment (Term info) -> Command info -> Either (Label, Term info) (Term info)
-- interpCommand _ (CBind fi id t) = Left (id, t)
-- interpCommand env (CEval fi t) =
--   Right (eval (fold (\acc k v -> termSubst k v acc) t env))

-- substEnv :: Environment (Term info) -> Term info -> Term info
-- substEnv env term =
--   -- fold (\acc k v -> termSubst k v acc) t env
--     foldl (\acc label ->
--             case Map.lookup id env of
--               Just v  -> substitute label v acc
--               Nothing -> error "free variable not bound in environment")
--     term (freeVars term)

data Error
  = Error1
  | Error2
  deriving (Eq, Show)

-- instance Num (Term info) where
--   fromInteger = (Numeral info) . fromIntegral
--   (+) = undefined
--   (*) = undefined
--   abs = undefined
--   signum = undefined
--   negate = undefined

instance Show (Term info) where
  show = beautify 0
    where
      beautify :: Int -> Term info -> String
      beautify _ (Numeral _ i) = show i
      beautify _ (Variable _ x) = x
      beautify i (Lambda _ var _ term) = if i /= 0 then "(" ++ s ++ ")" else s where s = "\\" ++ show var ++ {-": " ++ show t ++ -} ". " ++ beautify 0 term
      beautify i (Apply _ lhs rhs) = if i == 2 then "(" ++ s ++ ")" else s where s = beautify 1 lhs ++ " " ++ beautify 2 rhs
      beautify i (Succ _ term) = if i /= 0 then "(" ++ s ++ ")" else s where s = "Succ " ++ beautify 2 term
      beautify i (Pred _ term) = if i /= 0 then "(" ++ s ++ ")" else s where s = "Pred " ++ beautify 2 term
      beautify i (YComb _ term) = if i /= 0 then "(" ++ s ++ ")" else s where s = "Y " ++ beautify 2 term
      beautify i (If0 _ cond lterm rterm) = if i == 2 then "(" ++ s ++ ")" else s where s = "If0 " ++ beautify 1 cond ++ " then " ++ beautify 1 lterm ++ " else " ++ beautify 1 rterm
      beautify _ (Error _ Error1) = "Error1"
      beautify _ (Error _ Error2) = "Error2"
      beautify i (Catch _ term) = if i /= 0 then "(" ++ s ++ ")" else s where s = "Catch " ++ beautify 2 term

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
type Environment a = Map.Map Label a

emptyEnv :: Environment a
emptyEnv = Map.empty

showEnv :: (Show a) => Environment a -> String
showEnv env = Map.Debug.showTreeWith (\k v -> show (k, v)) True False env

-- The result of evaluation of a closure.
--   Either a natural number or another closure.
data Value info
  = Nat Int
  | Err Error
  | Closure (Environment (Term info)) (Term info)
  deriving (Eq)

instance Show (Value info) where
  show (Nat i) = show i
  show (Err err) = show err
  show (Closure env term) = "Closure " ++ show env ++ show term

type Eval a = (ReaderT (Environment a) (ExceptT String (WriterT [String] Identity))) a

-- termToValue :: Eval (Term info) -> Eval (Value info)
-- termToValue evaluation = do
--   result <- evaluation
--   undefined

-- env <- ask
-- return $ case result of
--   Numeral _ i -> Nat i
--   Error _ e -> Err e
--   term -> Closure env term

runEval :: Eval a -> Environment a -> (Either String a, [String])
runEval evl env = runIdentity . runWriterT . runExceptT $ runReaderT evl env

runEvalIO :: Eval a -> Environment a -> IO a
runEvalIO evaluation env = do
  let (result, logs) = runEval evaluation env
  _ <- traverse putStrLn logs
  either fail return result

interpret :: Term info -> Either String (Term info)
interpret term = fst $ runEval (eval term) emptyEnv

interpretIO :: Term info -> IO (Term info)
interpretIO term = runEvalIO (eval term) emptyEnv

-- Evaluation is commonly denoted by ⇓ and is sort of a decomposition of a
--   closure (a redex and an evaluation context) into a value.
--   If the term is of ground type then the result will be either a numeral,
--   a variable, or
--   (either a nautral number or a closure)
eval :: Term info -> Eval (Term info)
eval lit@(Numeral {}) = return lit
eval err@(Error {}) = return err
eval (Variable _ label) = do
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
eval (Lambda inf label typ body) = return $ Lambda inf label typ (normalise body)
eval (Apply _ lterm rterm) = do
  -- Call by value because evaluating argument before application
  arg <- eval rterm
  lval <- eval lterm
  case lval of
    Lambda _ label _ body -> do
      env <- ask
      let newEnv = Map.insert label arg env
      let newBody = substitute label arg body
      tell
        [ "Apply ("
            ++ show lval
            ++ ") ["
            ++ show arg
            ++ "/"
            ++ show label
            ++ "] with new environment\n"
            ++ showEnv env
        ]
      local (const newEnv) (eval newBody)
    _ ->
      throwError $
        "Error while evaluating application - "
          ++ "lhs of an application should always be an abstraction. "
          ++ "Specifically, "
          ++ show rterm
          ++ " cannot be applied to "
          ++ show lterm
-- ++ Map.Debug.showTreeWith (\k x -> show (k, x)) True False env
eval (Succ _ term) = do
  tell
    [ "\nFinding successor of "
        ++ show term
        ++ " with environemnt:\n"
        -- ++ Map.Debug.showTreeWith (\k v -> show (k, v)) True False env
    ]
  val <- eval term
  case val of
    Numeral inf i -> return $ Numeral inf (i + 1)
    _ -> throwError $ "Cannot apply successor to non natural number" ++ show term
eval (Pred _ term) = do
  tell
    [ "\nFinding predecessor of "
        ++ show term
    ]
  val <- eval term
  case val of
    Numeral inf 0 -> return $ Numeral inf 0
    Numeral inf i -> return $ Numeral inf (i - 1)
    _ -> throwError $ "Cannot apply predeccessor to non natural number " ++ show term
eval (If0 _ cond iftrue iffalse) = do
  tell
    [ "if "
        ++ show cond
        ++ "\n then "
        ++ show iftrue
        ++ "\n else "
        ++ show iffalse
    ]
  val <- eval cond
  case val of
    Numeral _ 0 -> eval iftrue
    Numeral {} -> eval iffalse
    err@(Error {}) -> return err
    _ ->
      throwError $
        "Cannot check if non numerical value is 0. "
          ++ "Specifically, "
          ++ show cond
          ++ " doesn't evaluate to a number."
eval (YComb inf term) = do
  val <- eval term
  case val of
    abst@(Lambda _ label _ body) -> do
      let selfApplication = substitute label (YComb inf abst) body
      eval selfApplication
    _ -> throwError "Error, it is only possible to take a fixed point of a lambda abstraction"
-- Following Laird's definition of catch which has ground type rather than
--   Cartwreight & Fallensien's family of t -> o typed operators.
-- Need a way to track the index of each argument. Perhaps store a special variable in the context?
-- The alternative would be either re-writing the eval function inside of the case for catch,
-- or passing in a special parameter counting the index / having a custom monad tracking hte same state
eval (Catch inf body) = do
  env <- ask
  let usedLabels = Map.keys env
  case catch body usedLabels of
    CaughtError err -> return $ Error inf err
    Constant i n -> return $ Numeral inf (i + n)
    ArgumentIndex i -> return $ Numeral inf i
    Diverge msg -> throwError msg

data CatchResult
  = Constant Int Int
  | CaughtError Error
  | ArgumentIndex Int
  | Diverge String

catch :: Term info -> [Label] -> CatchResult
catch (Numeral _ i) args = Constant i (length args)
catch (Error _ err) _ = CaughtError err
catch (Variable _ label) args = case lastElemIndex label args of
  Just i -> ArgumentIndex i
  Nothing ->
    Diverge $
      "Error, variable "
        ++ label
        ++ " is unbound and so cannot be caught. This program shouldn't "
        ++ "have correctly typed checked in the first place."
catch (Lambda _ label _ body) args = catch body (args ++ [label]) -- TODO: ensure variable shadowing works as expected
catch (Apply _ lhs rhs) args =
  case catch rhs args of
    Constant {} -> catch lhs args
    otherResult -> otherResult
catch (Succ _ body) args = catch body args
catch (Pred _ body) args = catch body args
catch (If0 _ predicate tt ff) args =
  case catch predicate args of
    Constant 0 _ -> catch tt args
    Constant {} -> catch ff args
    otherResult -> otherResult
catch (YComb _ body) args = catch body args
catch (Catch _ body) args = catch body args

normalise :: Term info -> Term info
normalise num@(Numeral {}) = num
normalise err@(Error {}) = err
normalise var@(Variable {}) = var
normalise (Lambda inf label typ body) = Lambda inf label typ (normalise body)
normalise (Apply inf lhs rhs) = do
  let lReduced = normalise lhs
  let rReduced = normalise rhs
  case lReduced of
    (Lambda _ label _ body) -> substitute label rReduced body
    _ -> Apply inf lReduced rReduced
normalise (Succ inf term) = Succ inf $ normalise term
normalise (Pred inf term) = Pred inf $ normalise term
normalise (If0 inf cond tt ff) = If0 inf (normalise cond) (normalise tt) (normalise ff)
normalise (Catch inf body) = Catch inf (normalise body)
normalise (YComb inf body) = YComb inf (normalise body)

substitute :: Label -> Term info -> Term info -> Term info
substitute _ _ literal@Numeral {} = literal
substitute old new var@(Variable _ label)
  | label == old = new
  | otherwise = var
substitute old new abst@(Lambda inf label t body)
  | label == old = abst
  | otherwise =
      let freshVar = fresh (used new ++ used body ++ [old, label])
       in Lambda inf freshVar t (substitute old new (rename label freshVar body))
substitute old new (Apply inf lhs rhs) =
  Apply inf (substitute old new lhs) (substitute old new rhs)
substitute old new (Succ inf body) = Succ inf (substitute old new body)
substitute old new (Pred inf body) = Pred inf (substitute old new body)
substitute old new (If0 inf cond iftrue iffalse) =
  let sub = substitute old new
   in (If0 inf (sub cond) (sub iftrue) (sub iffalse))
substitute old new (YComb inf body) = YComb inf (substitute old new body)
substitute _ _ err@(Error {}) = err
substitute old new (Catch inf body) = Catch inf (substitute old new body)

rename :: Label -> Label -> Term info -> Term info
rename _ _ literal@(Numeral {}) = literal
rename old new var@(Variable inf label)
  | label == old = Variable inf new
  | otherwise = var
rename old new abst@(Lambda inf label t body)
  | label == old = abst
  | otherwise = Lambda inf label t (rename old new body)
rename old new (Apply inf lhs rhs) = Apply inf (rename old new lhs) (rename old new rhs)
rename old new (Succ inf body) = Succ inf (rename old new body)
rename old new (Pred inf body) = Pred inf (rename old new body)
rename old new (If0 inf cond iftrue iffalse) =
  let r = rename old new
   in If0 inf (r cond) (r iftrue) (r iffalse)
rename old new (YComb inf body) = YComb inf (rename old new body)
rename _ _ err@(Error {}) = err
rename old new (Catch inf body) = Catch inf (rename old new body)

fresh :: [Label] -> Label
fresh usedLabels = case (find (`notElem` usedLabels) labels) of
  Just label -> label
  Nothing -> error "Error, somehow all variable names have been used. This shouldn't be possible."

used :: Term info -> [Label]
used (Numeral _ _) = []
used (Variable _ label) = [label]
used (Lambda _ label _ term) = label : used term
used (Apply _ lhs rhs) = used lhs ++ used rhs
used (Succ _ body) = used body
used (Pred _ body) = used body
used (If0 _ cond iftrue iffalse) = used cond ++ used iftrue ++ used iffalse
used (YComb _ body) = used body
used (Error {}) = []
used (Catch _ term) = used term

labels :: [Label]
labels = alphabetLazyList ++ alphaNumsLazyList

alphabetLazyList :: [String]
alphabetLazyList = [[char] | char <- ['a' .. 'z']]

alphaNumsLazyList :: [String]
alphaNumsLazyList = [char : show i | i <- [1 :: Int ..], char <- ['a' .. 'z']]

lastElemIndex :: (Eq a) => a -> [a] -> Maybe Int
lastElemIndex toFind elems =
  let reversedIndex = elemIndex toFind (reverse elems)
   in fmap (\i -> (length elems - i - 1)) reversedIndex