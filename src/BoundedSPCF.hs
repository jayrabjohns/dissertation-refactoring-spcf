module BoundedSPCF (Term (..), Type(..), Value (..), Label, Environment, Numeral, Product, Eval, emptyEnv, emptyProduct, projection, insertProduct, removeProduct, numerals, upperBound, eval, normalise, interpret, interpretIO, substitute, runEval, runEvalIO) where

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
projection :: Term -> Int -> Term
projection (Product _) 0 = emptyProduct
projection (Product prod) i = Data.List.genericIndex prod (i - 1)
projection term i = Case (Numeral i) term

-- projection term _ = error $ "Cannot project term " ++ show term

-- Insert element into 0 indexed n-fold product at a given position
insertProduct :: Product -> Term -> Int -> Product
insertProduct prod toInsert index =
  let (before, after) = Data.List.splitAt index prod
   in before ++ [toInsert] ++ after

-- Remove element from 0 indexed n-fold product at a given position
removeProduct :: Product -> Int -> Product
removeProduct prod index =
  let (before, after) = Data.List.splitAt (index + 1) prod
   in (take index before) ++ after

upperBound :: Numeral
upperBound = 2

numerals :: [Numeral]
numerals = [0 :: Numeral .. upperBound]

data Term
  = Numeral Numeral
  | Variable Label
  | Lambda Label Type Term
  | Apply Term Term
  | Succ Term
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
      beautify i (Apply lhs@(Lambda label _ _) rhs) = if i == 2 then "(" ++ s ++ ")" else s where s = beautify 2 lhs ++ "[" ++ show rhs ++ "/" ++ show label ++ "]" -- beautify 1 lhs ++ " " ++ beautify 2 rhs
      beautify i (Apply lhs rhs) = if i == 2 then "(" ++ s ++ ")" else s where s = beautify 1 lhs ++ " " ++ beautify 2 rhs
      beautify i (Succ term) = if i /= 0 then "(" ++ s ++ ")" else s where s = "Succ " ++ beautify 2 term
      beautify i (Product []) = if i /= 0 then "(" ++ s ++ ")" else s where s = "I"
      beautify _ (Product prod) = "<" ++ (intercalate ", " (map (beautify 2) prod)) ++ ">"
      beautify i (Case numeral prod) = if i /= 0 then "(" ++ s ++ ")" else s where s = "Case <" ++ beautify 0 numeral ++ ", " ++ beautify 0 prod ++ ">"
      beautify i (Catch term) = if i /= 0 then "(" ++ s ++ ")" else s where s = "Catch " ++ beautify 2 term

data Type
  = Base -- Base type (numerals)
  | Empty -- Non-terminating function
  | (:->) Type Type -- Procedure
  | Unit -- Type of the empty product
  | Cross Type Type -- Binary product
  deriving (Eq)

-- When constructing types with :-> we typically want them to associate right
-- o -> o -> o == o -> (o -> o) 
-- rather than
-- o -> o -> o == (o -> o) -> o
infixr 5 :->

-- instance Eq Type where
--   (==) = equal 
--     where 
--       equal :: Type -> Type -> Bool
--       equal Base Base = True
--       equal Empty Empty = True
--       equal Unit Unit = True
--       equal (t1 :-> t2) (t3 :-> t4) = (equal t1 t3) && (equal t2 t4)
--       equal (t1 `Cross` t2) (t3 `Cross` t4) = (equal t1 t3) && (equal t2 t4)
--       equal _ _ = False
  

instance Show Type where
  show = beautify 0
    where
      beautify :: Int -> Type -> String
      beautify _ Base = "o"
      beautify _ (Base :-> rhs) = "o -> " ++ beautify 0 rhs
      beautify _ (lhs :-> rhs) = beautify 0 lhs ++ " -> " ++ beautify 0 rhs
      beautify i (Cross lhs rhs) = if i == 0 then "(" ++ s ++ ")" else s where s = beautify 1 lhs ++ "x" ++ beautify 1 rhs
      beautify _ Unit = "()"
      beautify _ Empty = "0"

-- A mapping of identifiers (labels) to closures. The closure to which an
--   environment E maps an identifier x is normally denoted by E[x].
type Environment = Map.Map Label Term

emptyEnv :: Environment
emptyEnv = Map.empty

showEnv :: Environment -> String
showEnv env = Map.Debug.showTreeWith (\k v -> show (k, v)) True False env

-- The result of evaluation of a closure.
--   Either a natural number or another closure.
data Value
  = Nat Int
  | Closure Environment Term
  deriving (Eq)

termToValue :: Eval Term -> Eval Value
termToValue evaluation = do
  result <- evaluation
  env <- ask
  return $ case result of
    Numeral i -> Nat i
    term -> Closure env term

instance Show Value where
  show (Nat i) = show i
  show (Closure env term) = "Closure (" ++ show term ++ ") (" ++ show env ++ ")"

type Eval a = (ReaderT Environment (ExceptT String (WriterT [String] Identity))) a

runEval :: Eval a -> Environment -> (Either String a, [String])
runEval evalA env = runIdentity . runWriterT . runExceptT $ runReaderT evalA env

runEvalIO :: Eval a -> Environment -> IO a
runEvalIO evaluation env = do
  let (result, logs) = runEval evaluation env
  _ <- traverse putStrLn logs
  either fail return result

interpret :: Term -> Either String Value
interpret term = fst $ runEval (termToValue $ eval term) emptyEnv

interpretIO :: Term -> IO Value
interpretIO term = runEvalIO (termToValue $ eval term) emptyEnv

-- Evaluation is commonly denoted by ⇓ and is sort of a decomposition of a
--   closure (a redex and an evaluation context) into a value.
--   If the term is of ground type then the result will be a numeral, otherwise
--   it will return a the beta reduced term along with the evaluation
--   context up to that point.
eval :: Term -> Eval Term
eval (Numeral i) = return $ Numeral i
eval (Variable label) = do
  env <- ask
  tell
    [ "Evaluate "
        ++ show label
        ++ " with environement:\n"
        ++ showEnv env
    ]
  case Map.lookup label env of
    Just val -> return val
    Nothing -> throwError $ "Undefined variable " ++ label
eval (Lambda label typ body) = return $ Lambda label typ (normalise body)
eval (Apply lterm rterm) = do
  -- Call by value because evaluating argument before application
  arg <- eval rterm
  lval <- eval lterm
  case lval of
    Lambda label _ body -> do
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
eval (Succ term) = do
  tell ["\nFind successor of " ++ show term]
  val <- eval term
  case val of
    Numeral i ->
      if i < (upperBound - 1)
        then return $ Numeral (i + 1)
        else return $ Numeral i
    _ -> throwError $ "Cannot apply successor to non numeral" ++ show term
eval (Product terms) = fmap Product (traverse eval terms)
eval (Case num prod) = do
  nVal <- eval num
  pVal <- eval prod
  case (nVal, pVal) of
    (Numeral i, p@(Product {})) -> do
      let proj = projection p i
      tell
        [ show (Case num prod)
            ++ " <-> projection of "
            ++ show pVal
            ++ " at i = "
            ++ show nVal
            ++ " <-> "
            ++ show proj
        ]
      eval proj
    (Numeral 1, term) -> do
      tell
        [ show (Case num prod)
            ++ " <-> treat "
            ++ show pVal
            ++ " as a single element product"
        ]
      eval term
    _ ->
      throwError $
        "Error while evaluating "
          ++ show (Case num prod)
          ++ "\n Either "
          ++ show num
          ++ " doesn't reduce to a numeral or "
          ++ show prod
          ++ " doesn't reduce to a product"
eval (Catch body) = do
  env <- ask
  let usedLabels = Map.keys env
  let catchResult = runCatch (catch body) usedLabels
  case catchResult of
    Constant i n -> return $ Numeral (i + n)
    ArgumentIndex i -> return $ Numeral i
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
catch (Succ body) = catch body
catch (Product prod) = findStrict prod
  where
    findStrict :: [Term] -> Catch CatchResult
    findStrict [] = undefined
    findStrict (x : xs) = do
      res <- catch x
      case res of
        i@(ArgumentIndex _) -> return i
        _ -> findStrict xs
catch (Case n prod) = do
  res <- catch n
  case res of
    i@(ArgumentIndex _) -> return i
    _ -> catch prod
catch (Catch body) = catch body

normalise :: Term -> Term
normalise num@(Numeral _) = num
normalise var@(Variable _) = var
normalise (Lambda label typ body) = Lambda label typ (normalise body)
normalise (Apply lhs rhs) = do
  let arg = normalise rhs
  case lhs of
    (Lambda label _ body) -> normalise (substitute label arg body)
    _ -> Apply (normalise lhs) arg
normalise (Succ body) = Succ (normalise body)
normalise (Product prod) = Product (map normalise prod)
normalise (Case n prod) = Case (normalise n) (normalise prod)
normalise (Catch body) = Catch (normalise body)

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
substitute old new (Succ body) = Succ $ substitute old new body
substitute old new (Product prod) = Product $ map (substitute old new) prod
substitute old new (Case n body) =
  Case (substitute old new n) (substitute old new body)
substitute old new (Catch body) = Catch (substitute old new body)

-- substitute :: Label -> Term -> Term -> Term
-- substitute _ _ literal@Numeral {} = literal
-- substitute old new var@(Variable label)
--   | label == old = new
--   | otherwise = var
-- substitute old new abst@(Lambda label t body)
--   | label == old = abst
--   | otherwise =
--       let freshVar = fresh (used new ++ used body ++ [old, label])
--        in Lambda freshVar t (substitute old new (rename label freshVar body))
-- substitute old new (Apply lhs rhs) =
--   Apply (substitute old new lhs) (substitute old new rhs)
-- substitute old new (Product prod) = Product $ map (substitute old new) prod
-- substitute old new (Case n body) =
--   Case (substitute old new n) (substitute old new body)
-- substitute old new (Catch body) = Catch (substitute old new body)

rename :: Label -> Label -> Term -> Term
rename _ _ literal@(Numeral _) = literal
rename old new var@(Variable label)
  | label == old = Variable new
  | otherwise = var
rename old new abst@(Lambda label t body)
  | label == old = abst
  | otherwise = Lambda label t (rename old new body)
rename old new (Apply lhs rhs) = Apply (rename old new lhs) (rename old new rhs)
rename old new (Succ body) = Succ (rename old new body)
rename old new (Product prod) = Product $ map (rename old new) prod
rename old new (Case n body) = Case (rename old new n) (rename old new body)
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
used (Succ body) = used body
used (Product prod) = prod >>= used
used (Case num prod) = used num ++ used prod
used (Catch term) = used term

labels :: [Label]
labels = alphabetLazyList ++ alphaNumsLazyList

alphabetLazyList :: [String]
alphabetLazyList = [[char] | char <- ['a' .. 'z']]

alphaNumsLazyList :: [String]
alphaNumsLazyList = [char : show i | i <- [1 :: Int ..], char <- ['a' .. 'z']]