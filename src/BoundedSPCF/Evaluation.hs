module BoundedSPCF.Evaluation where

import BoundedSPCF.AST (Label, Term (..), projection, upperBound)
import BoundedSPCF.TermManipulation (normalise, substitute)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (MonadReader (ask, local), ReaderT (runReaderT))
import Control.Monad.Writer (MonadWriter (tell), WriterT (runWriterT))
import Data.List (elemIndex)
import Utils.Environment (Environment, empty, insert, labels, lookup)
import Prelude hiding (lookup)

type Eval a = (ReaderT (Environment a) (ExceptT String (WriterT [String] Identity))) a

runEval :: Eval a -> Environment a -> (Either String a, [String])
runEval evalA env = runIdentity . runWriterT . runExceptT $ runReaderT evalA env

runEvalIO :: (Show a) => Eval a -> Environment a -> IO a
runEvalIO evaluation env = do
  let (resultEither, logs) = runEval evaluation env
  _ <- putStrLn ""
  _ <- putStrLn "Starting new evaluation"
  _ <- traverse putStrLn logs
  -- _ <- traverse (\(i, lg) -> putStrLn $ show i ++ " | " ++ lg) (zip [i | i <- [0 .. (length logs)]] logs)
  result <- either fail return resultEither
  _ <- putStrLn $ "Final result: " ++ show result
  _ <- putStrLn ""
  return result

interpret :: Term -> Either String Term
interpret term = fst $ runEval (eval term) empty

interpretIO :: Term -> IO Term
interpretIO term = runEvalIO (eval term) empty

-- Evaluation is commonly denoted by ⇓ and is sort of a decomposition of a
--   closure (a redex and an evaluation context) into a value.
--   If the term is of ground type then the result will be a numeral, otherwise
--   it will return a the beta reduced term along with the evaluation
--   context up to that point.
eval :: Term -> Eval Term
eval Bottom = return Bottom
eval Top = return Top
eval (Numeral i) = return $ Numeral i
eval (Variable label) = do
  env <- ask
  tell
    [ "Evaluate "
        ++ show label
        ++ " with environement:\n"
        ++ show env
    ]
  case lookup label env of
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
      let newEnv = insert label arg env
      let newBody = substitute label arg body
      tell
        [ "Apply ("
            ++ show lval
            ++ ") ["
            ++ show arg
            ++ "/"
            ++ show label
            ++ "] with new environment\n"
            ++ show env
        ]
      local (const newEnv) (eval newBody)
    _ ->
      throwError $
        "Error while evaluating application - "
          ++ "the lhs of an application should always be an abstraction. "
          ++ ("Specifically, " ++ show rterm)
          ++ (" cannot be applied to " ++ show lterm)
eval (Succ term) = do
  tell ["\nFind successor of " ++ show term]
  val <- eval term
  case val of
    Numeral i ->
      if i < (upperBound)
        then return $ Numeral (i + 1)
        else return $ Numeral i
    _ -> throwError $ "Cannot apply successor to non numeral" ++ show term
eval (Product terms) = fmap Product (traverse eval terms)
eval (BinProduct lhs rhs) = do
  lEval <- eval lhs
  rEval <- eval rhs
  return $ BinProduct lEval rEval
eval (Fst term) = do
  body <- eval term
  tell ["π1(" ++ show term ++ ")"]
  case body of
    (BinProduct fstElem _) -> return fstElem
    (Product (fstElem : _)) -> return fstElem
    wrongTerm ->
      throwError $
        "Can only take the first projection of products of length >= 1. "
          ++ ("Fst is undefined for the term " ++ show wrongTerm)
eval (Snd term) = do
  body <- eval term
  tell ["π2(" ++ show term ++ ")"]
  case body of
    (BinProduct _ sndElem) -> return sndElem
    (Product (_ : sndElem : _)) -> return sndElem
    wrongTerm ->
      throwError $
        "Can only take the second projection of products of length >= 2. "
          ++ ("Snd is undefined for the term " ++ show wrongTerm)
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
    (Numeral 0, term) -> do
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
  let usedLabels = labels env
  let catchResult = runCatch (catch body) usedLabels
  result <- case catchResult of
    Constant (Numeral i) n -> return $ Numeral (i + n)
    Constant Bottom _ -> return Bottom
    Constant term _ ->
      throwError $
        "This case should not exist. "
          ++ "Catch has returned the following term as a constant "
          ++ show term
    ArgumentIndex i -> return $ Numeral i
    Diverge msg -> throwError msg

  tell [show (Catch body) ++ " <-> term is strict at position " ++ show result]
  return result

type Catch a = ReaderT [Label] Identity a

runCatch :: Catch a -> [Label] -> a
runCatch ctch args = runIdentity $ runReaderT ctch args

data CatchResult
  = Constant Term Int
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
catch Bottom = do
  args <- ask
  return $ Constant Bottom (length args)
catch Top = do
  args <- ask
  return $ Constant Top (length args)
catch i@(Numeral _) = do
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
catch (BinProduct lhs rhs) = do
  leftResult <- catch lhs
  case leftResult of
    Constant {} -> catch rhs
    otherResult -> return otherResult
catch (Fst term) = do
  let body = normalise term
  case body of
    (BinProduct fstElem _) -> catch fstElem
    (Product (fstElem : _)) -> catch fstElem
    wrongTerm ->
      return $
        Diverge $
          "Can only take the second projection of products of length >= 2. "
            ++ ("Snd is undefined for the term " ++ show wrongTerm)
catch (Snd term) = do
  let body = normalise term
  case body of
    (BinProduct _ sndElem) -> catch sndElem
    (Product (_ : sndElem : _)) -> catch sndElem
    wrongTerm ->
      return $
        Diverge $
          "Can only take the second projection of products of length >= 2. "
            ++ ("Snd is undefined for the term " ++ show wrongTerm)
catch (Case n prod) = do
  res <- catch n
  case res of
    i@(ArgumentIndex _) -> return i
    _ -> catch prod
catch (Catch body) = catch body