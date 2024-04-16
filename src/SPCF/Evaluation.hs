module SPCF.Evaluation where

import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (MonadReader (ask, local), ReaderT (runReaderT))
import Control.Monad.Writer (MonadWriter (tell), WriterT (runWriterT))
import SPCF.AST (Label, Term (..))
import SPCF.TermManipulation( normalise, substitute, lastElemIndex )
import Utils.Environment (Environment, empty, insert, labels, lookup)
import Prelude hiding (lookup)

type Eval a = (ReaderT (Environment a) (ExceptT String (WriterT [String] Identity))) a

runEval :: Eval a -> Environment a -> (Either String a, [String])
runEval evl env = runIdentity . runWriterT . runExceptT $ runReaderT evl env

runEvalIO :: Eval a -> Environment a -> IO a
runEvalIO evaluation env = do
  let (result, logs) = runEval evaluation env
  _ <- traverse putStrLn logs
  either fail return result

interpret :: Term info -> Either String (Term info)
interpret term = fst $ runEval (eval term) empty

interpretIO :: Term info -> IO (Term info)
interpretIO term = runEvalIO (eval term) empty

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
        ++ show env
    ]
  case lookup label env of
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
      let newEnv = insert label arg env
      let newBody = substitute label arg body
      tell
        [ "Apply ("
            ++ (show lval ++ ") [" ++ show arg ++ "/" ++ show label ++ "] ")
            ++ ("with new environment\n" ++ show env)
        ]
      local (const newEnv) (eval newBody)
    _ ->
      throwError $
        "Error while evaluating application - "
          ++ "lhs of an application should always be an abstraction. "
          ++ ("Specifically, " ++ show rterm)
          ++ (" cannot be applied to " ++ show lterm)
eval (Succ _ term) = do
  tell ["\nFinding successor of " ++ show term]
  val <- eval term
  case val of
    Numeral inf i -> return $ Numeral inf (i + 1)
    _ -> throwError $ "Cannot apply successor to non natural number" ++ show term
eval (Pred _ term) = do
  tell ["\nFinding predecessor of " ++ show term]
  val <- eval term
  case val of
    Numeral inf 0 -> return $ Numeral inf 0
    Numeral inf i -> return $ Numeral inf (i - 1)
    _ -> throwError $ "Cannot apply predeccessor to non natural number " ++ show term
eval (If0 _ cond iftrue iffalse) = do
  tell
    [ ("if " ++ show cond)
        ++ ("\n then " ++ show iftrue)
        ++ ("\n else " ++ show iffalse)
    ]
  val <- eval cond
  case val of
    Numeral _ 0 -> eval iftrue
    Numeral {} -> eval iffalse
    err@(Error {}) -> return err
    _ ->
      throwError $
        "Cannot check if non numerical value is 0. Specifically, "
          ++ (show cond ++ " doesn't evaluate to a number.")
eval (YComb inf term) = do
  val <- eval term
  case val of
    abst@(Lambda _ label _ body) -> do
      let selfApplication = substitute label (YComb inf abst) body
      eval selfApplication
    _ -> throwError "Error, it is only possible to take a fixed point of a lambda abstraction"
-- Following Laird's definition of catch which has ground type rather than
--   Cartwreight & Fallensien's family of t -> o typed operators.
eval (Catch inf body) = do
  env <- ask
  let usedLabels = labels env
  case catch body usedLabels of
    Constant (Numeral _ i) n -> return $ Numeral inf (i + n)
    Constant (Error _ _) n -> return $ Numeral inf n
    Constant term _ ->
      throwError $
        "Catch has been implemented incorrectly, this case should not exist. "
          ++ "It has returned the following term as a constant "
          ++ show term
    ArgumentIndex i -> return $ Numeral inf i
    Diverge msg -> throwError msg

data CatchResult info
  = Constant (Term info) Int
  | ArgumentIndex Int
  | Diverge String

catch :: Term info -> [Label] -> CatchResult info
catch i@(Numeral _ _) args = Constant i (length args)
catch err@(Error _ _) args = Constant err (length args)
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
    Constant (Numeral _ 0) _ -> catch tt args
    Constant (Numeral {}) _ -> catch ff args
    otherResult -> otherResult
catch (YComb _ body) args = catch body args
catch (Catch _ body) args = catch body args