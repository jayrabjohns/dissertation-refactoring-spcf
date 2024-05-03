module SPCF.Evaluation where

import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (MonadReader (ask, local), ReaderT (runReaderT))
import Control.Monad.Writer (MonadWriter (tell), WriterT (runWriterT))
import SPCF.AST (Label, Term (..))
import SPCF.TermManipulation (lastElemIndex, normalise, substitute)
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
eval term@(Bottom {}) = return term
eval term@(Top {}) = return term
eval lit@(Numeral {}) = return lit
eval err@(Error {}) = return err
eval term@(Lst _ _) = return term
eval (Variable _ name) = do
  env <- ask
  tell
    [ "\nEvaluating "
        ++ name
        ++ " with environement:\n"
        ++ show env
    ]
  case lookup name env of
    Just val -> return val
    Nothing -> throwError $ "Undefined variable " ++ name
eval (Lambda inf name typ body) = return $ Lambda inf name typ (normalise body)
eval (Apply _ lterm rterm) = do
  -- Call by value because evaluating argument before application
  arg <- eval rterm
  lval <- eval lterm
  case arg of
    top@(Top {}) -> return top
    bot@(Bottom {}) -> return bot
    _ -> case lval of
      Lambda _ name _ body -> do
        env <- ask
        let newEnv = insert name arg env
        let newBody = substitute name arg body
        tell
          [ "Apply ("
              ++ (show lval ++ ") [" ++ show arg ++ "/" ++ show name ++ "] ")
              -- ++ ("with new environment\n" ++ show env)
          ]
        local (const newEnv) (eval newBody)
      err@(Error {}) -> return err
      top@(Top {}) -> return top
      bot@(Bottom {}) -> return bot
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
    err@(Error {}) -> return err
    top@(Top {}) -> return top
    bot@(Bottom {}) -> return bot
    _ -> throwError $ "Cannot apply successor to non natural number" ++ show term
eval (Pred _ term) = do
  tell ["\nFinding predecessor of " ++ show term]
  val <- eval term
  case val of
    Numeral inf 0 -> return $ Bottom inf
    Numeral inf i -> return $ Numeral inf (i - 1)
    Error inf err -> return $ Error inf err
    Bottom inf -> return $ Bottom inf
    Top inf -> return $ Top inf
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
    Top inf -> return $ Top inf
    Bottom inf -> return $ Bottom inf
    _ ->
      throwError $
        "Cannot check if non numerical value is 0. Specifically, "
          ++ (show cond ++ " doesn't evaluate to a number.")
eval (YComb inf term) = do
  val <- eval term
  case val of
    abst@(Lambda _ name _ body) -> do
      let selfApplication = substitute name (YComb inf abst) body
      eval selfApplication
    err@(Error {}) -> return err
    top@(Top {}) -> return top
    bot@(Bottom {}) -> return bot
    _ -> throwError "Error, it is only possible to take a fixed point of a lambda abstraction"
eval (Catch inf body) = do
  case catch body [] of
    Constant (Numeral _ i) n -> return $ Numeral inf (i + n)
    Constant (Error _ _) n -> return $ Numeral inf n
    Constant term _ ->
      throwError $
        "Catch has been implemented incorrectly, this case should not exist. "
          ++ "It has returned the following term as a constant "
          ++ show term
    ArgumentIndex i -> return $ Numeral inf i
    Diverge msg -> throwError msg
eval (Pair inf lhs rhs) = do
  lEval <- eval lhs
  rEval <- eval rhs
  return $ Pair inf lEval rEval
eval (Fst _ term) = do
  body <- eval term
  tell ["snd(" ++ show term ++ ")"]
  case body of
    (Pair _ fstElem _) -> return fstElem
    wrongTerm ->
      throwError $
        "Can only take the first projection of products. "
          ++ ("Fst is undefined for the term " ++ show wrongTerm)
eval (Snd _ term) = do
  body <- eval term
  tell ["snd(" ++ show term ++ ")"]
  case body of
    (Pair _ _ sndElem) -> return sndElem
    wrongTerm ->
      throwError $
        "Can only take the second projection of products. "
          ++ ("Snd is undefined for the term " ++ show wrongTerm)

data CatchResult info
  = Constant (Term info) Int
  | ArgumentIndex Int
  | Diverge String

catch :: Term info -> [Label] -> CatchResult info
catch term@(Bottom _) args = Constant term (length args)
catch term@(Top _) args = Constant term (length args)
catch i@(Numeral _ _) args = Constant i (length args)
catch err@(Error _ _) args = Constant err (length args)
catch term@(Lst _ _) args = Constant term (length args)
catch (Variable _ name) args = case lastElemIndex name args of
  Just i -> ArgumentIndex i
  Nothing ->
    Diverge $
      "Error, variable "
        ++ name
        ++ " is unbound and so cannot be caught. This program shouldn't "
        ++ "have correctly typed checked in the first place."
catch (Lambda _ name _ body) args = catch body (args ++ [name]) -- TODO: ensure variable shadowing works as expected
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
catch (Pair _ lhs rhs) args = do
  case catch lhs args of
    Constant {} -> catch rhs args
    otherResult -> otherResult
catch (Fst _ term) args = do
  case normalise term of
    (Pair _ fstElem _) -> catch fstElem args
    wrongTerm ->
      Diverge $
        "Can only take the first projection of products of length >= 1. "
          ++ ("Snd is undefined for the term " ++ show wrongTerm)
catch (Snd _ term) args = do
  case normalise term of
    (Pair _ _ sndElem) -> catch sndElem args
    wrongTerm ->
      Diverge $
        "Can only take the second projection of products of length >= 2. "
          ++ ("Snd is undefined for the term " ++ show wrongTerm)

label' :: Term info -> Either (Term info) (Term info)
label' (Lambda _ name _ body) = label body name
label' wrongTerm =
  error $
    "Label is ony defined on lambda abstractions, "
      ++ ("not the term" ++ show wrongTerm)

label :: Term info -> Label -> Either (Term info) (Term info)
label term@(Bottom _) _ = Left term
label term@(Top _) _ = Left term
label term@(Numeral _ _) _ = Left term
label term@(Error _ _) _ = Left term
label term@(Lst _ _) _ = Left term
label term@(Variable {}) _ = Left term
label (Lambda _ _ _ body) name = label body name
label (Apply _ lhs rhs) name =
  case (lhs, rhs) of
    ((Variable _ varName), (Numeral {})) ->
      if name == varName then Right rhs else undefined
    ((Variable _ varName), wrongTerm) ->
      if name == varName
        then error "can only apply to nats"
        else label wrongTerm name
    _ -> case label rhs name of
      Left {} -> label lhs name
      otherResult -> otherResult
label (Succ _ body) args = label body args
label (Pred _ body) args = label body args
label (If0 _ predicate tt ff) args =
  case label predicate args of
    Left (Numeral _ 0) -> label tt args
    Left (Numeral {}) -> label ff args
    otherResult -> otherResult
label (YComb _ body) args = label body args
label (Catch _ body) args = label body args
label (Pair _ lhs rhs) args = do
  case label lhs args of
    Left {} -> label rhs args
    otherResult -> otherResult
label (Fst _ term) args = do
  case normalise term of
    (Pair _ fstElem _) -> label fstElem args
    wrongTerm ->
      error $
        "Can only take the second projection of products of length >= 2. "
          ++ ("Snd is undefined for the term " ++ show wrongTerm)
label (Snd _ term) args = do
  case normalise term of
    (Pair _ _ sndElem) -> label sndElem args
    wrongTerm ->
      error $
        "Can only take the second projection of products of length >= 2. "
          ++ ("Snd is undefined for the term " ++ show wrongTerm)