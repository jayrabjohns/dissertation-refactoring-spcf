module SPCF.Interpreter where

import Control.Monad.Except (runExceptT)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.State (MonadState (get, put), State, evalState, evalStateT)
import Control.Monad.Writer (MonadWriter (tell), WriterT (runWriterT))
import SPCF.AST (Label, Term, Type)
import SPCF.Evaluation (eval, runEval)
import SPCF.Types (Judgement, typeof)
import Utils.Environment (Environment, empty, insert)

data Statement info
  = Declare info Label (Term info)
  | Evaluate info (Term info)

data Program info = Prog
  { pinfo_of :: info,
    prog_of :: [Statement info]
  }

type Result a = Either String a

type EvalMany a = State (Environment a) [(Result a, [String])]

interpretProg :: Program info -> [(Result (Term info), [String])]
interpretProg (Prog _ statements) = (`evalState` empty) $ do
  results <- traverse interpretStatement statements
  return $ concat results

interpretStatement :: Statement info -> EvalMany (Term info)
interpretStatement (Declare _ label term) = do
  env <- get
  put $ insert label term env
  return []
interpretStatement (Evaluate _ term) = do
  env <- get
  let (evalResult, logs) = runEval (eval term) env
  let updatedLogs = ["[Evaluation] -- " ++ show term] ++ logs
  return [(evalResult, updatedLogs)]

typecheckProg :: Program info -> (Result [Type], [String])
typecheckProg (Prog _ statements) = do
  let types = traverse typecheckStatement statements
  let results = runIdentity $ runWriterT $ runExceptT $ (`evalStateT` empty) types
  results

typecheckStatement :: Statement info -> Judgement Type
typecheckStatement (Declare _ label term) = do
  tell ["", "Type judgement for " ++ label ++ " = " ++ show term]
  termType <- typeof term
  env <- get
  put $ insert label termType env
  return termType
typecheckStatement (Evaluate _ term) = do
  tell ["", "Type judgement for eval {" ++ show term ++ "}"]
  typeof term
