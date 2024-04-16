module SPCF.Interpreter where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State (MonadState (get, put), State, evalState, evalStateT)
import Control.Monad.Writer( WriterT(runWriterT), MonadWriter(tell) )
import SPCF.AST (Label, Term, Type)
import SPCF.Evaluation (eval, runEval)
import SPCF.Types
import Utils.Environment (Environment, empty, insert)

data Statement info
  = Declare info Label (Term info)
  | Evaluate info (Term info)

data Prog info = Prog
  { pinfo_of :: info,
    prog_of :: [Statement info]
  }

type Result a = Either String a

type EvalMany a = State (Environment a) [Result a]

interpretProg :: Prog info -> [Result (Term info)]
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
  let evalResult = fst $ runEval (eval term) env
  return [evalResult]

typecheckProg :: Prog info -> (Result [Type], [String])
typecheckProg (Prog _ statements) = do
  let types = traverse typecheckStatement statements
  let results = runIdentity $ runWriterT $ runExceptT $ (`evalStateT` empty) types
  results

typecheckStatement :: Statement info -> Judgement Type
typecheckStatement (Declare _ label term) = do
  tell ["", "Beginning type judgement for the term: " ++ show term]
  termType <- typeof term
  env <- get
  put $ insert label termType env
  return termType
typecheckStatement (Evaluate _ term) = typeof term
