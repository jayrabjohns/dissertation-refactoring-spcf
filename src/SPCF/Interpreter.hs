module SPCF.Interpreter where

import Control.Monad.State (MonadState (get, put), State, evalState)
import SPCF.AST (Label, Term)
import SPCF.Evaluation (eval, runEval)
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

interpProg :: Prog info -> [Result (Term info)]
interpProg = (`evalState` empty) . interpretStatements . prog_of

interpretStatements :: [Statement info] -> EvalMany (Term info)
interpretStatements statements = do
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