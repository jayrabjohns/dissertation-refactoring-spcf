module SPCF.Types where

import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.State (MonadState (get, put), StateT, evalStateT)
import Control.Monad.Writer (MonadWriter (tell), WriterT (runWriterT))
import SPCF.AST (Term (..), Type (..))
import SPCF.TermManipulation (normalise)
import Utils.Environment (Environment, empty, insert, lookup)
import Prelude hiding (lookup)

type Context a = Environment a

type Judgement a = (StateT (Context a) (ExceptT String (WriterT [String] Identity))) a

runJudgement :: Judgement a -> Context a -> (Either String a, [String])
runJudgement judgement initState = runIdentity . runWriterT . runExceptT $ evalStateT judgement initState

typeof' :: Term info -> Type
typeof' term =
  let termType = fst $ runJudgement (typeof term) empty
   in case termType of
        Right typ -> typ
        Left err -> error err

typeof :: Term info -> Judgement Type
typeof term@(Top _) = Empty <$ tell ["[" ++ show term ++ "]: " ++ show Empty]
typeof term@(Bottom _) = Empty <$ tell ["[" ++ show term ++ "]: " ++ show Empty]
typeof term@(Numeral {}) = Base <$ tell ["[" ++ show term ++ "]: " ++ show Base]
typeof term@(Error {}) = Base <$ tell ["[" ++ show term ++ "]: " ++ show Base]
typeof term@(Lst {}) = Base <$ tell ["[" ++ show term ++ "]: " ++ show Base]
typeof (Variable _ label) = do
  context <- get
  case lookup label context of
    Just val -> return val
    Nothing -> throwError $ "Undefined variable " ++ label
typeof f@(Lambda _ label varType body) = do
  context <- get
  put $ insert label varType context
  tell ["[" ++ label ++ "]: " ++ show varType]
  bodType <- typeof body
  let typ = varType :-> bodType
  tell ["[" ++ show f ++ "]: " ++ show typ]
  return typ
typeof term@(Apply _ l r) = do
  argType <- typeof r
  -- tell ["[" ++ show r ++ "]: " ++ show argType]
  funcType <- typeof l
  -- tell ["[" ++ show l ++ "]: " ++ show funcType]
  case funcType of
    (x :-> y) ->
      if x == argType
        then y <$ tell ["[" ++ show term ++ "]: " ++ show y]
        else
          throwError
            ( "Cannot type "
                ++ (show term ++ " because the lhs " ++ show x)
                ++ (" cannot be applied with rhs " ++ show argType)
            )
    _ -> throwError ""
typeof term@(Succ _ body) = do
  typ <- typeof body
  typ <$ tell ["[" ++ show term ++ "]: " ++ show typ]
typeof term@(Pred _ body) = do
  typ <- typeof body
  typ <$ tell ["[" ++ show term ++ "]: " ++ show typ]
typeof term@(If0 _ cond tt ff) = do
  condType <- typeof cond
  trueType <- typeof tt
  falseType <- typeof ff
  case (condType, trueType, falseType) of
    (Base, Base, Base) -> Base <$ tell ["[" ++ show term ++ "]: " ++ show Base]
    wrongType -> throwError $ "Cannot typecheck " ++ show term ++ ". Type calculated " ++ show wrongType
typeof term@(YComb _ body) = do
  bodyType <- typeof body
  case bodyType of
    (_ :-> returnType) -> returnType <$ tell ["[" ++ show term ++ "]: " ++ show returnType]
    wrongType -> throwError $ "Cannot typecheck " ++ show term ++ ". Type calculated " ++ show wrongType
typeof term@(Iter _ body) = do
  bodyType <- typeof body
  case bodyType of
    (_ :-> returnType) -> (returnType :-> Empty) <$ tell ["[" ++ show term ++ "]: " ++ show returnType]
    wrongType -> throwError $ "Cannot typecheck " ++ show term ++ ". Type calculated " ++ show wrongType
typeof term@(Catch _ body) = do
  bodyType <- typeof body
  case bodyType of
    (:->) _ _ -> Base <$ tell ["[" ++ show term ++ "]: " ++ show Base]
    wrongType -> throwError $ "Cannot typecheck " ++ show term ++ ". Type calculated " ++ show wrongType
typeof term@(Pair _ lhs rhs) = do
  leftType <- typeof lhs
  rightType <- typeof rhs
  case (leftType, rightType) of
    (Base, Base) -> Base <$ tell ["[" ++ show term ++ "]: " ++ show Base]
    wrongType -> throwError $ "Cannot typecheck " ++ show term ++ ". Type calculated " ++ show wrongType
typeof term@(Fst _ body) =
  case normalise body of
    pair@(Pair _ _ _) -> do
      pairType <- typeof pair
      case pairType of
        Base -> Base <$ tell ["[" ++ show term ++ "]: " ++ show Base]
        wrongType -> error $ "Cannot take π1 on the type " ++ show wrongType
    wrongType -> error $ "Cannot take π1 on the type " ++ show wrongType
typeof term@(Snd _ body) =
  case normalise body of
    pair@(Pair _ _ _) -> do
      pairType <- typeof pair
      case pairType of
        Base -> Base <$ tell ["[" ++ show term ++ "]: " ++ show Base]
        wrongType -> error $ "Cannot take π2 on the type " ++ show wrongType
    wrongType -> error $ "Cannot take π2 on the type " ++ show wrongType