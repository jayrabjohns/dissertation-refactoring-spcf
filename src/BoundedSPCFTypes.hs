module BoundedSPCFTypes where

import BoundedSPCF (Label, Term (..), Type (..))
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (MonadReader (ask, local), ReaderT (runReaderT))
import qualified Data.Map as Map

type Context = Map.Map Label Type

emptyContext :: Context
emptyContext = Map.empty

type Judgement a = (ReaderT Context Identity) a

typeof' :: Term -> Type
typeof' term = runJudgement (typeof term) emptyContext

runJudgement :: Judgement a -> Context -> a
runJudgement judgement context = runIdentity $ runReaderT judgement context

typeof :: Term -> Judgement Type
typeof (Numeral _) = return Nat
typeof (Variable label) = do
  context <- ask
  case Map.lookup label context of
    Just val -> return val
    Nothing -> error $ "Undefined variable " ++ label
typeof (Lambda label varType body) = do
  context <- ask
  let newContext = Map.insert label varType context
  bodType <- local (const newContext) (typeof body)
  return $ varType :-> bodType
typeof (Apply l r) = do
  lType <- typeof l
  rType <- typeof r
  case lType of
    x :-> _ ->
      if x == rType
        then return rType
        else error $ "lhs " ++ show x ++ " cannot be applied with rhs " ++ show rType
    _ -> error ""
typeof (Succ body) = typeof body
typeof (Product []) = return Unit
typeof (Product terms) = do
  types <- traverse typeof terms
  let allTypesAreEqual = and $ map (== head types) (tail types)
  if allTypesAreEqual
    then return $ foldl1 Cross types
    else error "Elements of a product must all be the same type."
-- return $ foldl' (Cross) (Cross fstType sndType) types
-- typeof p@(Product _) = error $ "Error, cannot construct a product with fewer than 2 elements\n" ++ show p
typeof (Case n p) = do
  numType <- typeof n
  prodType <- typeof p
  case (numType, prodType) of
    (Nat, (Cross _ elemType)) -> return $ elemType
    (wrongType, (Cross {})) -> error $ "the term Case<T1, T2> must have a Base type in place of T1, instead it is a " ++ show wrongType
    (_, wrongType) -> error $ "the term Case<T1, T2> must have a Cross type in place of T2, instead it is a " ++ show wrongType
typeof (Catch body) = do
  bodyType <- typeof body
  case bodyType of
    (:->) _ _ -> return Nat
    wrongType -> error $ "Catch is defined on types of the form T1 :-> T2 :-> ..., but instead it was given the type " ++ show wrongType