module BoundedSPCFTypes where

import BoundedSPCF
import Control.Monad.Identity
import Control.Monad.Reader
import Data.List
import qualified Data.Map as Map
import Debug.Trace

emptyContext :: Context
emptyContext = Map.empty

type Context = Map.Map Label Type

type Judgement a = (ReaderT Context Identity) a

runJudgement :: Judgement a -> Context -> a
runJudgement judgement context = runIdentity $ runReaderT judgement context

typeof :: Term -> Judgement Type
typeof (Numeral _) = return Base
typeof (Variable label) = do
  context <- ask
  case Map.lookup label context of
    Just val -> return val
    Nothing -> error $ "Undefined variable " ++ label
typeof (Lambda label varTyp body) = do
  context <- ask
  let newContext = Map.insert label varTyp context
  bodType <- local (const newContext) (typeof body)
  return $ varTyp :-> bodType
typeof t@(Apply l r) = do
  lTyp <- typeof l
  rTyp <- typeof r
  case lTyp of
    f :-> _ -> if f == rTyp then return rTyp else trace ("\nrhs: " ++ show rTyp) error $ "lhs " ++ show f ++ " cannot be applied with rhs " ++ show rTyp ++ "\n  term: " ++ show t
    _ -> error "lhs is variable or constant"
typeof (Succ body) = typeof body
typeof (Product (fstT : sndT : xs)) = do
  types <- traverse typeof xs
  fstType <- typeof fstT
  sndType <- typeof sndT
  return $ foldl' (Cross) (Cross fstType sndType) types
typeof (Product []) = return Unit
typeof p@(Product _) = error $ "Error, cannot construct a product with fewer than 2 elements\n" ++ show p
typeof (Case (Numeral n) p@(Product _)) = do
  typeof (projection p n)
typeof c@(Case _ _) =
  error $
    "Error, Case<n, p> must be of the form "
      ++ "<numeral, product> but "
      ++ show c
      ++ " was given"
typeof (Catch _) = return Base