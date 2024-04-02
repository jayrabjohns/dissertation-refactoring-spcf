module BoundedSPCFTypes where

import BoundedSPCF
import Control.Monad.Identity
import Control.Monad.Reader
import qualified Data.Map as Map
import Debug.Trace

emptyContext :: Context
emptyContext = Map.empty

type Context = Map.Map Label (Eval Type)

type Judgement a = (ReaderT Context Identity) a

runJudgement :: Judgement a -> Context -> a
runJudgement judgement context = runIdentity $ runReaderT judgement context

typeof :: Term -> Judgement (Eval Type)
typeof (Numeral _) = return $ pure Base
typeof (Variable label) = do
  context <- ask
  case Map.lookup label context of
    Just val -> return val
    Nothing -> error $ "Undefined variable " ++ label
typeof (Lambda label varTyp body) = do
  context <- ask
  let newContext = Map.insert label (pure varTyp) context
  typeEval <- local (const newContext) (typeof body)
  return $ do
    bodyType <- typeEval
    return $ varTyp :-> bodyType
-- return $ pure $ varTyp :-> bodType
typeof t@(Apply l r) = do
  lhs <- typeof l
  rhs <- typeof r
  return $ do
    lType <- lhs
    rType <- rhs
    case lType of
      f :-> _ ->
        if f == rType
          then return rType
          else trace ("\nrhs: " ++ show rType) error $ "lhs " ++ show f ++ " cannot be applied with rhs " ++ show rType ++ "\n  term: " ++ show t
      _ -> error "lhs is variable or constant"
typeof (Succ body) = typeof body
-- typeof (Product (fstT : sndT : xs)) = do
-- types <- traverse typeof xs
-- fstType <- typeof fstT
-- sndType <- typeof sndT
-- return do $
-- return $ foldl' (Cross) (Cross fstType sndType) types
typeof (Product []) = return $ pure Unit
typeof (Product terms) = do
  typeEvals <- traverse typeof terms
  return $ do
    types <- sequence typeEvals
    return $ foldr1 Cross types

-- return $ foldl' (Cross) (Cross fstType sndType) types
-- typeof p@(Product _) = error $ "Error, cannot construct a product with fewer than 2 elements\n" ++ show p
typeof c@(Case n p) = do
  context <- ask
  return $ do
    evalN <- eval n
    evalP <- eval p
    let projAtN =
          case (evalN, evalP) of
            (Numeral i, prod@(Product _)) -> projection prod i
            _ ->
              error $
                "Error, Case<n, p> must be of the form "
                  ++ "<numeral, product> but "
                  ++ show c
                  ++ " was given"
    runJudgement (typeof projAtN) context

-- do
--   nTerm <- eval n
--   x <- case nTerm of
--     Numeral i -> pure $ typeof (projection p i)
--     _ -> error ""
--   x
-- typeof c@(Case _ _) =
--   error $
--     "Error, Case<n, p> must be of the form "
--       ++ "<numeral, product> but "
--       ++ show c
--       ++ " was given"
typeof (Catch _) = return $ pure Base