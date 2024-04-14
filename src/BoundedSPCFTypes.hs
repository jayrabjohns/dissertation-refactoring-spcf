module BoundedSPCFTypes where

import BoundedSPCF (Label, Term (..), Type (..))
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (MonadReader (ask, local), ReaderT (runReaderT))
import qualified Data.Map as Map
import Debug.Trace (trace)

type Context a = Map.Map Label a

emptyContext :: Context a
emptyContext = Map.empty

type Judgement a = (ReaderT (Context a) Identity) a

runJudgement :: Judgement a -> (Context a) -> a
runJudgement judgement context = runIdentity $ runReaderT judgement context

typeof' :: Term -> Type
typeof' term = trace ("\nTyping term " ++ show term) runJudgement (typeof term) emptyContext

typeof :: Term -> Judgement Type
typeof Bottom = return Empty
typeof (Numeral _) = return Nat
typeof (Variable label) = do
  context <- ask
  case Map.lookup label context of
    Just val -> return $ trace ("[" ++ label ++ "]: " ++ show val) val
    Nothing -> error $ "Undefined variable " ++ label
typeof f@(Lambda label varType body) = do
  context <- ask
  let newContext = Map.insert label varType context
  bodType <- local (const newContext) (typeof body)
  let typ = varType :-> bodType
  return $ trace ("[" ++ show f ++ "]: " ++ show typ) typ
typeof term@(Apply l r) = do
  funcType <- typeof l
  argType <- typeof r
  case funcType of
    (x :-> y) ->
      if x == argType
        then return $ trace ("[" ++ show term ++ "]: " ++ show y) y
        else
          error
            ( "Cannot type "
                ++ (show term ++ " because the lhs " ++ show x)
                ++ (" cannot be applied with rhs " ++ show argType)
            )
    _ -> error ""
typeof (Succ body) = typeof body
typeof (Product []) = return Unit
typeof term@(Product terms) = do
  types <- traverse typeof terms
  let typ = foldl1 Cross types
  return $ trace ("[" ++ show term ++ "]: " ++ show typ) typ
-- let allTypesAreEqual = and $ map (== head types) (tail types)
-- if allTypesAreEqual
-- then return $ foldl1 Cross types
-- else error $ "Elements of a product must all be the same type. Product: " ++ show terms
-- return $ foldl' (Cross) (Cross fstType sndType) types
-- typeof p@(Product _) = error $ "Error, cannot construct a product with fewer than 2 elements\n" ++ show p
typeof term@(Case n p) = do
  numType <- typeof n
  prodType <- typeof p
  case (numType, prodType) of
    (Nat, (Cross _ elemType)) -> return $ trace ("[" ++ show term ++ "]: " ++ show elemType) elemType
    (Nat, Nat) -> return $ trace ("[" ++ show term ++ "]: " ++ show Nat) Nat
    (wrongType, (Cross {})) -> error $ "the term Case<T1, T2> must have a Nat in place of T1, instead it is a " ++ show wrongType
    (_, wrongType) -> error $ "the term Case<T1, T2> must have a Cross type in place of T2, instead it is a " ++ show wrongType
typeof term@(Catch body) = do
  bodyType <- typeof body
  case bodyType of
    (:->) _ _ -> return $ trace ("[" ++ show term ++ "]: " ++ show Nat) Nat
    wrongType -> error $ "Catch is defined on types of the form T1 :-> T2 :-> ..., but instead it was given the type " ++ show wrongType