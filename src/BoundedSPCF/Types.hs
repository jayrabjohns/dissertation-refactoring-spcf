module BoundedSPCF.Types where

import BoundedSPCF.AST (Term (..), Type (..))
import BoundedSPCF.TermManipulation
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (MonadReader (ask, local), ReaderT (runReaderT))
import Debug.Trace (trace)
import Utils.Environment (Environment, empty, insert, lookup)
import Prelude hiding (lookup)

type Context a = Environment a

type Judgement a = (ReaderT (Context a) Identity) a

runJudgement :: Judgement a -> (Context a) -> a
runJudgement judgement context = runIdentity $ runReaderT judgement context

typeof' :: Term -> Type
typeof' term = trace ("\nTyping term " ++ show term) runJudgement (typeof term) empty

typeof :: Term -> Judgement Type
typeof Bottom = return Empty
typeof Top = return Empty
typeof (Numeral _) = return Nat
typeof (Variable label) = do
  context <- ask
  case lookup label context of
    Just val -> return $ trace ("[" ++ label ++ "]: " ++ show val) val
    Nothing -> error $ "Undefined variable " ++ label
-- typeof f@(Lambda label Nat Bottom) =
--   let typ = Empty
--    in return $ trace ("[" ++ show f ++ "]: " ++ show typ) typ
typeof f@(Lambda label varType body) = do
  context <- ask
  let newContext = insert label varType context
  bodType <- local (const newContext) (typeof body)
  let typ = varType :-> bodType
  return $ trace ("[" ++ show f ++ "]: " ++ show typ) typ
typeof term@(Apply l r) = do
  funcType <- typeof l
  argType <- typeof r
  case funcType of
    (x :-> y) ->
      if x `isoOf` argType
        then return $ trace ("[" ++ show term ++ "]: " ++ show y) y
        else
          error
            ( "Cannot type "
                ++ (show term ++ " because the lhs " ++ show x)
                ++ (" cannot be applied with rhs " ++ show argType)
            )
    _ -> error $ "Can't apply to a term with type " ++ show funcType ++ ". Attempted to apply argument " ++ show argType
typeof (Succ body) = typeof body
typeof term@(Product []) = return $ trace ("[" ++ show term ++ "]: " ++ show Unit) Unit
-- typeof term@(Product [x]) = do
--   typ <- typeof x
--   return $ trace ("[" ++ show term ++ "]: " ++ show typ) typ
typeof term@(Product terms) = do
  types <- traverse typeof terms
  let typ = Cross (head types) (length types)
  return $ trace ("[" ++ show term ++ "]: " ++ show typ) typ
typeof term@(BinProduct lhs rhs) = do
  leftType <- typeof lhs
  rightType <- typeof rhs
  let typ = Pair leftType rightType
  return $ trace ("[" ++ show term ++ "]: " ++ show typ) typ
typeof term@(Fst body) = do
  bodyType <- typeof body
  case bodyType of
    (Cross fstType n) ->
      if n >= 1
        then return $ trace ("[" ++ show term ++ "]: " ++ show fstType) fstType
        else error $ "Cannot take π1 on the type " ++ show bodyType
    wrongType -> error $ "Cannot take π1 on the type " ++ show wrongType
typeof term@(Snd body) = do
  bodyType <- typeof body
  case bodyType of
    (Cross typ n) ->
      if n >= 2
        then return $ trace ("[" ++ show term ++ "]: " ++ show typ) typ
        else error $ "Cannot take π2 on the type " ++ show bodyType
    wrongType -> error $ "Cannot take π2 on the type " ++ show wrongType
typeof term@(Case n p) = do
  numType <- typeof n
  prodType <- typeof p
  -- let allElemsAreBaseType = all (== Nat) (tail types)
  case (numType, prodType) of
    -- (Nat, (Nat `Cross` elemType)) -> return $ trace ("[HI" ++ show term ++ "]: " ++ show elemType) elemType
    (Nat, (Pair Nat elemType)) -> return $ trace ("[" ++ show term ++ "]: " ++ show elemType) elemType
    (Nat, (Cross elemType _)) -> return $ trace ("[" ++ show term ++ "]: " ++ show elemType) elemType
    (Nat, elemType) -> return $ trace ("[" ++ show term ++ "]: " ++ show elemType) elemType
    (wrongType, (Cross {})) -> error $ "the term Case<T1, T2> must have a Nat in place of T1, instead it is a " ++ show wrongType
    (_, wrongType) -> error $ "the term Case<T1, T2> must have a Cross type in place of T2, instead it is a " ++ show wrongType
typeof term@(Catch body) = do
  bodyType <- typeof body
  case bodyType of
    (:->) _ _ -> return $ trace ("[" ++ show term ++ "]: " ++ show Nat) Nat
    wrongType -> error $ "Catch is defined on types of the form T1 :-> T2 :-> ..., but instead it was given the type " ++ show wrongType

-- | Built in isomorphisms between types
isoOf :: Type -> Type -> Bool
isoOf Unit (Cross _ 0) = True
isoOf (Cross _ 0) Unit = True
isoOf (Cross x 1) (Cross y 1) = isoOf x y
isoOf (Cross x 1) y = isoOf x y
isoOf x (Cross y 1) = isoOf x y
isoOf x y = x == y