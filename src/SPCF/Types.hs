module SPCF.Types where

import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (MonadReader (ask, local), ReaderT (runReaderT))
import qualified Data.Map as Map
import Debug.Trace (trace)
import SPCF.AST (Label, Term (..), Type (..))
import SPCF.Evaluation (Eval)

emptyContext :: Context
emptyContext = Map.empty

type Context = Map.Map Label (Eval Type)

type Judgement a = (ReaderT Context Identity) a

runJudgement :: Judgement a -> Context -> a
runJudgement judgement context = runIdentity $ runReaderT judgement context

typeof :: Term info -> Judgement (Eval Type)
typeof (Numeral {}) = return $ pure Base
typeof (Error {}) = return $ pure Base
typeof (Variable _ label) = do
  context <- ask
  case Map.lookup label context of
    Just val -> return val
    Nothing -> error $ "Undefined variable " ++ label
typeof (Lambda _ label varTyp body) = do
  context <- ask
  let newContext = Map.insert label (pure varTyp) context
  typeEval <- local (const newContext) (typeof body)
  return $ do
    bodyType <- typeEval
    return $ varTyp :-> bodyType
-- return $ pure $ varTyp :-> bodType
typeof t@(Apply _ l r) = do
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
typeof (Succ _ body) = typeof body
typeof (Pred _ body) = typeof body
typeof (If0 _ cond tt ff) = do
  evalCond <- typeof cond
  evalTrue <- typeof tt
  evalFalse <- typeof ff
  return $ do
    condType <- evalCond
    trueType <- evalTrue
    falseType <- evalFalse
    case (condType, trueType, falseType) of
      (Base, Base, Base) -> return Base
      _ -> error ""
typeof (YComb _ body) = typeof body
-- typeof (Product (fstT : sndT : xs)) = do
-- types <- traverse typeof xs
-- fstType <- typeof fstT
-- sndType <- typeof sndT
-- return do $
-- return $ foldl' (Cross) (Cross fstType sndType) types

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
typeof (Catch _ body) = do
  evalBody <- typeof body
  return $ do
    bodyType <- evalBody
    case bodyType of
      (:->) _ _ -> return Base
      _ -> error ""

-- data TyInfo = TyInfo {ty_of :: Type}
--   deriving (Show)

-- tycheckCommand :: (Show info) => Command info -> Judgement (Command TyInfo)
-- tycheckCommand (CBind fi id t) = do
--   t' <- tycheckTerm t
--   return $ CBind (mkInfo (ty_of_term t')) id t'
-- tycheckCommand (CEval fi t) = do
--   t' <- tycheckTerm t
--   return $ CEval (mkInfo (ty_of_term t')) t'

-- tycheckCommands :: (Show info) => [Command info] -> Judgement [Command TyInfo]
-- tycheckCommands [] = return []
-- tycheckCommands (c : cs) = do
--   c' <- tycheckCommand c
--   let ty = ty_of_command c'
--   case c' of
--     CBind _ id _ -> do
--       cs' <- local (add id ty) (tycheckCommands cs)
--       return $ c' : cs'
--     _ -> do
--       cs' <- tycheckCommands cs
--       return $ c' : cs'

-- tycheckProg :: (Show info) => Prog info -> Judgement (Prog TyInfo)
-- tycheckProg p = do
--   cs <- tycheckCommands (prog_of p)
--   return $ Prog {pinfo_of = mkInfo TyBool, prog_of = cs}