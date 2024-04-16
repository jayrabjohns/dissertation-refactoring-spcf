module SPCF.TermManipulation where

import Data.List (elemIndex, find)
import SPCF.AST (Label, Term (..))

normalise :: Term info -> Term info
normalise num@(Numeral {}) = num
normalise err@(Error {}) = err
normalise var@(Variable {}) = var
normalise (Lambda inf label typ body) = Lambda inf label typ (normalise body)
normalise (Apply inf lhs rhs) = do
  let lReduced = normalise lhs
  let rReduced = normalise rhs
  case lReduced of
    (Lambda _ label _ body) -> substitute label rReduced body
    _ -> Apply inf lReduced rReduced
normalise (Succ inf term) = Succ inf $ normalise term
normalise (Pred inf term) = Pred inf $ normalise term
normalise (If0 inf cond tt ff) = If0 inf (normalise cond) (normalise tt) (normalise ff)
normalise (Catch inf body) = Catch inf (normalise body)
normalise (YComb inf body) = YComb inf (normalise body)

substitute :: Label -> Term info -> Term info -> Term info
substitute _ _ literal@Numeral {} = literal
substitute old new var@(Variable _ label)
  | label == old = new
  | otherwise = var
substitute old new abst@(Lambda inf label t body)
  | label == old = abst
  | otherwise =
      let freshVar = fresh (used new ++ used body ++ [old, label])
       in Lambda inf freshVar t (substitute old new (rename label freshVar body))
substitute old new (Apply inf lhs rhs) =
  Apply inf (substitute old new lhs) (substitute old new rhs)
substitute old new (Succ inf body) = Succ inf (substitute old new body)
substitute old new (Pred inf body) = Pred inf (substitute old new body)
substitute old new (If0 inf cond iftrue iffalse) =
  let sub = substitute old new
   in (If0 inf (sub cond) (sub iftrue) (sub iffalse))
substitute old new (YComb inf body) = YComb inf (substitute old new body)
substitute _ _ err@(Error {}) = err
substitute old new (Catch inf body) = Catch inf (substitute old new body)

rename :: Label -> Label -> Term info -> Term info
rename _ _ literal@(Numeral {}) = literal
rename old new var@(Variable inf label)
  | label == old = Variable inf new
  | otherwise = var
rename old new abst@(Lambda inf label t body)
  | label == old = abst
  | otherwise = Lambda inf label t (rename old new body)
rename old new (Apply inf lhs rhs) = Apply inf (rename old new lhs) (rename old new rhs)
rename old new (Succ inf body) = Succ inf (rename old new body)
rename old new (Pred inf body) = Pred inf (rename old new body)
rename old new (If0 inf cond iftrue iffalse) =
  let r = rename old new
   in If0 inf (r cond) (r iftrue) (r iffalse)
rename old new (YComb inf body) = YComb inf (rename old new body)
rename _ _ err@(Error {}) = err
rename old new (Catch inf body) = Catch inf (rename old new body)

fresh :: [Label] -> Label
fresh usedLabels = case (find (`notElem` usedLabels) possibleLabels) of
  Just label -> label
  Nothing -> error "Error, somehow all variable names have been used. This shouldn't be possible."

used :: Term info -> [Label]
used (Numeral _ _) = []
used (Variable _ label) = [label]
used (Lambda _ label _ term) = label : used term
used (Apply _ lhs rhs) = used lhs ++ used rhs
used (Succ _ body) = used body
used (Pred _ body) = used body
used (If0 _ cond iftrue iffalse) = used cond ++ used iftrue ++ used iffalse
used (YComb _ body) = used body
used (Error {}) = []
used (Catch _ term) = used term

possibleLabels :: [Label]
possibleLabels = alphabetLazyList ++ alphaNumsLazyList

alphabetLazyList :: [String]
alphabetLazyList = [[char] | char <- ['a' .. 'z']]

alphaNumsLazyList :: [String]
alphaNumsLazyList = [char : show i | i <- [1 :: Int ..], char <- ['a' .. 'z']]

lastElemIndex :: (Eq a) => a -> [a] -> Maybe Int
lastElemIndex toFind elems =
  let reversedIndex = elemIndex toFind (reverse elems)
   in fmap (\i -> (length elems - i - 1)) reversedIndex