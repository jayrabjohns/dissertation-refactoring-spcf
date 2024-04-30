module BoundedSPCF.TermManipulation where

import BoundedSPCF.AST (Label, Term (..))
import Data.List (find)

normalise :: Term -> Term
normalise Bottom = Bottom
normalise Top = Top
normalise num@(Numeral _) = num
normalise var@(Variable _) = var
normalise (Lambda label typ body) = Lambda label typ (normalise body)
normalise (Apply lhs rhs) = do
  let arg = normalise rhs
  case lhs of
    (Lambda label _ body) -> normalise (substitute label arg body)
    _ -> Apply (normalise lhs) arg
normalise (Succ body) = Succ (normalise body)
-- normalise (Product [singleElem]) = singleElem
normalise (Product prod) = Product (map normalise prod)
normalise (BinProduct lhs rhs) = BinProduct (normalise lhs) (normalise rhs)
normalise (Fst term) = Fst (normalise term)
normalise (Snd term) = Snd (normalise term)
normalise (Case n prod) = Case (normalise n) (normalise prod)
normalise (Catch body) = Catch (normalise body)

substitute :: Label -> Term -> Term -> Term
substitute _ _ Bottom = Bottom
substitute _ _ Top = Top
substitute _ _ literal@Numeral {} = literal
substitute old new var@(Variable label)
  | label == old = new
  | otherwise = var
substitute old new abst@(Lambda label t body)
  | label == old = abst
  | otherwise =
      let freshVar = fresh (used new ++ used body ++ [old, label])
       in Lambda freshVar t (substitute old new (rename label freshVar body))
substitute old new (Apply lhs rhs) =
  Apply (substitute old new lhs) (substitute old new rhs)
substitute old new (Succ body) = Succ $ substitute old new body
substitute old new (Product prod) = Product $ map (substitute old new) prod
substitute old new (BinProduct lhs rhs) = BinProduct (substitute old new lhs) (substitute old new rhs)
substitute old new (Fst term) = Fst (substitute old new term)
substitute old new (Snd term) = Snd (substitute old new term)
substitute old new (Case n body) =
  Case (substitute old new n) (substitute old new body)
substitute old new (Catch body) = Catch (substitute old new body)

rename :: Label -> Label -> Term -> Term
rename _ _ Bottom = Bottom
rename _ _ Top = Top
rename _ _ literal@(Numeral _) = literal
rename old new var@(Variable label)
  | label == old = Variable new
  | otherwise = var
rename old new abst@(Lambda label t body)
  | label == old = abst
  | otherwise = Lambda label t (rename old new body)
rename old new (Apply lhs rhs) = Apply (rename old new lhs) (rename old new rhs)
rename old new (Succ body) = Succ (rename old new body)
rename old new (Product prod) = Product $ map (rename old new) prod
rename old new (BinProduct lhs rhs) = BinProduct (rename old new lhs) (rename old new rhs)
rename old new (Fst term) = Fst (rename old new term)
rename old new (Snd term) = Snd (rename old new term)
rename old new (Case n body) = Case (rename old new n) (rename old new body)
rename old new (Catch body) = Catch (rename old new body)

fresh :: [Label] -> Label
fresh usedLabels = case (find (`notElem` usedLabels) possibleLabels) of
  Just label -> label
  Nothing -> error "Error, somehow all variable names have been used. This shouldn't be possible."

used :: Term -> [Label]
used Bottom = []
used Top = []
used (Numeral _) = []
used (Variable label) = [label]
used (Lambda label _ term) = label : used term
used (Apply lhs rhs) = used lhs ++ used rhs
used (Succ body) = used body
used (Product prod) = prod >>= used
used (BinProduct lhs rhs) = used lhs ++ used rhs
used (Fst term) = used term
used (Snd term) = used term
used (Case num prod) = used num ++ used prod
used (Catch term) = used term

possibleLabels :: [Label]
possibleLabels = alphabetLazyList ++ alphaNumsLazyList

alphabetLazyList :: [String]
alphabetLazyList = [[char] | char <- ['a' .. 'z']]

alphaNumsLazyList :: [String]
alphaNumsLazyList = [char : show i | i <- [1 :: Int ..], char <- ['a' .. 'z']]