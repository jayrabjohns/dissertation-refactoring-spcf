module SPCF.AST where

type Label = String

data Term info
  = Numeral info Int
  | Variable info Label
  | Lambda info Label Type (Term info)
  | Apply info (Term info) (Term info)
  | Succ info (Term info)
  | Pred info (Term info)
  | YComb info (Term info)
  | If0 info (Term info) (Term info) (Term info)
  | Error info Error
  | Catch info (Term info)
  deriving (Eq)

termInfo :: Term info -> info
termInfo (Numeral inf _) = inf
termInfo (Variable inf _) = inf
termInfo (Lambda inf _ _ _) = inf
termInfo (Apply inf _ _) = inf
termInfo (If0 inf _ _ _) = inf
termInfo (Succ inf _) = inf
termInfo (Pred inf _) = inf
termInfo (YComb inf _) = inf
termInfo (Error inf _) = inf
termInfo (Catch inf _) = inf

data Error
  = Error1
  | Error2
  deriving (Eq, Show)

instance Show (Term info) where
  show = beautify 0
    where
      beautify :: Int -> Term info -> String
      beautify _ (Numeral _ i) = show i
      beautify _ (Variable _ x) = x
      beautify i (Lambda _ var _ term) = if i /= 0 then "(" ++ s ++ ")" else s where s = "\\" ++ show var ++ {-": " ++ show t ++ -} ". " ++ beautify 0 term
      beautify i (Apply _ lhs rhs) = if i == 2 then "(" ++ s ++ ")" else s where s = beautify 1 lhs ++ " " ++ beautify 2 rhs
      beautify i (Succ _ term) = if i /= 0 then "(" ++ s ++ ")" else s where s = "Succ " ++ beautify 2 term
      beautify i (Pred _ term) = if i /= 0 then "(" ++ s ++ ")" else s where s = "Pred " ++ beautify 2 term
      beautify i (YComb _ term) = if i /= 0 then "(" ++ s ++ ")" else s where s = "Y " ++ beautify 2 term
      beautify i (If0 _ cond lterm rterm) = if i == 2 then "(" ++ s ++ ")" else s where s = "If0 " ++ beautify 1 cond ++ " then " ++ beautify 1 lterm ++ " else " ++ beautify 1 rterm
      beautify _ (Error _ Error1) = "Error1"
      beautify _ (Error _ Error2) = "Error2"
      beautify i (Catch _ term) = if i /= 0 then "(" ++ s ++ ")" else s where s = "Catch " ++ beautify 2 term

-- In the bounded SPCF, the base type will also be bounded. E.g. Boolean or n natural ints

data Type
  = Base
  | (:->) Type Type
  deriving (Eq)

instance Show Type where
  show = beautify
    where
      beautify :: Type -> String
      beautify Base = "o"
      beautify (Base :-> rhs) = "o -> " ++ beautify rhs
      beautify (lhs :-> rhs) = "(" ++ beautify lhs ++ ") -> " ++ beautify rhs
