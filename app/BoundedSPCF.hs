module BoundedSPCF (Term (..), Type (..)) where

type Label = String

data Term
  = Variable Label
  | Lambda Label Type Term
  | Apply Term Term
  | Error1
  | Error2
  | Catch Label

instance Show Term where
  show = beautify 0
    where
      beautify :: Int -> Term -> String
      beautify _ (Variable x) = show x
      beautify i (Lambda var t term) = if i /= 0 then "(" ++ s ++ ")" else s where s = "\\" ++ show var ++ ": " ++ show t ++ " . " ++ beautify 0 term
      beautify i (Apply lhs rhs) = if i == 2 then "(" ++ s ++ ")" else s where s = beautify 1 lhs ++ " " ++ beautify 2 rhs
      beautify _ Error1 = "error1"
      beautify _ Error2 = "error2"
      beautify _ (Catch x) = x

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

-- Combinators

numeral :: Int -> Term
numeral i = Lambda "f" (Base :-> Base) (Lambda "x" Base (numeral' i))
  where
    numeral' i
      | i <= 0 = Variable "x"
      | otherwise = Apply (Variable "f") (numeral' (i - 1))

succ :: Term
succ =
  Lambda
    "m"
    ((Base :-> Base) :-> (Base :-> Base))
    ( Lambda
        "f"
        (Base :-> Base)
        ( Lambda
            "x"
            Base
            ( Apply
                (Apply (Variable "m") (Variable "f"))
                (Apply (Variable "f") (Variable "x"))
            )
        )
    )

suc :: Term -> Term
suc x = Apply succ x