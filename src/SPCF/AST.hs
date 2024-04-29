module SPCF.AST where

import Data.List

type Label = String

-- | A term in SCPF
data Term info
  = -- | A natural number
    Numeral info Int
  | -- | Bottom
    Bottom info
  | -- | Top
    Top info
  | -- | A family of error constants
    Error info Error
  | -- | A potentially infinite lazy list of terms
    Lst info [Term info]
  | -- | Partial element retrieval from a lazy list
    Project info (Term info) [Term info]
  | -- | A variable
    Variable info Label
  | -- | A lambda abstraction
    Lambda info Label Type (Term info)
  | -- | An application
    Apply info (Term info) (Term info)
  | -- | The successor of a natural number
    Succ info (Term info)
  | -- | The predecessor of a natural number
    Pred info (Term info)
  | -- | Fixed point recursion by finding the fp of a term
    YComb info (Term info)
  | -- | Sequential iteration
    Iter info (Term info)
  | -- | Conditional on whether a natural number is 0
    If0 info (Term info) (Term info) (Term info)
  | -- Non-local control operator to return where a function is strict
    Catch info (Term info)
  | --  | A pair of two natural numbers
    Pair info (Term info) (Term info)
  | -- | The first element of a pair
    Fst info (Term info)
  | -- | The second element of a pair
    Snd info (Term info)
  deriving (Eq)

-- | Retrieve the state from lexing and parsing for a given term.
termInfo :: Term info -> info
termInfo (Numeral inf _) = inf
termInfo (Variable inf _) = inf
termInfo (Bottom inf) = inf
termInfo (Top inf) = inf
termInfo (Lambda inf _ _ _) = inf
termInfo (Apply inf _ _) = inf
termInfo (If0 inf _ _ _) = inf
termInfo (Succ inf _) = inf
termInfo (Pred inf _) = inf
termInfo (YComb inf _) = inf
termInfo (Iter inf _) = inf
termInfo (Error inf _) = inf
termInfo (Catch inf _) = inf
termInfo (Pair inf _ _) = inf
termInfo (Fst inf _) = inf
termInfo (Snd inf _) = inf
termInfo (Lst inf _) = inf

-- | Error constants as defined by Cartwright and Felleisen
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
      beautify _ (Bottom _) = "⊥"
      beautify _ (Top _) = "T"
      beautify i (Lambda _ var t term) = if i /= 0 then "(" ++ s ++ ")" else s where s = "\\" ++ var ++ ":" ++ show t ++ " => " ++ beautify 0 term
      beautify i (Apply _ lhs rhs) = if i == 2 then "(" ++ s ++ ")" else s where s = beautify 1 lhs ++ " " ++ beautify 2 rhs
      beautify i (Succ _ term) = if i /= 0 then "(" ++ s ++ ")" else s where s = "succ " ++ beautify 2 term
      beautify i (Pred _ term) = if i /= 0 then "(" ++ s ++ ")" else s where s = "pred " ++ beautify 2 term
      beautify i (YComb _ term) = if i /= 0 then "(" ++ s ++ ")" else s where s = "fix " ++ beautify 2 term
      beautify i (Iter _ term) = if i /= 0 then "(" ++ s ++ ")" else s where s = "iter " ++ beautify 2 term
      beautify i (If0 _ cond lterm rterm) = if i == 2 then "(" ++ s ++ ")" else s where s = "if0 " ++ beautify 1 cond ++ " then " ++ beautify 1 lterm ++ " else " ++ beautify 1 rterm
      beautify _ (Error _ Error1) = "Error1"
      beautify _ (Error _ Error2) = "Error2"
      beautify i (Catch _ term) = if i /= 0 then "(" ++ s ++ ")" else s where s = "catch " ++ beautify 2 term
      beautify _ (Pair _ lhs rhs) = "<" ++ (beautify 2) lhs ++ ", " ++ (beautify 2) rhs ++ ">"
      beautify _ (Fst _ term) = "fst(" ++ show term ++ ")"
      beautify _ (Snd _ term) = "snd(" ++ show term ++ ")"
      beautify _ (Lst _ (x : _)) = "[" ++ beautify 0 x ++ ", ...]"
      beautify _ (Lst _ []) = "[]"

data Type
  = -- | Constants, numerals, errors
    Base
  | -- | Finished computation, analogous to void
    Empty
  | -- | Functions
    (:->) Type Type
  | -- | Empty product
    Unit
  | -- | Product
    Cross Type Type
  deriving (Eq)

-- instance Show Type where
--   show = beautify
--     where
--       beautify :: Type -> String
--       beautify Base = "o"
--       beautify Empty = "0"
--       beautify (Base :-> rhs) = "o -> " ++ beautify rhs
--       beautify (lhs :-> rhs) = "(" ++ beautify lhs ++ ") -> " ++ beautify rhs

instance Show Type where
  show = beautify 0
    where
      beautify :: Int -> Type -> String
      beautify _ Base = "o"
      -- beautify _ (Nat :-> rhs) = "Nat->" ++ beautify 0 rhs
      -- beautify i (lhs :-> rhs) = if i == 1 then "(" ++ beautify 1 lhs ++ ")" ++ "->" ++ beautify 1 rhs else beautify 0 lhs ++ "->" ++ beautify 1 rhs
      beautify i (lhs :-> rhs) = if i == 1 then "(" ++ s ++ ")" else s where s = beautify 1 lhs ++ "->" ++ beautify 0 rhs
      beautify _ Empty = "0"
      beautify i (Cross lhs rhs) = if i == 0 then "(" ++ s ++ ")" else s where s = beautify 1 lhs ++ "x" ++ beautify 1 rhs
      beautify _ Unit = "()"

projection :: Term info -> Int -> Term info
projection (Lst _ prod) i =
  case drop i prod of
    x : _ -> x
    [] ->
      error $
        "Index out of range. Cannot access element at index "
          ++ (show i ++ " of the product " ++ show prod)
projection term i = error $ "error cant project in " ++ show term ++ " at position " ++ show i -- Case (Numeral i) term

-- Insert element into 0 indexed n-fold product at a given position
insertProduct :: [Term info] -> Term info -> Int -> [Term info]
insertProduct prod toInsert index =
  let (before, after) = Data.List.splitAt index prod
   in before ++ [toInsert] ++ after

-- Remove element from 0 indexed n-fold product at a given position
removeProduct :: [Term info] -> Int -> [Term info]
removeProduct prod index =
  let (before, after) = Data.List.splitAt (index + 1) prod
   in (take index before) ++ after