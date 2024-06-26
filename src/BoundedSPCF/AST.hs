module BoundedSPCF.AST where

import Data.List (intercalate, splitAt)
import Prelude hiding (lookup)

type Label = String

type Numeral = Int

type Product = [Term]

-- | The upper bound for numerals in the language
upperBound :: Numeral
upperBound = 2

-- | An ordered lazy list of all numerals
numerals :: [Term]
numerals = Numeral <$> [0 .. upperBound - 1]

data Term
  = -- | Natural numbers
    Numeral Numeral
  | -- | Variables
    Variable Label
  | -- | Lambda abstractions / functions
    Lambda Label Type Term
  | -- | Application / function application
    Apply Term Term
  | -- | Successor (add one)
    Succ Term
  | -- | Finite tuple of single type
    Product Product
  | -- | Binary product
    BinProduct Term Term
  | -- | First element of a product
    Fst Term
  | -- | Second element of a product
    Snd Term
  | -- | Project into a product at a specified index
    Case Term Term
  | -- | Cartwright and Felleisen's catch procedure
    Catch Term
  | -- | Element with the empty type
    Bottom
  | -- | A sort of unrecoverable error
    Top
  deriving (Eq)

instance Show Term where
  show = beautify 0
    where
      beautify :: Int -> Term -> String
      beautify _ (Numeral i) = show i
      beautify _ (Variable x) = x
      beautify i (Lambda var t term) = if i /= 0 then "(" ++ s ++ ")" else s where s = "\\" ++ var ++ ":" ++ show t ++ " => " ++ beautify 0 term
      beautify i (Apply lhs@(Lambda label _ _) rhs) = if i == 2 then "(" ++ s ++ ")" else s where s = beautify 2 lhs ++ "[" ++ show rhs ++ "/" ++ show label ++ "]" -- beautify 1 lhs ++ " " ++ beautify 2 rhs
      beautify i (Apply lhs rhs) = if i == 2 then "(" ++ s ++ ")" else s where s = beautify 1 lhs ++ " " ++ beautify 2 rhs
      beautify i (Succ term) = if i /= 0 then "(" ++ s ++ ")" else s where s = "Succ " ++ beautify 2 term
      beautify i (Product []) = if i /= 0 then "(" ++ s ++ ")" else s where s = "I"
      beautify _ (Product prod) = "<" ++ (intercalate ", " (map (beautify 2) prod)) ++ ">"
      beautify _ (BinProduct lhs rhs) = "<" ++ (beautify 2) lhs ++ ", " ++ (beautify 2) rhs ++ ">"
      beautify _ (Fst term) = "π1(" ++ show term ++ ")"
      beautify _ (Snd term) = "π2(" ++ show term ++ ")"
      beautify i (Case numeral prod) = if i /= 0 then "(" ++ s ++ ")" else s where s = "Case <" ++ beautify 0 numeral ++ ", " ++ beautify 0 prod ++ ">"
      beautify i (Catch term) = if i /= 0 then "(" ++ s ++ ")" else s where s = "Catch " ++ beautify 2 term
      beautify _ Bottom = "⊥"
      beautify _ Top = "T"

data Type
  = -- | Base type (numerals)
    Nat
  | -- | Non-terminating function
    Empty
  | -- | Funtion type
    (:->) Type Type
  | -- | Empty product
    Unit
  | -- | n-fold product
    Cross Type Int
  | -- | Binary product
    Pair Type Type
  deriving (Eq)

-- When constructing types with :-> we typically want them to associate right
-- o -> o -> o == o -> (o -> o)
-- rather than
-- o -> o -> o == (o -> o) -> o
infixr 5 :->

instance Show Type where
  show = beautify 1
    where
      beautify :: Int -> Type -> String
      beautify _ Nat = "Nat"
      beautify i (lhs :-> rhs) = if i == 0 then "(" ++ s ++ ")" else s where s = beautify 0 lhs ++ "->" ++ beautify 1 rhs
      beautify i (Cross typ n) = if i == 0 then "(" ++ s ++ ")" else s where s = "[" ++ beautify 1 typ ++ "]^" ++ show n
      beautify _ Unit = "I"
      beautify _ Empty = "0"
      beautify i (Pair lhs rhs) = if i == 0 then "(" ++ s ++ ")" else s where s = beautify 0 lhs ++ "x" ++ beautify 1 rhs

unit :: Term
unit = Product []

-- | A family of n projections πᵢ: Tⁿ => T
-- | π₀(t) = I where I is the empty product
projection :: Term -> Int -> Term
projection (BinProduct lhs _) 0 = lhs
projection (BinProduct _ rhs) 1 = rhs
projection (Product prod) i =
  case drop i prod of
    x : _ -> x
    [] ->
      error $
        "Index out of range. Cannot access element at index "
          ++ (show i ++ " of the product " ++ show prod)
projection term i = Case (Numeral i) term

-- | Insert element into 0 indexed n-fold product at a given position
insertProduct :: Product -> Term -> Int -> Product
insertProduct prod toInsert index =
  let (before, after) = Data.List.splitAt index prod
   in before ++ [toInsert] ++ after

-- | Remove element from 0 indexed n-fold product at a given position
removeProduct :: Product -> Int -> Product
removeProduct prod index =
  let (before, after) = Data.List.splitAt (index + 1) prod
   in (take index before) ++ after