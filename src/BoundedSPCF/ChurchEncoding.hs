module BoundedSPCF.ChurchEncoding where

import BoundedSPCF.AST

churchnat :: Term -> Term
churchnat (Numeral i) = numeral i
churchnat badTerm = error $ "cant construct church nat from term " ++ show badTerm

numeral :: Int -> Term
numeral i = Lambda "f" (Nat :-> Nat) (Lambda "x" Nat (numeral' i))
  where
    numeral' i
      | i <= 0 = Variable "x"
      | otherwise = Apply (Variable "f") (numeral' (i - 1))