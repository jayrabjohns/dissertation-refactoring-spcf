module BoundedASPCF where

import BoundedSPCF (Product, Term (..), Type (..), insertProduct, projection, removeProduct, upperBound)
import BoundedSPCFTypes (typeof')
import Debug.Trace (trace)

-- Take a term of the bounded SPCF and perform an injection into a 'tuple form'
--   which contains the index at which `f` is strict as well a series of
--   continuation functions for each possible value of the argument.
inj :: Term -> Term
inj f =
  let fType = typeof' f
   in Apply (injTerm fType) f

injTerm :: Type -> Term
injTerm ftype@((:->) (Cross xtype _) Empty) =
  let strictIndex = Catch $ Variable "f"
   in Lambda "f" ftype $
        Case strictIndex $
          Product [BinProduct (Numeral i) (continuations i) | i <- [0 .. m + 1]]
  where
    -- A series of continuation functions for each value the argument could have
    continuations :: Int -> Term
    continuations i = Product [continuation j i | j <- [0 .. n]]

    -- A continuation function for each argument of f for a given value
    continuation :: Int -> Int -> Term
    continuation j i = Lambda "x" xtype $ Apply (Variable "f") (args j i)

    -- Value for f's strict argument along with placeholders for later
    args :: Int -> Int -> Term
    args j i = Product $ insertProduct otherArgs (Numeral j) i

    -- The rest of the arguemnts for f which can be applied at a later time
    otherArgs :: Product
    otherArgs = [projection (Variable "x") k | k <- [0 .. (productLength xtype) - 1]]

    -- f has m + 1 arguments,
    m :: Int
    m = (arity ftype) - 1

    -- Upper bound for the underlying datatype in bounded SPCF
    n :: Int
    n = upperBound - 1
injTerm typ =
  error $
    "Cannot inject term in this form, it must"
      ++ "be of the form [(T1 X T1 X ...) => 0]. The provided term is of type "
      ++ show typ

proj :: Term -> Term
proj term =
  let typ = typeof' term
   in Apply (projTerm typ) term

projTerm :: Type -> Term
projTerm typ@(Cross _ (Cross _ (continuationArgsType :-> Empty))) =
  Lambda "tuple" typ $
    Lambda "nextargs" fArgsType $
      trace ("\nconstructing proj term with type \\tuple: " ++ show typ ++ " \\nextargs: " ++ show fArgsType ++ ". ...") $
        let strictIndex = Fst (Variable "tuple")
            strictArg = \i -> projection (Variable "nextargs") i
         in Case strictIndex $
              Product [Case (strictArg i) (applications i) | i <- [0 .. m]]
  where
    applications :: Int -> Term
    applications i =
      let continuations = Snd (Variable "tuple")
          cont = \j -> projection continuations j
          missingArgs = Product $ removeProduct (trace ("nextargs = " ++ show providedArgs) providedArgs) i
       in Product [Apply (cont j) missingArgs | j <- [0 .. n]]

    providedArgs :: Product
    providedArgs = [projection (Variable "nextargs") i | i <- [0 .. m]]

    fArgsType = continuationArgsType `Cross` Nat

    m :: Int
    m = (productLength fArgsType) - 1

    n :: Int
    n = upperBound - 1
projTerm typ =
  error $
    "Cannot project term in this form, it must"
      ++ "be of the form [m+1 X (n^m => 0)^n]. The provided term is of type "
      ++ show typ

-- Assuming it is normalised for now
arity :: Type -> Int
arity Nat = 0
arity Empty = 0
arity Unit = 0
arity ((:->) _ t2) = 1 + arity t2
arity (Cross _ _) = 0

nfoldnats :: Int -> Type
nfoldnats 1 = Nat
nfoldnats n =
  let nats = [Nat | _ <- [0 .. n]]
   in foldr1 Cross nats

productLength :: Type -> Int
productLength Nat = 1
productLength Empty = 1
productLength Unit = 1
productLength ((:->) t1 t2) = productLength t1 + productLength t2
productLength (Cross t1 t2) = productLength t1 + productLength t2