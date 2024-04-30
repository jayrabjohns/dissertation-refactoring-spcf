module BoundedSPCF.AffineTransformation where

import BoundedSPCF.AST (Product, Term (..), Type (..), insertProduct, projection, removeProduct, upperBound)
import BoundedSPCF.TermManipulation (normalise)
import BoundedSPCF.Types (typeof')
import Debug.Trace (trace)

-- | Inductively apply inj as originally defined by Laird until a funciton is
-- | in tuple form, where a function is represented as the index at which
-- | `f` is strict as well a series of continuation functions for each possible
-- | value of the argument.
inj :: Term -> Term
inj Top = Top
inj Bottom = Bottom
inj f =
  -- trace ("about to inj " ++ show f) $
  let fType = typeof' f
   in if isBaseType fType
        then normalise f
        else
          let injF = normalise $ Apply (injTerm fType) f
           in case injF of
                (Case n (Product prod)) -> Case n (Product $ aux <$> prod)
                Top -> Top
                Bottom -> Bottom
                _ -> error $ "Inj shouldn't be able to produce a term of this kind: " ++ show injF
  where
    aux (BinProduct i (Product conts)) = BinProduct i (Product $ inj <$> conts)
    aux badTerm =
      error $
        "Inj term does not have expected structure."
          ++ (" Expected a binary product but got a " ++ show badTerm)

    isBaseType :: Type -> Bool
    isBaseType (Nat :-> Empty) = True
    isBaseType (lhs :-> rhs) = isBaseType lhs || isBaseType rhs
    isBaseType (Cross lhs rhs) = isBaseType lhs || isBaseType rhs
    isBaseType _ = False

-- | Apply inj once without any care for the consequences.
inj' :: Term -> Term
inj' Top = Top
inj' Bottom = Bottom
inj' f =
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

-- | Inductively apply proj, recovering the type structure from
-- | a funciton in tuple form.
proj :: Term -> Term
proj Top = Top
proj Bottom = Bottom
proj term =
  let typ = typeof' term
   in trace ("about to proj on type " ++ show typ) $ case typ of
        --  (Cross _ (Cross _ (_ :-> Empty))) -> trace ("1about to apply a proj on " ++ show term) $ normalise $ Apply (projTerm typ) term
        (Nat `Cross` ((Nat :-> Empty) `Cross` _)) -> trace ("done " ++ show term) $ normalise $ Apply (projTerm typ) term
        (Nat `Cross` (((Nat `Cross` _) :-> Empty) `Cross` _)) -> trace ("2about to apply a proj on " ++ show term) $ normalise $ Apply (projTerm typ) term
        _ -> case term of
          Case n (Product pairs) ->
            let f = Product $ aux <$> pairs
             in trace ("3about to apply a proj on " ++ show term) $ proj $ Case n f
          _ -> error "infite loop" -- trace ("not about to apply proj to term with type " ++ show typ ++ " and definition " ++ show term) $ normalise term -- error $ "Cannot project term " ++ show term
  where
    aux (BinProduct i (Product conts)) = BinProduct i (Product $ proj <$> conts)
    aux badTerm =
      error $
        "Proj term does not have expected structure."
          ++ (" Expected a binary product but got a " ++ show badTerm)

-- | Apply proj once without any care for the consequences.
proj' :: Term -> Term
proj' Top = Top
proj' Bottom = Bottom
proj' term =
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