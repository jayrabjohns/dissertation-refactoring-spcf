module SPCF.AffineTransformation where

import Frontend.Lexer (AlexPosn)
import SPCF.AST
import SPCF.Constants (info)
import SPCF.Types

-- This unbounded case of affine transformation is currently not working

inj :: Term AlexPosn -> Term AlexPosn
inj term@(Top _) = term
inj term@(Bottom _) = term
inj f =
  let fType = typeof' f
   in Apply info (injTerm fType) f

injTerm :: Type -> Term AlexPosn
injTerm ftype@((:->) (Cross xtype _) Empty) =
  let strictIndex = Catch info $ Variable info "f"
   in Lambda info "f" ftype $
        -- Case info strictIndex $
        Lst info [Pair info strictIndex {-(Numeral i)-} (continuations i) | i <- [0 .. m + 1]]
  where
    -- A series of continuation functions for each value the argument could have
    continuations :: Int -> Term AlexPosn
    continuations i = Lst info [continuation j i | j <- [0 ..]]

    -- A continuation function for each argument of f for a given value
    continuation :: Int -> Int -> Term AlexPosn
    continuation j i = Lambda info "x" xtype $ Apply info (Variable info "f") (args j i)

    -- Value for f's strict argument along with placeholders for later
    args :: Int -> Int -> Term AlexPosn
    args j i = Lst info $ insertProduct otherArgs (Numeral info j) i

    -- The rest of the arguemnts for f which can be applied at a later time
    otherArgs :: [Term AlexPosn]
    otherArgs = [projection (Variable info "x") k | k <- [0 .. (productLength xtype) - 1]]

    -- f has m + 1 arguments,
    m :: Int
    m = (arity ftype) - 1

-- Upper bound for the underlying datatype in bounded SPCF
-- n :: Int
-- n = upperBound - 1
injTerm typ =
  error $
    "Cannot inject term in this form, it must"
      ++ "be of the form [(T1 X T1 X ...) => 0]. The provided term is of type "
      ++ show typ

-- -- injTerm :: Type -> Term AlexPosn
-- -- injTerm ftype =
-- --   let strictIndex = Catch info $ Variable info "f"
-- --    in Lambda info "f" ftype $
-- --         Pair info strictIndex (Lst info [(continuations i) | i <- [0 .. m + 1]])
-- --   where
-- --     -- A series of continuation functions for each value the argument could have
-- --     continuations :: Int -> Term AlexPosn
-- --     continuations i = Lst info [continuation j i | j <- [0 ..]]

-- --     -- A continuation function for each argument of f for a given value
-- --     continuation :: Int -> Int -> Term AlexPosn
-- --     continuation j i =
-- --       Lambda info "x" undefined $
-- --         Apply info (Variable info "f") (args j i)

-- --     -- Value for f's strict argument along with placeholders for later
-- --     args :: Int -> Int -> Term AlexPosn
-- --     args j i = Lst info $ insertProduct otherArgs (Numeral info j) i

-- --     -- The rest of the arguemnts for f which can be applied at a later time
-- --     otherArgs :: [Term AlexPosn]
-- --     otherArgs = [projection (Variable info "x") k | k <- [0 ..]] -- up to number of args

-- --     -- f has m + 1 arguments,
-- --     m :: Int
-- --     m = (arity ftype) - 1

-- Upper bound for the underlying datatype in bounded SPCF
-- n :: Int
-- n = upperBound - 1

-- injTerm typ =
--   error $
--     "Cannot inject term in this form, it must"
--       ++ "be of the form [(T1 X T1 X ...) => 0]. The provided term is of type "
--       ++ show typ

-- proj :: Term AlexPosn -> Term AlexPosn
-- proj term@(Top _) = term
-- proj term@(Bottom _) = term
-- proj term =
--   let typ = typeof' term
--    in Apply info (projTerm typ) term

-- projTerm :: Type -> Term AlexPosn
-- projTerm typ =
--   Lambda info "tuple" typ $
--     Lambda info "nextargs" fArgsType $
--       let strictIndex = Fst (Variable "tuple")
--           strictArg = \i -> projection (Variable "nextargs") i
--        in Case strictIndex $
--             Product [Case (strictArg i) (applications i) | i <- [0 .. m]]
--   where
--     applications :: Int -> Term AlexPosn
--     applications i =
--       let continuations = Snd (Variable "tuple")
--           cont = \j -> projection continuations j
--           missingArgs = Lst info $ removeProduct providedArgs i
--        in Lst info [Apply (cont j) missingArgs | j <- [0 ..]]

--     providedArgs :: [Term AlexPosn]
--     providedArgs = [projection (Variable "nextargs") i | i <- [0 .. m]]

--     fArgsType = continuationArgsType `Cross` Nat

--     m :: Int
--     m = (productLength fArgsType) - 1

-- n :: Int
-- n = upperBound - 1

-- projTerm typ =
--   error $
--     "Cannot project term in this form, it must"
--       ++ "be of the form [m+1 X (n^m => 0)^n]. The provided term is of type "
--       ++ show typ

-- Assuming it is normalised for now
arity :: Type -> Int
arity Base = 0
arity Empty = 0
arity Unit = 0
arity ((:->) _ t2) = 1 + arity t2
arity (Cross _ _) = 0

-- nfoldnats :: Int -> Type
-- nfoldnats 1 = Base
-- nfoldnats n =
--   let nats = [Base | _ <- [0 .. n]]
--    in foldr1 Cross nats

productLength :: Type -> Int
productLength Base = 1
productLength Empty = 1
productLength Unit = 1
productLength ((:->) t1 t2) = productLength t1 + productLength t2
productLength (Cross t1 t2) = productLength t1 + productLength t2