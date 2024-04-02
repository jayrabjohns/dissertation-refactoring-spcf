module BoundedASPCF where

import BoundedSPCF
import BoundedSPCFTypes
import Debug.Trace

-- Take a term of the bounded SPCF and perform an injection into a 'tuple form'
--   which contains the index at which `f` is strict as well a series of
--   continuation functions for each possible value of the argument.
inj :: Term -> Eval Term
inj f@(Lambda _ (Cross _ _) _) = do
  fType <- runJudgement (typeof f) emptyContext
  return $ trace ("inj on type: " ++ show fType ++ "\n") $ Apply (injTerm fType) f
inj f = error $ "cannot inj " ++ show f

injTerm :: Type -> Term
injTerm typ = Lambda "f" typ $ Case strictnessIndex continuations
  where
    strictnessIndex :: Term
    strictnessIndex = Succ $ Catch $ Variable "f"

    -- A series of continuation functions for each value the argument could have
    continuations :: Term
    continuations = Product [pair j | j <- [0 .. (n - 1)]]

    -- A pair consisting of a value and a corresponding continuation intended
    --   to be called if the argument is that value
    pair :: Int -> Term
    pair j = Product [Numeral j, continuation j]

    -- A continuation function for each argument of f for a given value
    continuation :: Int -> Term
    continuation j =
      Product
        [ Lambda "x" typ $ Apply (Variable "f") (projections j i)
          | i <- [0 .. m]
        ]

    projections :: Int -> Int -> Term
    projections j i = Product $ insertProduct p (Numeral j) i

    -- All possible projections on x <π_0, π_1, ... , π_(m-1)>
    p :: Product
    p = [projection (Variable "x") k | k <- [0 .. m - 1]]

    -- f has m + 1 arguments,
    m :: Int
    m = (arity typ) - 1

    -- Upper bound for the underlying datatype in bounded SPCF
    n :: Int
    n = upperBound

-- Takes a term from the ASPCF and projects it back to SPCF
proj :: Term -> Eval Term
proj term = do
  typ <- runJudgement (typeof term) emptyContext
  return $ case typ of
    (Cross ltype _) -> Apply (projTerm ltype) term
    _ -> error $ "cannot proj on type " ++ show typ ++ "\n" ++ show term

-- let strictnessIndex = projection term 1
--     continuations = projection term 2
--  in Apply (Apply projTerm strictnessIndex) continuations

projTerm :: Type -> Term
projTerm typ = Lambda "x" typ $ Lambda "y" undefined $ body
  where
    body :: Term
    body =
      Case
        strictnessIndex
        $ Product
          [ Case
              (projection (Variable "y") i)
              applications
            | i <- [0 .. m]
          ]

    strictnessIndex :: Term
    strictnessIndex = projection (Variable "x") 1

    applications :: Term
    applications =
      Product
        [ Apply (continuation j) (arg j)
          | j <- [0 .. (n - 1)]
        ]

    continuation :: Int -> Term
    continuation j = projection continuations j

    continuations :: Term
    continuations = projection (Variable "x") 2

    arg :: Int -> Term
    arg j = Product $ removeProduct projections j

    projections :: Product
    projections = [projection (Variable "y") k | k <- [0 .. m]]

    -- there are m + 1 many things on the lhs of the product
    m :: Int
    m = (arity typ) - 1

    n :: Int
    n = upperBound

-- Assuming it is normalised for now
arity :: Type -> Int
arity Base = 1
arity Empty = 0
arity Unit = 0
arity ((:->) t1 t2) = arity t1 + arity t2
arity (Cross t1 t2) = arity t1 + arity t2