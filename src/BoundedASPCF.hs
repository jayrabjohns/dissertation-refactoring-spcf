module BoundedASPCF where

import BoundedSPCF

-- Take a term of the bounded SPCF and perform an injection into a 'tuple form'
--   which contains the index at which `f` is strict as well a series of
--   continuation functions for each possible value of the argument.
inj :: Term -> Term
inj f@(Lambda _ typ@(Cross _ _) _) = Apply (injTerm typ) f
inj f = error $ "cannot inj " ++ show f

injTerm :: Type -> Term
injTerm typ = Lambda "f" typ $ Case (Catch (Variable "f")) continuations
  where
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
proj :: Term -> Term
proj x@(Lambda label (Cross ltyp _) _) = Apply (projTerm label ltyp) x
proj p = error $ "cannot proj " ++ show p

projTerm :: Label -> Type -> Term
projTerm x ltyp = Lambda "y" undefined body
  where
    body :: Term
    body =
      Case
        (projection (Variable x) 1)
        (Product [continuations i | i <- [0 .. m]])

    continuations :: Int -> Term
    continuations i = Case (projection (Variable "y") i) applications

    applications :: Term
    applications = Product [Apply (continuation j) (arg j) | j <- [0 .. (n - 1)]]

    continuation :: Int -> Term
    continuation j = projection (projection (Variable x) 2) j

    arg :: Int -> Term
    arg j = Product $ removeProduct projections j

    projections :: [Term]
    projections = [projection (Variable "y") k | k <- [0 .. m]]

    m :: Int
    m = (arity ltyp) - 1

    n :: Int
    n = upperBound

-- Assuming it is normalised for now
arity :: Type -> Int
arity Base = 1
arity Empty = 0
arity ((:->) t1 t2) = arity t1 + arity t2
arity (Cross _ _) = 1