module SPCFConsts (apply, omega, (<+), (+>), addLeft, addRight) where

import SPCF

-- Apply any number of arguments to a term.
--   A helper function for the construction of nested applications.
apply :: Term -> [Term] -> Term
apply term [] = term
apply term (arg : args) = apply (Apply term arg) args

-- Prototypical divergent expression
omega :: Term
omega = YComb $ Lambda "x" (Base, 0) (Variable "x")

-- Addition operator.
--   Recurses on the left argument, also known as left addition.
(<+) :: Term -> Term -> Either String Value
(<+) lhs rhs = interpret (addLeft lhs rhs)

-- Addition operator.
--   Recurses on the right argument, also known as right addition.
(+>) :: Term -> Term -> Either String Value
(+>) lhs rhs = interpret (addRight lhs rhs)

-- Add two terms using the left addition combinator
addLeft :: Term -> Term -> Term
addLeft lhs rhs = apply (YComb addLeftTerm) [lhs, rhs]

-- Add two terms using the right addition combinator
addRight :: Term -> Term -> Term
addRight lhs rhs = apply (YComb addRightTerm) [lhs, rhs]

-- Combinator for left addition.
--   Recurses on its first argument until it is 0.
addLeftTerm :: Term
addLeftTerm =
  Lambda
    "f"
    ((Base :-> Base :-> Base), 0)
    ( Lambda
        "x"
        (Base, 0)
        ( Lambda
            "y"
            (Base, 0)
            ( If0
                (Variable "x")
                (Variable "y")
                ( Succ
                    ( ( Apply
                          ( Apply
                              (Variable "f")
                              (Pred (Variable "x"))
                          )
                          (Variable "y")
                      )
                    )
                )
            )
        )
    )

-- Combinator for right addition.
--   Recurses on its second argument until it is 0.
addRightTerm :: Term
addRightTerm =
  Lambda
    "f"
    ((Base :-> Base :-> Base), 0)
    ( Lambda
        "x"
        (Base, 0)
        ( Lambda
            "y"
            (Base, 0)
            ( If0
                (Variable "y")
                (Variable "x")
                ( Succ
                    ( Apply
                        ( Apply
                            (Variable "f")
                            (Pred (Variable "y"))
                        )
                        (Variable "x")
                    )
                )
            )
        )
    )
