module SPCFConsts (apply, (<+), (+>), addLeft, addRight, addLeftTerm, addRightTerm) where

import SPCF

-- Apply any number of arguments to a term.
--   A helper function for the construction of nested applications.
apply :: Term info -> [Term info] -> Term info
apply term [] = term
apply term (arg : args) = apply (Apply (termInfo term) term arg) args

-- Prototypical divergent expression
-- omega :: Term info
-- omega = YComb $ Lambda "x" undefined (Variable "x")

-- Addition operator.
--   Recurses on the left argument, also known as left addition.
--   This is a partial function and will error on unsuccessful evaluations.
(<+) :: Term info -> Term info -> Term info
(<+) lhs rhs = either error id val where val = interpret (addLeft lhs rhs)

-- Addition operator.
--   Recurses on the right argument, also known as right addition.
--   This is a partial function and will error on unsuccessful evaluations.
(+>) :: Term info -> Term info -> Term info
(+>) lhs rhs = either error id val where val = interpret (addRight lhs rhs)

-- Add two terms using the left addition combinator
addLeft :: Term info -> Term info -> Term info
addLeft lhs rhs = apply (YComb addLeftTerm) [lhs, rhs]

-- Add two terms using the right addition combinator
addRight :: Term info -> Term info -> Term info
addRight lhs rhs = apply (YComb addRightTerm) [lhs, rhs]

-- Combinator for left addition.
--   Recurses on its first argument until it is 0.
addLeftTerm :: Term info
addLeftTerm =
  Lambda
    "f"
    (Base :-> Base :-> Base)
    ( Lambda
        "x"
        Base
        ( Lambda
            "y"
            Base
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
addRightTerm :: Term info
addRightTerm =
  Lambda
    "f"
    (Base :-> Base :-> Base)
    ( Lambda
        "x"
        Base
        ( Lambda
            "y"
            Base
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
