module Main where

import BoundedSPCF (Term (..), Type (..))

-- import SPCF (Term (..), Type (..))

main :: IO ()
main = print exTerm

boundedTerm :: Term
boundedTerm =
  Lambda
    "x"
    (Base :-> Base)
    ( Variable
        "y"
    )

exTerm :: Term
exTerm =
  Lambda
    "m"
    ((Base :-> Base) :-> (Base :-> Base))
    ( Lambda
        "n"
        ((Base :-> Base) :-> (Base :-> Base))
        ( Lambda
            "f"
            (Base :-> Base)
            ( Lambda
                "x"
                Base
                ( Apply
                    (Apply (Variable "m") (Variable "f"))
                    (Apply (Apply (Variable "n") (Variable "f")) (Variable "x"))
                )
            )
        )
    )