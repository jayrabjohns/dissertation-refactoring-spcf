module Main where

import SPCF (Term (..), Type (..))

main :: IO ()
main = print exTerm

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