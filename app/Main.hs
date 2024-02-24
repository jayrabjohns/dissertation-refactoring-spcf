module Main where

import SPCF

main :: IO ()
main = do
  leftAdd <- either fail return (5 <+ 3)
  rightAdd <- either fail return (5 +> 3)
  _ <- print $ "left addition: " ++ show leftAdd ++ " -- right addition: " ++ show rightAdd

(<+) :: Int -> Int -> Either String Value
lhs <+ rhs =
  let term = (add addTermLeft (Literal lhs) (Literal rhs))
   in interpret term

(+>) :: Int -> Int -> Either String Value
lhs +> rhs =
  let term = (add addTermRight (Literal lhs) (Literal rhs))
   in interpret term

add :: Term -> Term -> Term -> Term
add addTerm lhs rhs = Apply (Apply (YComb addTerm) lhs) rhs

addTermLeft :: Term
addTermLeft =
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

addTermRight :: Term
addTermRight =
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