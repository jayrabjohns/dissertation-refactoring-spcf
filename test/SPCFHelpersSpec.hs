module SPCFHelpersSpec where

import qualified Data.Map as Map
import SPCF
import Test.HUnit

tests :: Test
tests =
  TestList
    [ substituteVariable,
      substituteBoundVariable,
      substituteFreeVariable
    ]

exampleAbstraction :: Term
exampleAbstraction =
  Lambda
    "x"
    (Base :-> Base)
    (If0 (Variable "x") (Variable "y") (Pred (Variable "x")))

substituteVariable :: Test
substituteVariable = do
  let term = Variable "a"
  TestLabel
    "substitution of a variable should equal the substitution"
    $ assertSubstitute "a" exampleAbstraction term exampleAbstraction

substituteBoundVariable :: Test
substituteBoundVariable = do
  let term =
        Lambda
          "x"
          (Base :-> Base)
          (If0 (Variable "x") (Variable "x") (Pred (Variable "x")))
  TestLabel
    "should do nothing when substituting a variable bound by a surrounding lambda"
    $ assertSubstitute "x" exampleAbstraction term term

substituteFreeVariable :: Test
substituteFreeVariable = do
  let term = exampleAbstraction
  -- The substituted term contains a free variable, risking capture by
  --   the surrounding term it's being substituted in
  let substitution =
        ( Lambda
            "x"
            (Base :-> Base)
            (Succ (Variable "a"))
        )
  let expectedResult =
        ( Lambda
            "b"
            (Base :-> Base)
            (If0 (Variable "b") (substitution) (Pred (Variable "b")))
        )
  TestLabel
    ( "should perform variable avoiding capture when substituting a "
        ++ "free variable with a term that also has a free variable"
    )
    $ assertSubstitute "y" substitution term expectedResult

assertSubstitute :: Label -> Term -> Term -> Term -> Test
assertSubstitute old new body expectedResult = do
  let result = substitute old new body
  TestCase $
    assertEqual
      "substitution isn't equal to what's expected"
      expectedResult
      result
