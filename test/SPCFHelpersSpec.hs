module SPCFHelpersSpec where

import qualified Data.Map as Map
import SPCF
import Test.HUnit

tests :: Test
tests =
  TestList
    [substituteVariable]

exampleAbstraction :: Term
exampleAbstraction =
  Lambda
    "x"
    (Base :-> Base)
    (If0 (Variable "x") (Variable "x") (Pred (Variable "x")))

substituteVariable :: Test
substituteVariable = do
  let term = Variable "a"
  let sub =
        Lambda
          "x"
          (Base :-> Base)
          (If0 (Variable "x") (Variable "x") (Pred (Variable "x")))
  TestLabel
    "substitution of a variable should equal the substitution"
    $ assertSubstitute "a" sub term sub

substituteAbstraction :: Test
substituteAbstraction = do
  let term =
        Lambda
          "x"
          (Base :-> Base)
          (If0 (Variable "x") (Variable "x") (Pred (Variable "x")))

assertSubstitute :: Label -> Term -> Term -> Term -> Test
assertSubstitute old new body expectedResult =
  do
    let result = substitute old new body
    TestCase $
      assertEqual
        "substitution isn't equal to what's expected"
        expectedResult
        result
