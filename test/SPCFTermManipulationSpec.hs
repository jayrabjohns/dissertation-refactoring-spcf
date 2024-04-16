module SPCFTermManipulationSpec where

import Frontend.Lexer (AlexPosn (..))
import SPCF.AST
import SPCF.Constants (info)
import SPCF.TermManipulation
import Test.HUnit

tests :: Test
tests =
  TestList
    [ substituteVariable,
      substituteBoundVariable,
      substituteFreeVariable
    ]

exampleAbstraction :: Term AlexPosn
exampleAbstraction =
  Lambda info "x" (Base :-> Base) $
    (If0 info (Variable info "x") (Variable info "y") (Pred info (Variable info "x")))

substituteVariable :: Test
substituteVariable = do
  let term = Variable info "a"
  TestLabel
    "substitution of a variable should equal the substitution"
    $ assertSubstitute "a" exampleAbstraction term exampleAbstraction

substituteBoundVariable :: Test
substituteBoundVariable = do
  let term =
        Lambda info "x" (Base :-> Base) $
          ( If0
              info
              (Variable info "x")
              (Variable info "x")
              (Pred info (Variable info "x"))
          )
  TestLabel
    "should do nothing when substituting a variable bound by a surrounding lambda"
    $ assertSubstitute "x" exampleAbstraction term term

substituteFreeVariable :: Test
substituteFreeVariable = do
  let term = exampleAbstraction
  -- The substituted term contains a free variable, risking capture by
  --   the surrounding term it's being substituted in
  let substitution =
        Lambda info "x" (Base :-> Base) $
          (Succ info (Variable info "a"))

  let expectedResult =
        Lambda info "b" (Base :-> Base) $
          (If0 info (Variable info "b") (substitution) (Pred info (Variable info "b")))

  TestLabel
    ( "should perform variable avoiding capture when substituting a "
        ++ "free variable with a term that also has a free variable"
    )
    $ assertSubstitute "y" substitution term expectedResult

assertSubstitute :: (Eq info) => Label -> Term info -> Term info -> Term info -> Test
assertSubstitute old new body expectedResult = do
  let result = substitute old new body
  TestCase $
    assertEqual
      "substitution isn't equal to what's expected"
      expectedResult
      result
