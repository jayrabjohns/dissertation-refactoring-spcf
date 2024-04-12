module SPCFSpec where

import qualified Data.Map as Map
import SPCF
import SPCFConsts (info)
import qualified SPCFConsts
import Test.HUnit

tests :: Test
tests =
  TestList
    [ evalVariable,
      evalLambdaAbstraction,
      evalApplication,
      evalSuccessor,
      evalPredecessor,
      evalPredecessorOf0,
      evalTrueIf0,
      evalFalseIfNot0,
      evalNestedTerms,
      evalFixedPointOfLiteral,
      evalFixedPoint,
      evalError,
      evalRightAdd,
      evalLeftAdd,
      catchOnLiteral,
      catchOnError,
      catchOnSimpleAbstraction,
      catchApplicationRhsFirst,
      catchApplicationLhsSecond,
      catchOnIf0Condition,
      catchOnIf0True,
      catchOnIf0False,
      catchFixedPoint
    ]

evalVariable :: Test
evalVariable = do
  let term = Variable info "x"
  let expectedVal = Numeral info 5
  let env = Map.fromList [("x", expectedVal)]
  TestLabel
    "should substitute a variable with a value from the environment"
    $ assertEval term env expectedVal

evalLambdaAbstraction :: Test
evalLambdaAbstraction = do
  let term = (Lambda info "x" (Base :-> Base) (Variable info "x"))
  let env = Map.singleton "x" (Numeral info 5)
  let expectedVal = term
  TestLabel
    "should evaluate an abstraction to a closure containing the abstraction"
    $ assertEval term env expectedVal

evalApplication :: Test
evalApplication =
  do
    let term =
          ( Apply
              info
              (Lambda info "f" (Base :-> Base) (Variable info "f"))
              (Variable info "y")
          )
    let expectedVal = Numeral info 10
    let env = Map.fromList [("y", expectedVal)]
    TestLabel
      "should correctly evaluate application to a numeral"
      $ assertEval term env expectedVal

evalSuccessor :: Test
evalSuccessor = do
  let term = Succ info (Variable info "x")
  let expectedVal = Numeral info 1
  let env = Map.singleton "x" (Numeral info 0)
  TestLabel
    "successor of a numeral should evaluate to the numeral + 1"
    $ assertEval term env expectedVal

evalPredecessor :: Test
evalPredecessor = do
  let term = Pred info $ Variable info "x"
  let expectedVal = Numeral info 0
  let env = Map.singleton "x" (Numeral info 1)
  TestLabel
    "predecessor of a numeral should evaluate to the numeral - 1"
    $ assertEval term env expectedVal

evalPredecessorOf0 :: Test
evalPredecessorOf0 = do
  let term = Pred info (Variable info "x")
  let expectedVal = Numeral info 0
  let env = Map.singleton "x" (Numeral info 0)
  TestLabel
    "predecessor of 0 in the natural numbers should evaluate to 0"
    $ assertEval term env expectedVal

evalTrueIf0 :: Test
evalTrueIf0 = do
  let term =
        If0
          info
          (Variable info "x")
          ( Apply
              info
              (Lambda info "f" (Base :-> Base) (Variable info "f"))
              (Variable info ("y"))
          )
          (Variable info "z")

  let expectedVal = Numeral info 3
  let env = Map.fromList [("x", (Numeral info 0)), ("y", expectedVal), ("z", (Numeral info 6))]
  TestLabel
    "if0 should evaluate to left term when condition is 0"
    $ assertEval term env expectedVal

evalFalseIfNot0 :: Test
evalFalseIfNot0 = do
  let term =
        If0
          info
          (Variable info "x")
          ( Apply
              info
              (Lambda info "f" (Base :-> Base) (Variable info "f"))
              (Variable info "y")
          )
          (Variable info "z")

  let expectedVal = Numeral info 6
  let env = Map.fromList [("x", (Numeral info 1)), ("y", (Numeral info 3)), ("z", (Numeral info 6))]
  TestLabel
    "if0 should evaluate to right term when condition is 0"
    $ assertEval term env expectedVal

evalNestedTerms :: Test
evalNestedTerms = do
  -- \fxy. If0 x then y else (succ (f (pred x)))
  let program =
        Lambda info "f" (Base :-> Base) $
          Lambda info "x" Base $
            Lambda info "y" Base $
              If0
                info
                (Variable info "x")
                (Variable info "y")
                ( Succ
                    info
                    (Apply info (Variable info "f") (Pred info (Variable info "x")))
                )
  let f =
        Lambda info "w" (Base :-> Base) $
          Succ info $
            Succ info $
              Variable info "w"
  let (x, y) = ((Numeral info 5), (Numeral info 6))
  let application = Apply info (Apply info (Apply info program f) x) y
  let expectedVal = Numeral info 7
  TestLabel
    "should evaluate nested expression correctly"
    $ assertEval application emptyEnv expectedVal

evalFixedPointOfLiteral :: Test
evalFixedPointOfLiteral = do
  let term =
        YComb info $
          Lambda info "f" (Base :-> Base) $
            (Numeral info 4)

  let expectedVal = Numeral info 4
  TestLabel
    "fixed point of a literal should evaluate to the given literal"
    $ assertEval term emptyEnv expectedVal

evalFixedPoint :: Test
evalFixedPoint = do
  let addition = SPCFConsts.addLeft (Variable info "f") (Variable info "z")
  let env = Map.fromList [("f", (Numeral info 5)), ("z", (Numeral info 3))]
  let expectedVal = Numeral info 8
  TestLabel
    "should evaluate term using a fixed point combinator avoiding variable capture"
    $ assertEval addition env expectedVal

evalError :: Test
evalError = do
  let term = Error info Error1
  let expectedVal = Error info Error1
  TestLabel
    "should error"
    $ assertEval term emptyEnv expectedVal

evalLeftAdd :: Test
evalLeftAdd = do
  let term = SPCFConsts.addLeft (Error info Error1) (Error info Error2)
  let expectedVal = Error info Error1
  TestLabel
    "should return left error when that is evaluated first"
    $ assertEval term emptyEnv expectedVal

evalRightAdd :: Test
evalRightAdd = do
  let term = SPCFConsts.addRight (Error info Error1) (Error info Error2)
  let expectedVal = Error info Error2
  TestLabel
    "should return right error when that is evaluated first"
    $ assertEval term emptyEnv expectedVal

catchOnLiteral :: Test
catchOnLiteral = do
  let term =
        Catch info $
          Lambda info "x" Base $
            Lambda info "y" Base $
              Lambda info "z" Base (Numeral info 4)
  let expectedVal = Numeral info (4 + 3)
  TestLabel
    "should return constant + the number of arguments when catch evaluates to a constant"
    $ assertEval term emptyEnv expectedVal

catchOnError :: Test
catchOnError = do
  let term =
        Catch info $
          Lambda info "x" Base $
            Lambda info "y" Base $
              Lambda info "z" Base $
                Error info Error1
  let expectedVal = Numeral info 3
  TestLabel
    "should return the number arguments when catch evaluates a constant error"
    $ assertEval term emptyEnv expectedVal

catchOnSimpleAbstraction :: Test
catchOnSimpleAbstraction = do
  let term = Catch info $ Lambda info "f" Base $ Variable info "f"
  let expectedVal = Numeral info 0
  TestLabel
    "should return 1 with 1 argument term"
    $ assertEval term emptyEnv expectedVal

catchApplicationRhsFirst :: Test
catchApplicationRhsFirst = do
  let term =
        Catch info $
          Lambda info "x" (Base :-> Base) $
            Lambda info "y" Base $
              (Apply info (Variable info "x") (Variable info "y"))
  let expectedVal = Numeral info 1
  TestLabel
    "should consider RHS first when catching an application"
    $ assertEval term emptyEnv expectedVal

catchApplicationLhsSecond :: Test
catchApplicationLhsSecond = do
  let term =
        Catch info $
          Lambda info "x" (Base :-> Base) $
            Lambda info "y" Base $
              (Apply info (Variable info "x") (Numeral info 0))
  let expectedVal = Numeral info 0
  TestLabel
    "should consider LHS second when catching an applicaiton of a constant"
    $ assertEval term emptyEnv expectedVal

catchOnIf0Condition :: Test
catchOnIf0Condition = do
  let term = Catch info $ Lambda info "x" Base $ If0 info (Variable info "x") (Error info Error1) (Error info Error2)
  let expectedVal = Numeral info 0
  TestLabel
    "should consider condition first when catching If0"
    $ assertEval term emptyEnv expectedVal

catchOnIf0True :: Test
catchOnIf0True = do
  let term = Catch info $ Lambda info "x" Base (If0 info (Numeral info 0) (Variable info "x") (Error info Error2))
  let expectedVal = Numeral info 0
  TestLabel
    "should consider true path first when catching If0 and the condition is a constant 0"
    $ assertEval term emptyEnv expectedVal

catchOnIf0False :: Test
catchOnIf0False = do
  let term = Catch info $ Lambda info "x" Base $ If0 info (Numeral info 1) (Numeral info 1) (Variable info "x")
  let expectedVal = Numeral info 0
  TestLabel
    "should consider false path when catching If0, the condition, and true path are constant"
    $ assertEval term emptyEnv expectedVal

catchFixedPoint :: Test
catchFixedPoint = do
  let term = Catch info $ YComb info SPCFConsts.addRightTerm
  let expectedVal = Numeral info 2
  TestLabel
    "should return index of argument which is evaluated first"
    $ assertEval term emptyEnv expectedVal

assertEval :: (Eq info) => Term info -> Environment (Term info) -> Term info -> Test
assertEval term env expectedVal =
  TestCase $
    do
      result <- runEvalIO (eval term) env
      assertEqual
        ( "Term doesn't evaluate to the expected result. Term: "
            ++ show term
            ++ ". Environment: "
            ++ show env
        )
        expectedVal
        result
