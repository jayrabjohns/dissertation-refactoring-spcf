module SPCFSpec where

import qualified Data.Map as Map
import SPCF
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
      catchIsErrorSensitive,
      catchOnIf0Condition,
      catchOnIf0True,
      catchOnIf0False,
      catchFixedPoint
    ]

evalVariable :: Test
evalVariable = do
  let term = (Variable "x")
  let expectedVal = Numeral 5
  let env = Map.fromList [("x", expectedVal)]
  TestLabel
    "should substitute a variable with a value from the environment"
    $ assertEval term env expectedVal

evalLambdaAbstraction :: Test
evalLambdaAbstraction = do
  let term = (Lambda "x" (Base :-> Base) (Variable "x"))
  let env = Map.singleton "x" (Numeral 5)
  let expectedVal = term
  TestLabel
    "should evaluate an abstraction to a closure containing the abstraction"
    $ assertEval term env expectedVal

evalApplication :: Test
evalApplication =
  do
    let term =
          ( Apply
              (Lambda "f" (Base :-> Base) (Variable "f"))
              (Variable ("y"))
          )
    let expectedVal = Numeral 10
    let env = Map.fromList [("y", expectedVal)]
    TestLabel
      "should correctly evaluate application to a numeral"
      $ assertEval term env expectedVal

evalSuccessor :: Test
evalSuccessor = do
  let term = Succ (Variable "x")
  let expectedVal = Numeral 1
  let env = Map.singleton "x" (Numeral 0)
  TestLabel
    "successor of a numeral should evaluate to the numeral + 1"
    $ assertEval term env expectedVal

evalPredecessor :: Test
evalPredecessor = do
  let term = Pred (Variable "x")
  let expectedVal = Numeral 0
  let env = Map.singleton "x" (Numeral 1)
  TestLabel
    "predecessor of a numeral should evaluate to the numeral - 1"
    $ assertEval term env expectedVal

evalPredecessorOf0 :: Test
evalPredecessorOf0 = do
  let term = Pred (Variable "x")
  let expectedVal = Numeral 0
  let env = Map.singleton "x" (Numeral 0)
  TestLabel
    "predecessor of 0 in the natural numbers should evaluate to 0"
    $ assertEval term env expectedVal

evalTrueIf0 :: Test
evalTrueIf0 = do
  let term =
        ( If0
            (Variable "x")
            ( Apply
                (Lambda "f" (Base :-> Base) (Variable "f"))
                (Variable ("y"))
            )
            (Variable "z")
        )
  let expectedVal = Numeral 3
  let env = Map.fromList [("x", (Numeral 0)), ("y", expectedVal), ("z", (Numeral 6))]
  TestLabel
    "if0 should evaluate to left term when condition is 0"
    $ assertEval term env expectedVal

evalFalseIfNot0 :: Test
evalFalseIfNot0 = do
  let term =
        ( If0
            (Variable "x")
            ( Apply
                (Lambda "f" (Base :-> Base) (Variable "f"))
                (Variable ("y"))
            )
            (Variable "z")
        )
  let expectedVal = Numeral 6
  let env = Map.fromList [("x", (Numeral 1)), ("y", (Numeral 3)), ("z", (Numeral 6))]
  TestLabel
    "if0 should evaluate to right term when condition is 0"
    $ assertEval term env expectedVal

-- \fxy. If0 x then y else (succ (f (pred x)))
conditionalWithHigherOrderFuncTerm :: Term
conditionalWithHigherOrderFuncTerm =
  Lambda
    "f"
    (Base :-> Base)
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
                    (Apply (Variable "f") (Pred (Variable "x")))
                )
            )
        )
    )

evalNestedTerms :: Test
evalNestedTerms = do
  let program = conditionalWithHigherOrderFuncTerm
  let f = (Lambda "w" (Base :-> Base) (Succ (Succ (Variable "w"))))
  let (x, y) = ((Numeral 5), (Numeral 6))
  let application = (Apply (Apply (Apply program f) x) y)
  let expectedVal = Numeral 7
  TestLabel
    "should evaluate nested expression correctly"
    $ assertEval application emptyEnv expectedVal

evalFixedPointOfLiteral :: Test
evalFixedPointOfLiteral = do
  let term =
        ( YComb
            ( Lambda
                "f"
                (Base :-> Base)
                (Numeral 4)
            )
        )
  let expectedVal = Numeral 4
  TestLabel
    "fixed point of a literal should evaluate to the given literal"
    $ assertEval term emptyEnv expectedVal

-- fix :: (a -> a) -> a
-- fix f = x where x = f x

-- fact :: Int -> Int
-- fact n = (fix fac) n
--   where
--     fac _ 0 = 1
--     fac f x = x * f (x - 1)

-- add' :: Int -> Int -> Int
-- add' x y = (fix aux) x y
--   where
--     aux _ 0 y = y
--     aux f x y = f (x - 1) (y + 1)

-- YComb :: (a -> a) -> a
--       :: ((b -> b -> b) -> (b -> b -> b)) -> (b -> b -> b)
addTerm :: Term
addTerm =
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
                ( Apply
                    ( Apply
                        (Variable "f")
                        (Pred (Variable "x"))
                    )
                    (Succ (Variable "y"))
                )
            )
        )
    )

add :: Term -> Term -> Term
add x y = Apply (Apply (YComb (addTerm)) x) y

evalFixedPoint :: Test
evalFixedPoint = do
  let addition = add (Variable "f") (Variable "z")
  let env = Map.fromList [("f", (Numeral 5)), ("z", (Numeral 3))]
  let expectedVal = Numeral 8
  TestLabel
    "should evaluate term using a fixed point combinator avoiding variable capture"
    $ assertEval addition env expectedVal

evalError :: Test
evalError = do
  let term = Error Error1
  let expectedVal = Error Error1
  TestLabel
    "should error"
    $ assertEval term emptyEnv expectedVal

evalLeftAdd :: Test
evalLeftAdd = do
  let term = SPCFConsts.addLeft (Error Error1) (Error Error2)
  let expectedVal = Error Error1
  TestLabel
    "should return left error when that is evaluated first"
    $ assertEval term emptyEnv expectedVal

evalRightAdd :: Test
evalRightAdd = do
  let term = SPCFConsts.addRight (Error Error1) (Error Error2)
  let expectedVal = Error Error2
  TestLabel
    "should return right error when that is evaluated first"
    $ assertEval term emptyEnv expectedVal

catchOnLiteral :: Test
catchOnLiteral = do
  let term = Catch $ Lambda "x" Base $ Lambda "y" Base $ Lambda "z" Base (Numeral 4)
  let expectedVal = Numeral (4 + 3)
  TestLabel
    "should return constant + the number of arguments when catch evaluates to a constant"
    $ assertEval term emptyEnv expectedVal

catchOnError :: Test
catchOnError = do
  let term = Catch $ Lambda "x" Base $ Lambda "y" Base $ Lambda "z" Base (Error Error2)
  let expectedVal = Error Error2
  TestLabel
    "should return constant + the number of arguments when catch evaluates to a constant"
    $ assertEval term emptyEnv expectedVal

catchOnSimpleAbstraction :: Test
catchOnSimpleAbstraction = do
  let term = Catch $ Lambda "f" Base $ Variable "f"
  let expectedVal = Numeral 0
  TestLabel
    "should return 1 with 1 argument term"
    $ assertEval term emptyEnv expectedVal

catchApplicationRhsFirst :: Test
catchApplicationRhsFirst = do
  let term = Catch $ Lambda "x" (Base :-> Base) $ Lambda "y" Base $ (Apply (Variable "x") (Variable "y"))
  let expectedVal = Numeral 1
  TestLabel
    "should consider RHS first when catching an application"
    $ assertEval term emptyEnv expectedVal

catchApplicationLhsSecond :: Test
catchApplicationLhsSecond = do
  let term = Catch $ Lambda "x" (Base :-> Base) $ Lambda "y" Base $ (Apply (Variable "x") (Numeral 0))
  let expectedVal = Numeral 0
  TestLabel
    "should consider LHS second when catching an applicaiton of a constant"
    $ assertEval term emptyEnv expectedVal

catchIsErrorSensitive :: Test
catchIsErrorSensitive = do
  let term = Catch $ SPCFConsts.addLeft (Error Error1) (Error Error2)
  let expectedVal = Error Error2
  TestLabel
    "should return error as soon as it is encountered when catching term"
    $ assertEval term emptyEnv expectedVal

-- This doesn't type check because programs in the SPCF are closed and must
--   return o. If this wasn't the case, x would have to have type (o -> o) -> o.
-- let term = Catch $ SPCFConsts.addLeft (Error Error1) (Error Error2)
-- catchApplication :: Test
-- catchApplication = do
--   let identity = Lambda "z" Base (Variable "z")
--   let term = Catch $ Apply (Variable "x") identity
--   let expectedVal = Nat 3
--   TestLabel
--     "should consider RHS first when catching an application"
--     $ assertEval term emptyEnv expectedVal

catchOnIf0Condition :: Test
catchOnIf0Condition = do
  let term = Catch $ Lambda "x" Base $ If0 (Variable "x") (Error Error1) (Error Error2)
  let expectedVal = Numeral 0
  TestLabel
    "should consider condition first when catching If0"
    $ assertEval term emptyEnv expectedVal

catchOnIf0True :: Test
catchOnIf0True = do
  let term = Catch $ Lambda "x" Base (If0 (Numeral 0) (Variable "x") (Error Error2))
  let expectedVal = Numeral 0
  TestLabel
    "should consider true path first when catching If0 and the condition is a constant 0"
    $ assertEval term emptyEnv expectedVal

catchOnIf0False :: Test
catchOnIf0False = do
  let term = Catch $ Lambda "x" Base $ If0 (Numeral 1) (Numeral 1) (Variable "x")
  let expectedVal = Numeral 0
  TestLabel
    "should consider false path when catching If0, the condition, and true path are constant"
    $ assertEval term emptyEnv expectedVal

catchFixedPoint :: Test
catchFixedPoint = do
  let term = Catch $ YComb SPCFConsts.addRightTerm
  let expectedVal = Numeral 2
  TestLabel
    "should return index of argument which is evaluated first"
    $ assertEval term emptyEnv expectedVal

assertEval :: Term -> Environment -> Term -> Test
assertEval term env expectedVal =
  TestCase
    ( do
        result <- runEvalIO (eval term) env
        assertEqual
          ( "Term doesn't evaluate to the expected result. Term: "
              ++ show term
              ++ ". Environment: "
              ++ show env
          )
          expectedVal
          result
    )
