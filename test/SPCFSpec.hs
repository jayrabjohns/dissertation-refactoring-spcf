module SPCFSpec where

import qualified Data.Map as Map
import SPCF
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
      evalNestedTermsWithCaptureAvoidance,
      evalFixedPointOfLiteral
      -- evalFixedPoint
    ]

evalVariable :: Test
evalVariable = do
  let term = (Variable "x")
  let expectedVal = Nat 5
  let env = Map.fromList [("x", expectedVal)]
  TestLabel
    "should substitute a variable with a value from the environment"
    $ assertEval term env expectedVal

evalLambdaAbstraction :: Test
evalLambdaAbstraction = do
  let term = (Lambda "x" (Base :-> Base) (Variable "x"))
  let env = Map.singleton "x" (Nat 5)
  let expectedVal = Closure env term
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
    let expectedVal = Nat 10
    let env = Map.fromList [("y", expectedVal)]
    TestLabel
      "should correctly evaluate application to a numeral"
      $ assertEval term env expectedVal

evalSuccessor :: Test
evalSuccessor = do
  let term = Succ (Variable "x")
  let expectedVal = Nat 1
  let env = Map.singleton "x" (Nat 0)
  TestLabel
    "successor of a numeral should evaluate to the numeral + 1"
    $ assertEval term env expectedVal

evalPredecessor :: Test
evalPredecessor = do
  let term = Pred (Variable "x")
  let expectedVal = Nat 0
  let env = Map.singleton "x" (Nat 1)
  TestLabel
    "predecessor of a numeral should evaluate to the numeral - 1"
    $ assertEval term env expectedVal

evalPredecessorOf0 :: Test
evalPredecessorOf0 = do
  let term = Pred (Variable "x")
  let expectedVal = Nat 0
  let env = Map.singleton "x" (Nat 0)
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
  let expectedVal = Nat 3
  let env = Map.fromList [("x", (Nat 0)), ("y", expectedVal), ("z", (Nat 6))]
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
  let expectedVal = Nat 6
  let env = Map.fromList [("x", (Nat 1)), ("y", (Nat 3)), ("z", (Nat 6))]
  TestLabel
    "if0 should evaluate to right term when condition is 0"
    $ assertEval term env expectedVal

evalNestedTerms :: Test
evalNestedTerms = do
  let program =
        ( Lambda
            "f"
            ((Base :-> Base) :-> Base :-> Base :-> Base)
            ( Lambda
                "x"
                (Base :-> Base :-> Base)
                ( Lambda
                    "y"
                    (Base :-> Base)
                    ( If0
                        (Variable "x")
                        (Variable "y")
                        ( Succ
                            (Apply (Variable "f") (Pred (Variable "x")))
                        )
                    )
                )
            )
        )

  let f = (Lambda "w" (Base :-> Base) (Succ (Succ (Variable "w"))))
  let (x, y) = ((Literal 5), (Literal 6))
  let application = (Apply (Apply (Apply program f) x) y)
  let expectedVal = Nat 7
  let env = Map.empty
  TestLabel
    "should evaluate nested expression correctly"
    $ assertEval application env expectedVal

evalNestedTermsWithCaptureAvoidance :: Test
evalNestedTermsWithCaptureAvoidance = do
  -- Apply (x-1) to f
  --   f discards its input and instead uses the free variable 'y'
  --   f adds two to y
  --   the result of f(x) has 1 added to it and is the final result
  --   so program(f, x, y) -> y + 3
  let program =
        ( Lambda
            "f"
            ((Base :-> Base) :-> Base :-> Base :-> Base)
            ( Lambda
                "x"
                (Base :-> Base :-> Base)
                ( Lambda
                    "y"
                    (Base :-> Base)
                    ( If0
                        (Variable "x")
                        (Variable "y")
                        ( Succ
                            (Apply (Variable "f") (Pred (Variable "x")))
                        )
                    )
                )
            )
        )
  let f = (Lambda "x" (Base :-> Base) (Succ (Succ (Variable "y"))))
  let (x, y) = ((Literal 5), (Literal 10))
  let application = (Apply (Apply (Apply program f) x) y)
  let expectedVal = Nat 13
  let env = Map.empty
  TestLabel
    "should avoid variable capture when evaluating nested terms"
    $ assertEval application env expectedVal

-- The variable gets renames in the context but not in sub terms. I dont think bound variable should be renamed?

evalFixedPointOfLiteral :: Test
evalFixedPointOfLiteral = do
  let term =
        ( YComb
            ( Lambda
                "f"
                (Base :-> Base)
                (Literal 4)
            )
        )
  let expectedVal = Nat 4
  let env = Map.empty
  TestLabel
    "fixed point of a literal should evaluate to the given literal"
    $ assertEval term env expectedVal

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

evalFixedPoint :: Test
evalFixedPoint = do
  -- let factorial =
  --       ( Lambda
  --           "f"
  --           (Base :-> Base)
  --           (Literal 4)
  --       )
  -- let term = (YComb factorial)
  -- let expectedVal = Nat 4
  -- let env = Map.empty

  let x = Variable "w"
  let y = Variable "z"
  let addition = add x y
  let env = Map.fromList [("w", (Nat 5)), ("z", (Nat 3))]
  let expectedVal = Nat 8

  TestLabel
    "fixed point"
    -- \$ TestCase (assertEqual " " (add' 3 5) 8)
    $ assertEval addition env expectedVal

assertEval :: Term -> Environment -> Value -> Test
assertEval term env expectedVal =
  TestCase
    ( do
        let result = eval (Closure env term)
        case result of
          Right val ->
            assertEqual
              ( "Term doesn't evaluate to the expected result. Term: "
                  ++ show term
                  ++ ". Environment: "
                  ++ show env
              )
              expectedVal
              val
          Left err -> assertFailure err
    )
