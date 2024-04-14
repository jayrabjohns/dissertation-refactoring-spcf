module BoundedASPCFFragmentSpec where

import BoundedASPCF
import BoundedSPCF
import BoundedSPCFTypes
import Test.HUnit

tests :: Test
tests =
  TestList
    [ injectionHasCorrectType,
      retractIsObservationallyEquivelant
    ]

injectionHasCorrectType :: Test
injectionHasCorrectType = do
  let f =
        Lambda "p" (Nat `Cross` Nat) $
          Case
            (Case (Numeral 1) (Variable "p"))
            (Product $ [Bottom, Bottom])
  let injTermF = injTerm (typeof' f)
  let result = typeof' injTermF
  let expected = ((Nat `Cross` Nat) :-> Empty) :-> (Nat `Cross` ((Nat :-> Empty) `Cross` (Nat :-> Empty)))
  TestCase $ do
    assertEqual "" expected result

retractIsObservationallyEquivelant :: Test
retractIsObservationallyEquivelant = do
  let f =
        Lambda "p" (Nat `Cross` Nat) $
          Case
            (Case (Numeral 1) (Variable "p"))
            (Product $ [Bottom, Bottom])
  let projInjF = proj (inj f)
  let allArgCombinations =
        [ Product [Numeral x, Numeral y]
          | x <- [0 .. upperBound - 1],
            y <- [0 .. upperBound - 1]
        ]
  TestList
    [ assertSameResult (Apply f arg) (Apply projInjF arg)
      | arg <- allArgCombinations
    ]

assertSameResult :: Term -> Term -> Test
assertSameResult term1 term2 = TestCase $ do
  _ <- putStrLn ("term1 = " ++ show (normalise term1))
  _ <- putStrLn ("term2 = " ++ show (normalise term2))
  result1 <- runEvalIO (eval term1) emptyEnv
  result2 <- runEvalIO (eval term2) emptyEnv
  assertEqual
    ( "Terms don't evaluate to the same result. Term: "
        ++ (show term1 ++ "Evaluates to " ++ show result1 ++ "\n")
        ++ (show term2 ++ "Evaluates to " ++ show result2 ++ ". \n")
        ++ ("Environment: " ++ show emptyEnv)
    )
    result1
    result2

assertEval :: Term -> Environment -> Term -> Test
assertEval term env expectedTerm = TestCase $ do
  result <- runEvalIO (eval term) env
  assertEqual
    ( "Term doesn't evaluate to the expected result. "
        ++ ("Term: " ++ show term ++ ". ")
        ++ ("Environment: " ++ show env)
    )
    expectedTerm
    result