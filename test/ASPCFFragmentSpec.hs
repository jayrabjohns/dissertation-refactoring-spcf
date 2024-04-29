module ASPCFFragmentSpec where

import SPCF.AST
import SPCF.AffineTransformation
import SPCF.Constants
import SPCF.Evaluation
import SPCF.TermManipulation
import SPCF.Types
import Test.HUnit
import Utils.Environment

tests :: Test
tests =
  TestList
    [ injectionHasCorrectType2
    --   injectionHasCorrectType3,
    --   projectionHasCorrectType2,
    --   projectionHasCorrectType3,
    --   retractIsObservationallyEquivelant2,
    --   retractIsObservationallyEquivelant3
    ]

injectionHasCorrectType2 :: Test
injectionHasCorrectType2 = do
  let f =
        Lambda info "x" (Base `Cross` Base) $
          If0
            info
            (projection (Variable info "x") 0)
            ( If0
                info
                (projection (Variable info "x") 1)
                (Bottom info)
                (Bottom info)
            )
            (Bottom info)
  let injTermF = injTerm (typeof' f)
  let result = typeof' injTermF
  -- let continuationsType = foldl1 Cross ((Nat :-> Empty) <$ numerals)
  -- let expected = ((Nat `Cross` Nat) :-> Empty) :-> (Nat `Cross` continuationsType)
  TestCase $ do
    _ <- print injTermF
    assertEqual "" Base result

-- injectionHasCorrectType3 :: Test
-- injectionHasCorrectType3 = do
--   let f =
--         Lambda "p" (Nat `Cross` Nat `Cross` Nat) $
--           Case
--             (Case (Numeral 1) (Variable "p"))
--             (Product (Bottom <$ numerals))
--   let injTermF = injTerm (typeof' f)
--   let result = typeof' injTermF
--   let continuationsType = foldl1 Cross ((Nat `Cross` Nat :-> Empty) <$ numerals)
--   let expected = ((Nat `Cross` Nat `Cross` Nat) :-> Empty) :-> (Nat `Cross` continuationsType)
--   TestCase $
--     assertEqual "" expected result

-- projectionHasCorrectType2 :: Test
-- projectionHasCorrectType2 = do
--   let f =
--         Lambda "p" (Nat `Cross` Nat) $
--           Case
--             (Case (Numeral 1) (Variable "p"))
--             (Product (Bottom <$ numerals))
--   let projTermF = projTerm (typeof' (inj f))
--   let result = typeof' projTermF
--   let continuationsType = foldl1 Cross ((Nat :-> Empty) <$ numerals)
--   let expected = (Nat `Cross` continuationsType) :-> (Nat `Cross` Nat :-> Empty)
--   TestCase $
--     assertEqual "" expected result

-- projectionHasCorrectType3 :: Test
-- projectionHasCorrectType3 = do
--   let f =
--         Lambda "p" (Nat `Cross` Nat `Cross` Nat) $
--           Case
--             (Case (Numeral 1) (Variable "p"))
--             (Product (Bottom <$ numerals))
--   let projTermF = projTerm (typeof' (inj f))
--   let result = typeof' projTermF
--   let continuationsType = foldl1 Cross ((Nat `Cross` Nat :-> Empty) <$ numerals)
--   let expected = (Nat `Cross` continuationsType) :-> (Nat `Cross` Nat `Cross` Nat :-> Empty)
--   TestCase $
--     assertEqual "" expected result

-- retractIsObservationallyEquivelant2 :: Test
-- retractIsObservationallyEquivelant2 = do
--   let f =
--         Lambda "p" (Nat `Cross` Nat) $
--           Case
--             (Case (Numeral 1) (Variable "p"))
--             (Product (Bottom <$ numerals))
--   let projInjF = proj (inj f)
--   let allArgCombinations =
--         [ Product [x, y]
--           | x <- numerals,
--             y <- numerals
--         ]
--   TestList
--     [ assertSameResult (Apply f arg) (Apply projInjF arg)
--       | arg <- allArgCombinations
--     ]

-- retractIsObservationallyEquivelant3 :: Test
-- retractIsObservationallyEquivelant3 = do
--   let f =
--         Lambda "p" (Nat `Cross` Nat `Cross` Nat) $
--           Case
--             (Case (Numeral 1) (Variable "p"))
--             (Product (Bottom <$ numerals))
--   let projInjF = proj (inj f)
--   let allArgCombinations =
--         [ Product [x, y, z]
--           | x <- numerals,
--             y <- numerals,
--             z <- numerals
--         ]
--   TestList
--     [ assertSameResult (Apply f arg) (Apply projInjF arg)
--       | arg <- allArgCombinations
--     ]

assertSameResult :: (Eq info) => Term info -> Term info -> Test
assertSameResult term1 term2 = TestCase $ do
  _ <- putStrLn ("term1 = " ++ show (normalise term1))
  _ <- putStrLn ("term2 = " ++ show (normalise term2))
  result1 <- runEvalIO (eval term1) empty
  result2 <- runEvalIO (eval term2) empty
  assertEqual
    ( "Terms don't evaluate to the same result. Term: "
        ++ (show term1 ++ "Evaluates to " ++ show result1 ++ "\n")
        ++ (show term2 ++ "Evaluates to " ++ show result2 ++ ". \n")
    )
    result1
    result2

assertEval :: (Eq info) => Term info -> Environment (Term info) -> Term info -> Test
assertEval term env expectedTerm = TestCase $ do
  result <- runEvalIO (eval term) env
  assertEqual
    ( "Term doesn't evaluate to the expected result. "
        ++ ("Term: " ++ show term ++ ". ")
        ++ ("Environment: " ++ show env)
    )
    expectedTerm
    result