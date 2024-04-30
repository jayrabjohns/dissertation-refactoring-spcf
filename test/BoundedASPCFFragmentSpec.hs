module BoundedASPCFFragmentSpec where

import BoundedSPCF.AST
import BoundedSPCF.AffineTransformation
import BoundedSPCF.Evaluation
import BoundedSPCF.TermManipulation
import BoundedSPCF.Types
import Test.HUnit
import Utils.Environment

tests :: Test
tests =
  TestList
    [ -- injHasCorrectType2,
      -- injHasCorrectType3,
      -- projHasCorrectType2,
      -- projHasCorrectType3,
      -- retractIsObservationallyEquivelant2,
      retractIsObservationallyEquivelant3
    ]

injHasCorrectType2 :: Test
injHasCorrectType2 = do
  let f =
        Lambda "p" (Nat `Cross` Nat) $
          Case
            (Case (Numeral 1) (Variable "p"))
            (Product (Bottom <$ numerals))
  let continuationsType = foldl1 Cross ((Nat :-> Empty) <$ numerals)
  let expectedInjFType = Nat `Cross` continuationsType
  let expectedInjTermFType = ((Nat `Cross` Nat) :-> Empty) :-> Nat `Cross` continuationsType

  let injTermF = injTerm (typeof' f)
  let injF = inj f

  let injTermFType = typeof' injTermF
  let injFType = typeof' injF
  TestList
    [ TestCase $ assertEqual "Inj term doesn't have the correct type" expectedInjTermFType injTermFType,
      TestCase $ assertEqual "Inj result doesn't have the correct type" expectedInjFType injFType
    ]

-- | Test that inj fully trasnforms a term of type order 3
-- | and that correponding injTerm  eturns the correct type
-- | for a single step.
injHasCorrectType3 :: Test
injHasCorrectType3 = do
  let f =
        Lambda "p" (Nat `Cross` Nat `Cross` Nat) $
          Case
            (Case (Numeral 1) (Variable "p"))
            (Product (Bottom <$ numerals))
  let innerContinuationsType = foldl1 Cross ((Nat :-> Empty) <$ numerals)
  let innerInjType = (Nat `Cross` innerContinuationsType)
  let expectedInjFType = Nat `Cross` (innerInjType `Cross` innerInjType)

  let continuationsType2 = foldl1 Cross ((Nat `Cross` Nat :-> Empty) <$ numerals)
  let expectedInjTermFType = ((Nat `Cross` Nat `Cross` Nat) :-> Empty) :-> Nat `Cross` continuationsType2

  let injTermF = injTerm (typeof' f)
  let injF = inj f

  let injTermFType = typeof' injTermF
  let injFType = typeof' injF
  TestList
    [ TestCase $
        assertEqual
          "Inj term doesn't have the correct type"
          expectedInjTermFType
          injTermFType,
      TestCase $
        assertEqual
          "Inj result doesn't have the correct type"
          expectedInjFType
          injFType
    ]

projHasCorrectType2 :: Test
projHasCorrectType2 = do
  let f =
        Lambda "p" (Nat `Cross` Nat) $
          Case
            (Case (Numeral 1) (Variable "p"))
            (Product (Bottom <$ numerals))
  let continuationsType = foldl1 Cross ((Nat :-> Empty) <$ numerals)
  let expectedProjTermType = (Nat `Cross` continuationsType) :-> (Nat `Cross` Nat :-> Empty)
  let expectedAffineType = typeof' f

  let injTermF = injTerm (typeof' f)
  let injF = Apply injTermF f
  let projTermF = projTerm (typeof' injF)
  let projTermType = typeof' projTermF

  let affineF = proj . inj $ f
  let affineFType = typeof' affineF

  TestList
    [ TestCase $
        assertEqual
          "Proj term doesn't have the correct type"
          expectedProjTermType
          projTermType,
      TestCase $
        assertEqual
          "The projection of f should have the same type as f"
          expectedAffineType
          affineFType
    ]

projHasCorrectType3 :: Test
projHasCorrectType3 = do
  let f =
        Lambda "p" (Nat `Cross` Nat `Cross` Nat) $
          Case
            (Case (Numeral 1) (Variable "p"))
            (Product (Bottom <$ numerals))

  let continuationsType = foldl1 Cross ((Nat `Cross` Nat :-> Empty) <$ numerals)
  let expectedProjTermType = (Nat `Cross` continuationsType) :-> (Nat `Cross` Nat `Cross` Nat :-> Empty)
  let expectedAffineType = typeof' f

  let injTermF = injTerm (typeof' f)
  let injF = Apply injTermF f
  let projTermF = projTerm (typeof' injF)
  let projTermType = typeof' projTermF

  let affineF = proj . inj $ f
  let affineFType = typeof' affineF

  TestList
    [ TestCase $
        assertEqual
          "Proj term doesn't have the correct type"
          expectedProjTermType
          projTermType,
      TestCase $
        assertEqual
          "The projection of f should have the same type as f"
          expectedAffineType
          affineFType
    ]

retractIsObservationallyEquivelant2 :: Test
retractIsObservationallyEquivelant2 = do
  let f =
        Lambda "p" (Nat `Cross` Nat) $
          Case
            (Case (Numeral 1) (Variable "p"))
            (Product (Bottom <$ numerals))
  let projInjF = proj . inj $ f
  let allArgCombinations = [Product [x, y] | x <- numerals, y <- numerals]
  TestList
    [ assertSameResult f projInjF arg
      | arg <- allArgCombinations
    ]

retractIsObservationallyEquivelant3 :: Test
retractIsObservationallyEquivelant3 = do
  let f =
        Lambda "p" (Nat `Cross` Nat `Cross` Nat) $
          Case
            (Case (Numeral 1) (Variable "p"))
            (Product (Bottom <$ numerals))
  let affineF = proj . inj $ f
  let allArgCombinations =
        [ Product [x, y, z]
          | x <- numerals,
            y <- numerals,
            z <- numerals
        ]
  TestList
    [ assertSameResult f affineF arg
      | arg <- allArgCombinations
    ]

assertSameResult :: Term -> Term -> Term -> Test
assertSameResult term1 term2 arg = TestCase $ do
  _ <- putStrLn ""
  _ <- putStrLn ("term1 = " ++ show term1)
  _ <- putStrLn ""
  _ <- putStrLn ("term2 size = " ++ show (length (show term2)))
  _ <- putStrLn ("term2 = " ++ show (normalise term2))
  result1 <- runEvalIO (eval (Apply term1 arg)) empty
  result2 <- runEvalIO (eval (Apply term2 arg)) empty
  assertEqual
    ( "Terms don't evaluate to the same result. Term: "
        ++ ("term1 evaluates to " ++ show result1 ++ "\n")
        ++ ("term2 evaluates to " ++ show result2 ++ ". \n")
    )
    result1 -- (Numeral 1) -- result1
    result2

assertEval :: Term -> Environment Term -> Term -> Test
assertEval term env expectedTerm = TestCase $ do
  result <- runEvalIO (eval term) env
  assertEqual
    ( "Term doesn't evaluate to the expected result. "
        ++ ("Term: " ++ show term ++ ". ")
        ++ ("Environment: " ++ show env)
    )
    expectedTerm
    result