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
    [ -- countProductLength,
      -- countProductLengthOfOne,
      -- countBinaryProductLength,
      -- countProductLengthOfConstant
      -- injHasCorrectType1
      -- injHasCorrectType2
      -- injHasCorrectType3
      -- projHasCorrectType1
      -- projHasCorrectType2
      -- projHasCorrectType3
      -- retractIsObservationallyEquivelant2
      retractIsObservationallyEquivelant3
    ]

-- countProductLength :: Test
-- countProductLength = do
--   let prod = Product [Numeral 1, Numeral 1, Numeral 1]
--   let expected = 3
--   let prodLen = productLength (typeof' prod)
--   TestCase $
--     assertEqual "should correctly determine length of product" expected prodLen

-- countProductLengthOfOne :: Test
-- countProductLengthOfOne = do
--   let prod = Product [Numeral 1]
--   let expected = 1
--   let prodLen = productLength (typeof' prod)
--   TestCase $
--     assertEqual "should correctly determine length of product" expected prodLen

-- countProductLengthOfConstant :: Test
-- countProductLengthOfConstant = do
--   let prod = Numeral 1
--   let expected = 1
--   let prodLen = productLength (typeof' prod)
--   TestCase $
--     assertEqual "should correctly determine length of product" expected prodLen

-- countBinaryProductLength :: Test
-- countBinaryProductLength = do
--   let prod = BinProduct (Numeral 1) (Product [BinProduct (Numeral 1) (Lambda "x" Nat (Variable "x")), BinProduct (Numeral 1) (Lambda "x" Nat (Variable "x"))])
--   let expected = 2
--   let prodLen = productLength (typeof' prod)
--   TestCase $
--     assertEqual "should correctly determine length of product" expected prodLen

-- countFunctionArguments :: Test
-- countFunctionArguments = do
--   let func = Lambda "x" Nat $ Lambda "y" (Nat `Cross` Nat) $ Variable "x"
--   let expected = 2
--   let numArgs = arity (typeof' func)
--   TestCase $
--     assertEqual "should correctly determine number of function arguments" expected numArgs

-- injHasCorrectType0 :: Test
-- injHasCorrectType0 = do
--   let f =
--         Lambda "p" (Cross Nat 0) $
--           Case
--             (Case (Numeral 0) (Variable "p"))
--             (Product (Bottom <$ numerals))
--   let expectedType = Unit
--   let injType = typeof' $ inj f
--   TestCase $
--     assertEqual
--       "should type nullary case for inj"
--       expectedType
--       injType

injHasCorrectType1 :: Test
injHasCorrectType1 = do
  let f =
        Lambda "p" (Cross Nat 1) $
          Case
            (Case (Numeral 0) (Variable "p"))
            (Product (Bottom <$ numerals))
  let expectedType = Pair Nat (Cross (Unit :-> Empty) 2)
  let injType = typeof' $ inj f
  TestCase $
    assertEqual
      "should type nullary case for inj"
      expectedType
      injType

injHasCorrectType2 :: Test
injHasCorrectType2 = do
  let f =
        Lambda "p" (Cross Nat 2) $
          Case
            (Case (Numeral 1) (Variable "p"))
            (Product (Bottom <$ numerals))
  let expectedInjFType =
        Pair Nat $
          Cross
            (Pair Nat $ Cross (Unit :-> Empty) upperBound)
            upperBound
  let expectedInjTermFType =
        ((Cross Nat 2) :-> Empty)
          :-> (Pair Nat $ Cross (Cross Nat 1 :-> Empty) upperBound)

  let injTermF = injTerm (typeof' f)
  let injF = inj f

  let injTermFType = typeof' injTermF
  let injFType = typeof' injF
  TestList
    [ TestCase $
        assertEqual
          "Inj term doesn't have the correct type: inj f = "
          expectedInjTermFType
          injTermFType,
      TestCase $
        assertEqual
          "Inj result doesn't have the correct type"
          expectedInjFType
          injFType
    ]

-- | Test that inj fully trasnforms a term of type order 3
-- | and that the correponding injTerm returns the correct type
-- | for a single step.
injHasCorrectType3 :: Test
injHasCorrectType3 = do
  let f =
        Lambda "p" (Cross Nat 3) $
          Case
            (Case (Numeral 1) (Variable "p"))
            (Product (Bottom <$ numerals))
  let innerInjType =
        Pair Nat $
          Cross (Pair Nat (Cross (Unit :-> Empty) upperBound)) upperBound
  let expectedInjFType = Pair Nat (Cross innerInjType upperBound)
  let expectedInjTermFType =
        ((Cross Nat 3) :-> Empty)
          :-> (Pair Nat $ Cross ((Cross Nat 2) :-> Empty) upperBound)

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

projHasCorrectType1 :: Test
projHasCorrectType1 = do
  let f =
        Lambda "p" (Cross Nat 1) $
          Case
            (Case (Numeral 0) (Variable "p"))
            (Product (Bottom <$ numerals))
  let expectedProjTermType =
        (Pair Nat (Cross (Unit :-> Empty) upperBound))
          :-> (Cross Nat 1 :-> Empty)
  let fType = typeof' f

  let injTermF = injTerm fType
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
          fType
          affineFType
    ]

projHasCorrectType2 :: Test
projHasCorrectType2 = do
  let f =
        Lambda "p" (Cross Nat 2) $
          Case
            (Case (Numeral 1) (Variable "p"))
            (Product (Bottom <$ numerals))
  let fType = typeof' f
  let expectedInjType =
        Pair Nat $
          Cross ((Cross Nat 1 :-> Empty)) upperBound
  let expectedProjTermType = expectedInjType :-> (Cross Nat 2 :-> Empty)

  -- let injTermF = injTerm fType
  let injF = inj' f
  let projTermF = projTerm (typeof' injF)
  let projTermType = typeof' projTermF

  let affineF = (proj . inj) f
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
          fType
          affineFType
    ]

projHasCorrectType3 :: Test
projHasCorrectType3 = do
  let f =
        Lambda "p" (Cross Nat 3) $
          Case
            (Case (Numeral 1) (Variable "p"))
            (Product (Bottom <$ numerals))
  let fType = typeof' f
  let affineF = (proj . inj) f
  let affineFType = typeof' affineF

  TestCase $
    assertEqual
      "The projection of f should have the same type as f"
      fType
      affineFType

retractIsObservationallyEquivelant2 :: Test
retractIsObservationallyEquivelant2 = do
  let f =
        Lambda "p" (Cross Nat 2) $
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
        Lambda "p" (Cross Nat 3) $
          Case
            (Case (Numeral 1) (Variable "p"))
            (Product (Bottom <$ numerals))
  let affineF = (proj . inj) f
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
  _ <- print ""
  _ <- print ("arg size = " ++ show (length (show arg)))
  _ <- print ("arg = " ++ show arg)
  _ <- print ("term1 size = " ++ show (length (show term1)))
  -- _ <- putStrLn ("term1 = " ++ show term1)
  _ <- print ("term2 size = " ++ show (length (show term2)))
  -- _ <- putStrLn ("term2 = " ++ show term2)
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