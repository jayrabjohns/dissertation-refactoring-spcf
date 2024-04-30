module BoundedSPCFTypesSpec where

import BoundedSPCF.AST
import BoundedSPCF.Types
import Test.HUnit
import Utils.Environment

tests :: Test
tests =
  TestList
    [ typeProduct,
      typeComplexProduct,
      typeCase,
      typeLongFunction,
      typeFunction
    ]

typeProduct :: Test
typeProduct = do
  let prod = Product [Numeral 1, Numeral 2, Numeral 3]
  let expectedType = Cross Nat 3
  let typ = runJudgement (typeof prod) empty
  TestCase $
    assertEqual "" expectedType typ

typeComplexProduct :: Test
typeComplexProduct = do
  let prod =
        Lambda "f" (Nat :-> Empty) $
          Case (Numeral 0) $
            Product
              [ BinProduct
                  (Numeral 0)
                  ( Product
                      [ Lambda "empty" Unit $
                          Apply (Variable "f") (Product [j])
                        | j <- numerals
                      ]
                  )
              ]
  let expectedType = (Nat :-> Empty) :-> Pair Nat (Cross (Unit :-> Empty) 2)
  let typ = runJudgement (typeof prod) empty
  TestCase $
    assertEqual "" expectedType typ

typeCase :: Test
typeCase = do
  let prod = Product [Numeral 1, Numeral 2, Numeral 3, Numeral 4]
  let term = Case (Numeral 1) prod
  let expectedType = Nat
  let result = runJudgement (typeof term) empty
  TestCase $
    assertEqual "" expectedType result

typeLongFunction :: Test
typeLongFunction = do
  let term = Lambda "x" Nat $ Lambda "y" Nat $ Lambda "z" Nat $ Lambda "w" Nat $ (Variable "x")
  let expectedType = Nat :-> Nat :-> Nat :-> Nat :-> Nat
  let result = runJudgement (typeof term) empty
  TestCase $
    assertEqual "" expectedType result

typeFunction :: Test
typeFunction = do
  let term = Lambda "p" (Cross Nat 3) $ Lambda "i" Nat $ Case (Variable "i") (Variable "p")
  let expectedType = (Cross Nat 3) :-> Nat :-> Nat
  let typ = runJudgement (typeof term) empty
  TestCase $
    assertEqual "" expectedType typ