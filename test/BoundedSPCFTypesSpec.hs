module BoundedSPCFTypesSpec where

import BoundedSPCF
import BoundedSPCFTypes
import Test.HUnit

tests :: Test
tests =
  TestList
    [ typeProduct,
      typeCase,
      typeLongFunction,
      typeFunction
    ]

typeProduct :: Test
typeProduct = do
  let prod = Product [Numeral 1, Numeral 2, Numeral 3]
  let expectedType = Nat `Cross` Nat `Cross` Nat
  let typ = runJudgement (typeof prod) emptyContext
  TestCase $
    assertEqual "" expectedType typ

typeCase :: Test
typeCase = do
  let prod = Product [Numeral 1, Numeral 2, Numeral 3, Numeral 4]
  let term = Case (Numeral 1) prod
  let expectedType = Nat
  let result = runJudgement (typeof term) emptyContext
  TestCase $
    assertEqual "" expectedType result

typeLongFunction :: Test
typeLongFunction = do
  let term = Lambda "x" Nat $ Lambda "y" Nat $ Lambda "z" Nat $ Lambda "w" Nat $ (Variable "x")
  let expectedType = Nat :-> Nat :-> Nat :-> Nat :-> Nat
  let result = runJudgement (typeof term) emptyContext
  TestCase $
    assertEqual "" expectedType result

typeFunction :: Test
typeFunction = do
  let term = Lambda "p" (Nat `Cross` Nat `Cross` Nat) $ Lambda "i" Nat $ Case (Variable "i") (Variable "p")
  let expectedType = (Nat `Cross` Nat `Cross` Nat) :-> Nat :-> Nat
  let typ = runJudgement (typeof term) emptyContext
  TestCase $
    assertEqual "" expectedType typ