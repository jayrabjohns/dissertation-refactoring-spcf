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
  let expectedType = Base `Cross` Base `Cross` Base
  let typ = runJudgement (typeof prod) emptyContext
  TestCase $
    assertEqual "" expectedType typ

typeCase :: Test
typeCase = do
  let prod = Product [Numeral 1, Numeral 2, Numeral 3, Numeral 4]
  let term = Case (Numeral 1) prod
  let expectedType = Base
  let result = runJudgement (typeof term) emptyContext
  TestCase $
      assertEqual "" expectedType result

typeLongFunction :: Test
typeLongFunction = do
  let term = Lambda "x" Base $ Lambda "y" Base $ Lambda "z" Base $ Lambda "w" Base $ (Variable "x")
  let expectedType = Base :-> Base :-> Base :-> Base :-> Base
  let result = runJudgement (typeof term) emptyContext
  TestCase $
      assertEqual "" expectedType result

typeFunction :: Test
typeFunction = do
    let term = Lambda "p" (Base `Cross` Base `Cross` Base) $ Lambda "i" Base $ Case (Variable "i") (Variable "p")
    let expectedType = (Base `Cross` Base `Cross` Base) :-> Base :-> Base
    let typ = runJudgement (typeof term) emptyContext
    TestCase $
      assertEqual "" expectedType typ