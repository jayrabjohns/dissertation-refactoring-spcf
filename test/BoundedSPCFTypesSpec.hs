module BoundedSPCFTypesSpec where

import BoundedSPCF
import BoundedSPCFTypes
import Test.HUnit

tests :: Test
tests =
  TestList
    [ BoundedSPCFTypesSpec.typeof
    ]

typeof :: Test
typeof =
  do
    let term = Lambda "p" (Base `Cross` Base `Cross` Base) $ Lambda "i" Base $ Case (Variable "i") (Variable "p")
    let expectedType = (Base `Cross` Base `Cross` Base) :-> Base :-> (Base `Cross` Base `Cross` Base)
    let typ = runJudgement (BoundedSPCFTypes.typeof term) emptyContext
    TestCase $
      assertEqual "" expectedType typ