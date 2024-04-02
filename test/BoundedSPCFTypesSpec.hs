module BoundedSPCFTypesSpec where

import BoundedSPCF
import BoundedSPCFTypes
import qualified Data.Map as Map
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
    let env = Map.fromList [("i", Numeral 1)]
    let judegement = runJudgement (BoundedSPCFTypes.typeof term) emptyContext
    TestCase $ do
      typ <- runEvalIO judegement env
      assertEqual "" expectedType typ