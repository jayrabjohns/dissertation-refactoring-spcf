module ProductSpec where

import BoundedSPCF.AST
import Test.HUnit

tests :: Test
tests =
  TestList
    [ insert,
      remove
    ]

insert :: Test
insert = do
  let prod = map Numeral [1 .. 5]
  let updatedProduct = insertProduct prod (Numeral 10) 3
  let expected = map Numeral [1, 2, 3, 10, 4, 5]
  TestLabel
    "should insert an element into a pre-existing product"
    $ assertProduct updatedProduct expected

remove :: Test
remove = do
  let prod = map Numeral [0 .. 5]
  let updatedProduct = removeProduct prod 3
  let expected = map Numeral [0, 1, 2, 4, 5]
  TestLabel
    "should remove an element into a pre-existing product"
    $ assertProduct updatedProduct expected

assertProduct :: Product -> Product -> Test
assertProduct term expectedTerm =
  TestCase $
    assertEqual
      ( "Term doesn't equal the expected result."
          ++ "\n Term: "
          ++ show term
          ++ "\n Expected term: "
          ++ show expectedTerm
      )
      expectedTerm
      term
