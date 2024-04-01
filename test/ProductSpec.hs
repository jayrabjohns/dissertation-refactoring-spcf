module ProductSpec where

import BoundedSPCF
import Test.HUnit

tests :: Test
tests =
  TestList
    [ ProductSpec.insertProduct,
      ProductSpec.removeProduct
    ]

insertProduct :: Test
insertProduct = do
  let prod = map Numeral [1 .. 5]
  let updatedProduct = BoundedSPCF.insertProduct prod (Numeral 10) 3
  let expected = map Numeral [1, 2, 3, 10, 4, 5]
  TestLabel
    "should insert an element into a pre-existing product"
    $ assertProduct updatedProduct expected

removeProduct :: Test
removeProduct = do
  let prod = map Numeral [1 .. 5]
  let updatedProduct = BoundedSPCF.removeProduct prod 3
  let expected = map Numeral [1, 2, 3, 5]
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
