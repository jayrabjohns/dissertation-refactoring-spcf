module Main (main) where

import qualified ASPCFFragmentSpec
import qualified BoundedASPCFFragmentSpec
import qualified BoundedSPCFSpec
import qualified BoundedSPCFTypesSpec
import qualified ProductSpec
import qualified SPCFSpec
import qualified SPCFTermManipulationSpec
import qualified System.Exit as Exit
import Test.HUnit (Counts (errors, failures), Test (TestList), runTestTT)

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 || errors result > 0
    then Exit.exitFailure
    else Exit.exitSuccess

tests :: Test
tests =
  TestList
    [ -- ASPCFFragmentSpec.tests
      -- SPCFSpec.tests,
      -- SPCFTermManipulationSpec.tests,
      -- ProductSpec.tests
      -- BoundedSPCFSpec.tests,
      -- BoundedSPCFTypesSpec.tests
      BoundedASPCFFragmentSpec.tests
    ]