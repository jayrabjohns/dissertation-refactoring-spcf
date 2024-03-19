module Main (main) where

-- import qualified BoundedASPCFFragmentSpec
-- import qualified BoundedSPCFSpec
import qualified SPCFSpec
import qualified System.Exit as Exit
import Test.HUnit

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 || errors result > 0
    then Exit.exitFailure
    else Exit.exitSuccess

tests :: Test
tests =
  TestList
    [ -- SPCFHelpersSpec.tests,
      SPCFSpec.tests
      -- BoundedSPCFSpec.tests
      -- BoundedASPCFFragmentSpec.tests
    ]