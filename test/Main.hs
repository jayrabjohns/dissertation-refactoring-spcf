module Main (main) where

import qualified MonadicExampleSpec
import qualified SPCFHelpersSpec
import qualified SPCFSpec
import qualified SimpleExampleSpec
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
    [ -- SimpleExampleSpec.tests,
      --   MonadicExampleSpec.tests,
      -- SPCFHelpersSpec.tests,
      SPCFSpec.tests
    ]