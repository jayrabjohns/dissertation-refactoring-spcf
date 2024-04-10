module Main where

import Frontend.Parser
-- import Preprocessor (importLines, substImports)
import SPCF
import SPCFTypes
import System.Environment (getArgs)

-- import Tycheck (runTycheck, tycheckProg)

main :: IO ()
main = do
  args <- getArgs
  let filepath =
        ( case args of
            [] -> error "Error: no input file"
            [f] -> f
            _ -> error "expected max. 1 argument"
        )
  src <- readFile filepath
  program <- either fail return (parseProg filepath src)
  let results = interpProg program
  _ <- traverse (print . showResult) results
  return ()

showResult :: Result (Term info) -> String
showResult (Left err) = "Error: " ++ err
showResult (Right term) = show term

-- main :: IO ()
-- main = do
--   leftAdd <- interpretIO (addLeft 5 3)
--   rightAdd <- interpretIO (addRight 5 3)
--   _ <- print $ "5 +l 3 = " ++ show leftAdd
--   _ <- print $ "5 +r 3 = " ++ show rightAdd
--   let leftAddError = (Error Error1 <+ Error Error2)
--   let rightAddError = (Error Error1 +> Error Error2)
--   _ <- print $ "Error1 +l Error2 = " ++ show leftAddError
--   print $ "Error1 +r Error2 = " ++ show rightAddError
