module Main where

import Frontend.Parser (parseProg)
import SPCF.AST (Term)
import SPCF.Interpreter (Result, interpretProg, typecheckProg)
import System.Environment (getArgs)

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

  let (types, typingLogs) = typecheckProg program
  _ <- traverse putStrLn typingLogs
  _ <- either fail return types

  let results = interpretProg program
  _ <- traverse (print . showResult) results
  return ()

showResult :: Result (Term info) -> String
showResult (Left err) = "Error: " ++ err
showResult (Right term) = show term
