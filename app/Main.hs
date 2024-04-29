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
            [f] -> f
            [] -> error "Error: no input file"
            _ -> error "expected max. 1 argument"
        )
  src <- readFile filepath
  program <- either fail return (parseProg filepath src)

  let (types, typingLogs) = typecheckProg program
  _ <- traverse putStrLn typingLogs
  _ <- either fail return types

  let results = interpretProg program
  _ <- traverse (uncurry showResult) results
  return ()

showResult :: Result (Term info) -> [String] -> IO ()
showResult term logs = do
  _ <- traverse putStrLn logs
  _ <- print $ case term of
    (Left err) -> "Error: " ++ err
    (Right body) -> show body
  return ()