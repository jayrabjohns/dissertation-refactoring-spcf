module Main where

import SPCF
import SPCFConsts

main :: IO ()
main = do
  leftAdd <- either fail return (5 <+ 3)
  rightAdd <- either fail return (5 +> 3)
  _ <- print $ "5 +l 3: " ++ show leftAdd ++ " -- 5 +r 3: " ++ show rightAdd
  leftAddError <- either fail return (Error Error1 <+ Error Error2)
  rightAddError <- either fail return (Error Error1 +> Error Error2)
  print $ "(+l) Error1 Error2: " ++ show leftAddError ++ " -- (+r) Error1 Error2: " ++ show rightAddError
