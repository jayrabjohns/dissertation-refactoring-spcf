module Main where

import SPCF
import SPCFConsts

main :: IO ()
main = do
  leftAdd <- interpretIO (addLeft 5 3)
  rightAdd <- interpretIO (addRight 5 3)
  _ <- print $ "5 +l 3 = " ++ show leftAdd
  _ <- print $ "5 +r 3 = " ++ show rightAdd
  leftAddError <- either fail return (Error Error1 <+ Error Error2)
  rightAddError <- either fail return (Error Error1 +> Error Error2)
  _ <- print $ "Error1 +l Error2 = " ++ show leftAddError
  print $ "Error1 +r Error2 = " ++ show rightAddError
