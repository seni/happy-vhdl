module Main where

import Expr (run)

main = do
  in_str <- getLine
  putStr "Expr\n"
  putStr $ (show $ run in_str) ++ "\n\n"
