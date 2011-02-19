module Main where

import Prolog_list (run)

main = do
  in_str <- getLine
  putStr "Prolog list\n"
  putStr $ (show $ run in_str) ++ "\n\n"
