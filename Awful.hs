--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Main where
  import Errors
  import Files
  import System.Environment
  main :: IO ()
  main =
    do
      args <- getArgs
      putStrLn
        (case args of
          [arg] ->
            case eval arg of
              Left err -> write_error "input" err
              Right term -> term
          _ -> "Input error.")
--------------------------------------------------------------------------------------------------------------------------------