--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Main where
  import Errors
  import Files
  import System.Environment
  import Tree
  main :: IO ()
  main =
    do
      args <- getArgs
      f <-
        case args of
          [arg] ->
            case parse_input arg of
              Left e -> return (write_error e)
              Right a ->
                case a of
                  Check b -> checks b
                  Eval b c ->
                    do
                      d <- eval'' b c
                      return
                        (case d of
                          Left e -> write_error e
                          Right e -> e)
          _ -> return "Awful requires one command line argument."
      putStrLn f
--------------------------------------------------------------------------------------------------------------------------------