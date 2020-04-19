--------------------------------------------------------------------------------------------------------------------------------
module Main where
  import System.Process (callCommand)
  main :: IO ()
  main =
    do
      callCommand "ghc Awful.hs"
      callCommand "Awful \"Run (Algebra.test, Language.test, Standard.test)\""
--------------------------------------------------------------------------------------------------------------------------------