--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Main where
  import Errors
  import Files
  main :: IO ()
  main =
    do
      putStrLn "Testing the built-in functions."
      test_Add_Int
      test_Compare_Int
      test_Convert_Int
      test_Div
      test_Mod
      test_Times_Int
  test :: String -> String -> IO ()
  test input result =
    case eval input >>= test' result of
      Left err -> putStrLn ("Failed test " ++ input ++ ". " ++ write_error "input" err)
      Right () -> return ()
  test' :: String -> String -> Err ()
  test' result result' =
    case result == result' of
      False -> Left (Expected result result')
      True -> Right ()
  test_Add_Int :: IO ()
  test_Add_Int =
    do
      test "Add -2 -2" "-4"
      test "Add -2 -1" "-3"
      test "Add -2 0" "-2"
      test "Add -2 1" "-1"
      test "Add -2 2" "0"
      test "Add -1 -2" "-3"
      test "Add -1 -1" "-2"
      test "Add -1 0" "-1"
      test "Add -1 1" "0"
      test "Add -1 2" "1"
      test "Add 0 -2" "-2"
      test "Add 0 -1" "-1"
      test "Add 0 0" "0"
      test "Add 0 1" "1"
      test "Add 0 2" "2"
      test "Add 1 -2" "-1"
      test "Add 1 -1" "0"
      test "Add 1 0" "1"
      test "Add 1 1" "2"
      test "Add 1 2" "3"
      test "Add 2 -2" "0"
      test "Add 2 -1" "1"
      test "Add 2 0" "2"
      test "Add 2 1" "3"
      test "Add 2 2" "4"
  test_Compare_Int :: IO ()
  test_Compare_Int =
    do
      test "Compare -2 -2" "EQ"
      test "Compare -2 -1" "LT"
      test "Compare -2 0" "LT"
      test "Compare -2 1" "LT"
      test "Compare -2 2" "LT"
      test "Compare -1 -2" "GT"
      test "Compare -1 -1" "EQ"
      test "Compare -1 0" "LT"
      test "Compare -1 1" "LT"
      test "Compare -1 2" "LT"
      test "Compare 0 -2" "GT"
      test "Compare 0 -1" "GT"
      test "Compare 0 0" "EQ"
      test "Compare 0 1" "LT"
      test "Compare 0 2" "LT"
      test "Compare 1 -2" "GT"
      test "Compare 1 -1" "GT"
      test "Compare 1 0" "GT"
      test "Compare 1 1" "EQ"
      test "Compare 1 2" "LT"
      test "Compare 2 -2" "GT"
      test "Compare 2 -1" "GT"
      test "Compare 2 0" "GT"
      test "Compare 2 1" "GT"
      test "Compare 2 2" "EQ"
  test_Convert_Int :: IO ()
  test_Convert_Int =
    do
      test "Convert -2" "-2"
      test "Convert -1" "-1"
      test "Convert 0" "0"
      test "Convert 1" "1"
      test "Convert 2" "2"
  test_Div :: IO ()
  test_Div =
    do
      test "Div -2 1" "-2"
      test "Div -1 1" "-1"
      test "Div 0 1" "0"
      test "Div 1 1" "1"
      test "Div 2 1" "2"
      test "Div -2 2" "-1"
      test "Div -1 2" "-1"
      test "Div 0 2" "0"
      test "Div 1 2" "0"
      test "Div 2 2" "1"
  test_Mod :: IO ()
  test_Mod =
    do
      test "Mod -2 1" "0"
      test "Mod -1 1" "0"
      test "Mod 0 1" "0"
      test "Mod 1 1" "0"
      test "Mod 2 1" "0"
      test "Mod -2 2" "0"
      test "Mod -1 2" "1"
      test "Mod 0 2" "0"
      test "Mod 1 2" "1"
      test "Mod 2 2" "0"
  test_Times_Int :: IO ()
  test_Times_Int =
    do
      test "Times -2 -2" "4"
      test "Times -2 -1" "2"
      test "Times -2 0" "0"
      test "Times -2 1" "-2"
      test "Times -2 2" "-4"
      test "Times -1 -2" "2"
      test "Times -1 -1" "1"
      test "Times -1 0" "0"
      test "Times -1 1" "-1"
      test "Times -1 2" "-2"
      test "Times 0 -2" "0"
      test "Times 0 -1" "0"
      test "Times 0 0" "0"
      test "Times 0 1" "0"
      test "Times 0 2" "0"
      test "Times 1 -2" "-2"
      test "Times 1 -1" "-1"
      test "Times 1 0" "0"
      test "Times 1 1" "1"
      test "Times 1 2" "2"
      test "Times 2 -2" "-4"
      test "Times 2 -1" "-2"
      test "Times 2 0" "0"
      test "Times 2 1" "2"
      test "Times 2 2" "4"
--------------------------------------------------------------------------------------------------------------------------------