--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Main where
  import Data.Set
  import Dictionary
  import Errors
  import Eval
  import Files
  import Tree
  import Typing
{- Standard.awf
Add {Arrow, Logical, Pair, Unit}
And {Arrow, Logical, Modular, Pair, Unit}
Add_all
All {Either, Logical, Maybe, Modular, Ordering, Pair, Unit}
And
And_all
Apply {Arrow, Either, List, Maybe, Pair}
Apply_2 {Either, Pair}
Apply_discard
Apply_left
Apply_right
Associate_left
Associate_right {Either, Pair}
Bind
Commute {Either, Pair}
Compare {Either, List, Logical, Maybe, Ordering, Pair, Unit}
Compose {Arrow, Either, List, Maybe}
Compose_discard_left
Compose_discard_right
Contains
Convert {Arrow, Logical, Pair, Unit}
Empty {List, Maybe}
Equal
Field {Modular}
Filter {List, Maybe}
Flatten {Arrow, Either, List, Maybe}
Flip {Either, List, Maybe}
Fold_Either
Fold_List
Fold_Logical
Fold_Maybe
Fold_Pair
Fold_Unit
Fold_right {Either, List, Maybe, Pair}
Greater
Greater_or_equal
Id
Identity
Inverse {Int, Logical, Pair, Unit}
Is_empty
Is_zero
Less
Less_or_equal
Lift {Arrow, Either, List, Maybe}
List
Max {Arrow, Logical, Modular, Pair, Unit}
Min {Arrow, Logical, Modular, Pair, Unit}
Negate
Not {Arrow, Logical, Modular, Pair, Unit}
Not_empty
Not_equal
Not_zero
Or {Arrow, Logical, Modular, Pair, Unit}
Or_all
Size {List, Maybe}
Times {Arrow, Logical, Pair, Unit}
-}
  main :: IO ()
  main = _
  test ::
    (
      (
        (Set String, Dictionary Language_or_location, Dictionary Language_or_location),
        Dictionary Kind_0,
        (Dictionary Constructor_3, Dictionary Polymorphic_type),
        Dictionary Polymorphic_term,
        Dictionary (Dictionary [[String]]),
        Dictionary Operator_0) ->
      String ->
      ([String], String) ->
      [String])
  test (a0, a1, a2, a3, a4, a5) f (args, res) =
    let
      str = f ++ intercalate " " args
    in
      case parse parse_term "input" str of
        Left _ -> ["Error in test " ++ str ++ "."]
        Right x ->
          case tokenise_parse_naming_typing_eval a0 a1 a2 a3 x a4 a5 of
            Left e -> ["Error in test " ++ str ++ "."]
            Right y ->
              case res == y of
                False -> ["Wrong result in test " ++ str ++ ". Expected " ++ res ++ ", the result was " ++ y ++ " instead."]
                True -> []
  tests ::
    (
      (
        (Set String, Dictionary Language_or_location, Dictionary Language_or_location),
        Dictionary Kind_0,
        (Dictionary Constructor_3, Dictionary Polymorphic_type),
        Dictionary Polymorphic_term,
        Dictionary (Dictionary [[String]]),
        Dictionary Operator_0) ->
      String ->
      [([String], String)] ->
      [String])
  tests a f cases = cases >>= test a f
  test_Add_Int ::
    (
      (
        (Set String, Dictionary Language_or_location, Dictionary Language_or_location),
        Dictionary Kind_0,
        (Dictionary Constructor_3, Dictionary Polymorphic_type),
        Dictionary Polymorphic_term,
        Dictionary (Dictionary [[String]]),
        Dictionary Operator_0) ->
      [String])
  test_Add_Int a =
    tests
      a
      "Add"
      [
        (["-1", "-1"], "-2"),
        (["-1", "0"], "-1"),
        (["-1", "1"], "0"),
        (["0", "-1"], "-1"),
        (["0", "0"], "0"),
        (["0", "1"], "1"),
        (["1", "-1"], "0"),
        (["1", "0"], "1"),
        (["1", "1"], "2")]
  test_Add_Modular :: [String]
  test_Add_Modular = test_Add_Modular_1 ++ test_Add_Modular_2 ++ test_Add_Modular_3 ++ test_Add_Modular_4
  test_Add_Modular_1 :: [String]
  test_Add_Modular_1 = _
  test_Add_Modular_2 :: [String]
  test_Add_Modular_2 = _
  test_Add_Modular_3 :: [String]
  test_Add_Modular_3 = _
  test_Add_Modular_4 :: [String]
  test_Add_Modular_4 = _
  test_Compare_Int :: [String]
  test_Compare_Int = _
  test_Compare_Modular :: [String]
  test_Compare_Modular = test_Compare_Modular_1 ++ test_Compare_Modular_2 ++ test_Compare_Modular_3 ++ test_Compare_Modular_4
  test_Compare_Modular_1 :: [String]
  test_Compare_Modular_1 = _
  test_Compare_Modular_2 :: [String]
  test_Compare_Modular_2 = _
  test_Compare_Modular_3 :: [String]
  test_Compare_Modular_3 = _
  test_Compare_Modular_4 :: [String]
  test_Compare_Modular_4 = _
  test_Convert_Int :: [String]
  test_Convert_Int = _
  test_Convert_Modular :: [String]
  test_Convert_Modular = test_Convert_Modular_1 ++ test_Convert_Modular_2 ++ test_Convert_Modular_3 ++ test_Convert_Modular_4
  test_Convert_Modular_1 :: [String]
  test_Convert_Modular_1 = _
  test_Convert_Modular_2 :: [String]
  test_Convert_Modular_2 = _
  test_Convert_Modular_3 :: [String]
  test_Convert_Modular_3 = _
  test_Convert_Modular_4 :: [String]
  test_Convert_Modular_4 = _
  test_Div :: [String]
  test_Div = _
  test_Inverse_Modular :: [String]
  test_Inverse_Modular = test_Inverse_Modular_1 ++ test_Inverse_Modular_2 ++ test_Inverse_Modular_3 ++ test_Inverse_Modular_4
  test_Inverse_Modular_1 :: [String]
  test_Inverse_Modular_1 = _
  test_Inverse_Modular_2 :: [String]
  test_Inverse_Modular_2 = _
  test_Inverse_Modular_3 :: [String]
  test_Inverse_Modular_3 = _
  test_Inverse_Modular_4 :: [String]
  test_Inverse_Modular_4 = _
  test_Mod :: [String]
  test_Mod = _
  test_Times_Int :: [String]
  test_Times_Int = _
  test_Times_Modular :: [String]
  test_Times_Modular = test_Times_Modular_1 ++ test_Times_Modular_2 ++ test_Times_Modular_3 ++ test_Times_Modular_4
  test_Times_Modular_1 :: [String]
  test_Times_Modular_1 = _
  test_Times_Modular_2 :: [String]
  test_Times_Modular_2 = _
  test_Times_Modular_3 :: [String]
  test_Times_Modular_3 = _
  test_Times_Modular_4 :: [String]
  test_Times_Modular_4 = _
  test_language :: [String]
  test_language =
    (
      test_Add_Int _ ++
      test_Add_Modular ++
      test_Compare_Int ++
      test_Compare_Modular ++
      test_Convert_Int ++
      test_Convert_Modular ++
      test_Div ++
      test_Inverse_Modular ++
      test_Mod ++
      test_Times_Modular)
--------------------------------------------------------------------------------------------------------------------------------