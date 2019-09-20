--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Errors where
  type Err t = Either Error t
  data Error =
    Int_starting_with_zero Location_0 |
    Missing_char_and_end_quote |
    Missing_end_comment |
    Missing_end_quote |
    Negation_of_int_starting_with_zero Location_0 |
    Newline_inside_quotes Location_0 |
    Parse_error Location_0 |
    Syntactic_type_mismatch (Maybe Name) Syntax_type' Syntax_type' |
    Undefined String String Location_0
      deriving Show
  data Location_0 = Location_0 Integer Integer deriving (Eq, Ord, Show)
  data Location_1 = Location_1 String Location_0 deriving Show
  data Location_2 = Language | File Location_1 deriving Show
  data Name = Name Location_0 String deriving Show
  data Syntax_type' = Arrow_syntax' Syntax_type' Syntax_type' | Expr_syntax' | List_syntax' Syntax_type' | Name_st' String
    deriving (Eq, Show)
  write_error :: String -> Error -> String
  write_error file_name err =
    case err of
      Int_starting_with_zero location -> "Int starting with zero at " ++ write_location_1 file_name location ++ "."
      Missing_char_and_end_quote -> "Missing char and end quote in " ++ file_name ++ "."
      Missing_end_comment -> "Missing end comment in " ++ file_name ++ "."
      Missing_end_quote -> "Missing end quote in " ++ file_name ++ "."
      Negation_of_int_starting_with_zero location ->
        "Negation of int starting with zero at " ++ write_location_1 file_name location ++ "."
      Newline_inside_quotes location -> "Newline inside quotes at " ++ write_location_1 file_name location ++ "."
      Parse_error location -> "Parse error at " ++ write_location_1 file_name location ++ "."
      Syntactic_type_mismatch location_0 syntax_type_0 syntax_type_1 ->
        (
          "Syntactic type mismatch in " ++
          (case location_0 of
            Nothing -> "input"
            Just (Name location_1 f) -> f ++ " at " ++ write_location_1 file_name location_1) ++
          " between " ++
          fst (write_syntax_type syntax_type_0) ++
          " and " ++
          fst (write_syntax_type syntax_type_1) ++
          ".")
      Undefined typ name location ->
        "Undefined " ++ typ ++ " " ++ name ++ " at " ++ write_location_1 file_name location ++ "."
  write_location_0 :: Location_0 -> String
  write_location_0 (Location_0 line char) = show line ++ ":" ++ show char
  write_location_1 :: String -> Location_0 -> String
  write_location_1 file_name location = file_name ++ ":" ++ write_location_0 location
  write_syntax_type :: Syntax_type' -> (String, Bool)
  write_syntax_type syntax_type_0 =
    case syntax_type_0 of
      Arrow_syntax' syntax_type_1 syntax_type_2 ->
        let
          (syntax_type_1', b) = write_syntax_type syntax_type_1
        in
          (
            (
              (case b of
                False -> syntax_type_1'
                True -> "(" ++ syntax_type_1' ++ ")") ++
              " -> " ++
              fst (write_syntax_type syntax_type_2)),
            True)
      Expr_syntax' -> ("Term", False)
      List_syntax' syntax_type_1 -> ("[" ++ fst (write_syntax_type syntax_type_1) ++ "]", False)
      Name_st' name -> (name, False)
--------------------------------------------------------------------------------------------------------------------------------