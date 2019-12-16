--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Errors (Err, Error (..), Language_or_location (..), Line_and_char (..), write_error) where
  type Err = Either Error
  data Error =
    Conflicting_definitions_of_term String Language_or_location Line_and_char |
    Evaluation_crashed |
    Expected String String |
    Int_starting_with_zero Line_and_char |
    Negation_of_int_starting_with_zero Line_and_char |
    Parse_error Line_and_char |
    The_result_is_not_writeable |
    Type_mismatch |
    Undefined String String Line_and_char
      deriving Show
  data Language_or_location = Language | Location String Line_and_char deriving Show
  data Line_and_char = Line_and_char Integer Integer deriving (Eq, Ord, Show)
  write_error :: String -> Error -> String
  write_error file_name err =
    case err of
      Conflicting_definitions_of_term name language_or_location line_and_char ->
        (
          "Conflicting definitions of term " ++
          name ++
          (case language_or_location of
            Language -> " in the language and at "
            Location file_name' line_and_char' ->
              " at " ++ write_file_name_and_line_and_char file_name' line_and_char' ++ " and ") ++
          write_file_name_and_line_and_char file_name line_and_char ++
          ".")
      Evaluation_crashed -> "Evaluation crashed."
      Expected term term' -> "Expected " ++ term ++ ", got " ++ term' ++ "."
      Int_starting_with_zero line_and_char ->
        "Int starting with zero at " ++ write_file_name_and_line_and_char file_name line_and_char ++ "."
      Negation_of_int_starting_with_zero line_and_char ->
        "Negation of int starting with zero at " ++ write_file_name_and_line_and_char file_name line_and_char ++ "."
      Parse_error line_and_char -> "Parse error at " ++ write_file_name_and_line_and_char file_name line_and_char ++ "."
      The_result_is_not_writeable -> "The result is not writeable."
      Type_mismatch -> "Type mismatch in input."
      Undefined typ name line_and_char ->
        "Undefined " ++ typ ++ " " ++ name ++ " at " ++ write_file_name_and_line_and_char file_name line_and_char ++ "."
  write_file_name_and_line_and_char :: String -> Line_and_char -> String
  write_file_name_and_line_and_char file_name line_and_char = file_name ++ ":" ++ write_line_and_char line_and_char
  write_line_and_char :: Line_and_char -> String
  write_line_and_char (Line_and_char line char) = show line ++ ":" ++ show char
--------------------------------------------------------------------------------------------------------------------------------