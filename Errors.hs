--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE StandaloneDeriving #-}
module Errors (
  Err,
  Err',
  Error (..),
  Error' (..),
  Eval_err,
  Evaluation_error (..),
  Language_or_location (..),
  Line_and_char,
  Type_err,
  Type_error (..),
  Type_error_location (..),
  add_file_name,
  init_line_and_char,
  next_char,
  next_line,
  write_error) where
  import Data.Bifunctor (first)
  import Data.List (intercalate)
  type Err = Either Error
  type Err' = Either Error'
  data Error =
    Circular_dependency_between_files [String] |
    Conflicting_definitions_of_term String Language_or_location String Line_and_char |
    Error String Error' |
    Evaluation_error Evaluation_error |
    Input_error |
    Type_error_in_input Type_error
      deriving Show
  data Error' =
    Failed_to_find_file String Line_and_char |
    Int_starting_with_zero Line_and_char |
    Invalid_character Line_and_char |
    Missing_end_comment |
    Negation_of_int_starting_with_zero Line_and_char |
    Parse_error Line_and_char |
    Run_error Line_and_char Evaluation_error |
    Test_failed Line_and_char String String |
    Type_error Type_error_location Line_and_char Type_error |
    Undefined String String Line_and_char
      deriving Show
  type Eval_err = Either Evaluation_error
  data Evaluation_error = Crashed | Not_writeable
      deriving Show
  data Language_or_location = Language | Location String Line_and_char
      deriving Show
  data Line_and_char = Line_and_char Integer Integer
      deriving Show
  type Type_err = Either Type_error
  data Type_error = Type_mismatch | Unresolved_type_variables
      deriving Show
  data Type_error_location = Definition String | Test
      deriving Show
  deriving instance Eq Line_and_char
  instance Monoid Line_and_char where
    mempty = init_line_and_char
  deriving instance Ord Line_and_char
  instance Semigroup Line_and_char where
    (<>) = max
  add_file_name :: String -> Err' t -> Err t
  add_file_name file_name = first (Error file_name)
  init_line_and_char :: Line_and_char
  init_line_and_char = Line_and_char 1 1
  next_char :: Line_and_char -> Line_and_char
  next_char (Line_and_char line char) = Line_and_char line (char + 1)
  next_line :: Line_and_char -> Line_and_char
  next_line (Line_and_char line _) = Line_and_char (line + 1) 1
  write_error :: Error -> String
  write_error err =
    case err of
      Circular_dependency_between_files file_names ->
        "Circular dependency between files [" ++ intercalate "," file_names ++ "]."
      Conflicting_definitions_of_term name language_or_location file_name line_and_char ->
        (
          "Conflicting definitions of term " ++
          name ++
          (case language_or_location of
            Language -> " in the language and at "
            Location file_name' line_and_char' ->
              " at " ++ write_file_name_and_line_and_char file_name' line_and_char' ++ " and ") ++
          write_file_name_and_line_and_char file_name line_and_char ++
          ".")
      Error file_name err' ->
        case err' of
          Failed_to_find_file name line_and_char ->
            (
              "Failed to find file " ++
              name ++
              " requested at " ++
              write_file_name_and_line_and_char file_name line_and_char ++
              ".")
          Int_starting_with_zero line_and_char ->
            "Int starting with zero at " ++ write_file_name_and_line_and_char file_name line_and_char ++ "."
          Invalid_character line_and_char ->
            "Invalid character at " ++ write_file_name_and_line_and_char file_name line_and_char ++ "."
          Missing_end_comment -> "Missing end comment in " ++ file_name ++ "."
          Negation_of_int_starting_with_zero line_and_char ->
            "Negation of int starting with zero at " ++ write_file_name_and_line_and_char file_name line_and_char ++ "."
          Parse_error line_and_char -> "Parse error at " ++ write_file_name_and_line_and_char file_name line_and_char ++ "."
          Run_error line_and_char err'' ->
            case err'' of
              Crashed -> "Test at " ++ write_file_name_and_line_and_char file_name line_and_char ++ " crashed."
              Not_writeable ->
                "The terms in test at " ++ write_file_name_and_line_and_char file_name line_and_char ++ " are not comparable."
          Test_failed line_and_char term_0 term_1 ->
            "Test at " ++ write_file_name_and_line_and_char file_name line_and_char ++ " failed. " ++ term_0 ++ " = " ++ term_1
          Type_error location line_and_char err'' ->
            (
              write_type_error err'' ++
              " in " ++
              write_type_error_location location ++
              " at " ++
              write_file_name_and_line_and_char file_name line_and_char ++
              ".")
          Undefined typ name line_and_char ->
            "Undefined " ++ typ ++ " " ++ name ++ " at " ++ write_file_name_and_line_and_char file_name line_and_char ++ "."
      Evaluation_error err' ->
        case err' of
          Crashed -> "Evaluation crashed."
          Not_writeable -> "The result is not writeable."
      Input_error -> "Input error."
      Type_error_in_input err' -> write_type_error err' ++ " in input."
  write_file_name_and_line_and_char :: String -> Line_and_char -> String
  write_file_name_and_line_and_char file_name line_and_char = file_name ++ ":" ++ write_line_and_char line_and_char
  write_line_and_char :: Line_and_char -> String
  write_line_and_char (Line_and_char line char) = show line ++ ":" ++ show char
  write_type_error :: Type_error -> String
  write_type_error err =
    case err of
      Type_mismatch -> "Type mismatch"
      Unresolved_type_variables -> "Unresolved type variables"
  write_type_error_location :: Type_error_location -> String
  write_type_error_location location =
    case location of
      Definition name -> "definition " ++ name
      Test -> "test"
--------------------------------------------------------------------------------------------------------------------------------