--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Errors (Err, Error (..), Language_or_location (..), Line_and_char (..), Loc (..), Name (..), write_error) where
  import Data.List
  import Modular
  type Err = Either Error
  data Error =
    Circular_dependency_between_files [String] |
    Circular_inheritance_between_classes String [String] |
    Conflicting_definitions String String Language_or_location String Line_and_char |
    Conflicting_instances String String Language_or_location String Line_and_char |
    Constructor_has_been_given_a_wrong_number_of_arguments String String Line_and_char |
    Def_err String String Line_and_char String |
    Duplicate_import_of_file String String Line_and_char Line_and_char |
    Expected String String Line_and_char String |
    Failed_to_find_the_file String String Line_and_char |
    Illegal_instance String String String Line_and_char String |
    Int_starting_with_zero String Line_and_char |
    Invalid_modular Modular_0 String Line_and_char |
    Kind_mismatch String Line_and_char |
    Kind_mismatch_in_class String Line_and_char |
    Kind_mismatch_in_constraint String Line_and_char String String |
    Method_requires_instance_or_constraint String String (Maybe Name) Line_and_char String String |
    Missing_definition String String String String Line_and_char |
    Missing_end_comment String |
    Mixed_operator_associativities String String Line_and_char Line_and_char |
    Parse_error String Line_and_char |
    Pattern_error String Line_and_char |
    Too_many_arguments String Line_and_char String |
    Type_mismatch Loc |
    Type_variable_is_not_of_kind_Nat String String Line_and_char |
    Undefined String String String Line_and_char |
    Unresolved_type_variables Loc
      deriving Show
  data Language_or_location = Language | Location String Line_and_char deriving Show
  data Line_and_char = Line_and_char Integer Integer deriving (Eq, Ord, Show)
  data Loc = Def_loc String Line_and_char String | Input_loc | Method_loc String Line_and_char String String deriving Show
  data Name = Name Line_and_char String deriving Show
  write_conflicting :: String -> String -> Language_or_location -> String -> Line_and_char -> String
  write_conflicting kind x language_or_location file_name_1 line_and_char_1 =
    (
      "Conflicting " ++
      kind ++
      " of " ++
      x ++
      (case language_or_location of
        Language -> " in the language and at "
        Location file_name_0 line_and_char_0 ->
          " at " ++ write_file_name_and_line_and_char file_name_0 line_and_char_0 ++ " and ") ++
      write_file_name_and_line_and_char file_name_1 line_and_char_1 ++
      ".")
  write_error :: Error -> String
  write_error err =
    case err of
      Circular_dependency_between_files a -> "Circular dependency between files [" ++ intercalate "," a ++ "]."
      Circular_inheritance_between_classes file_name classes ->
        "Circular inheritance between classes [" ++ intercalate ", " classes ++ "] in " ++ file_name ++ "."
      Conflicting_definitions kind x language_or_location file_name line_and_char ->
        write_conflicting "definitions" (kind ++ " " ++ x) language_or_location file_name line_and_char
      Conflicting_instances cls typ language_or_location file_name line_and_char ->
        write_conflicting "instances" (cls ++ " " ++ typ) language_or_location file_name line_and_char
      Constructor_has_been_given_a_wrong_number_of_arguments file_name a b ->
        (
          "Data constructor " ++
          a ++
          " at " ++
          write_file_name_and_line_and_char file_name b ++
          " has been given a wrong number of arguments.")
      Def_err file_name a b c ->
        a ++ write_file_name_and_line_and_char file_name b ++ " is not a component of class " ++ c ++ "."
      Duplicate_import_of_file a b c d ->
        (
          "Duplicate import of file " ++
          a ++
          " in " ++
          b ++
          " at " ++
          write_line_and_char c ++
          " and " ++
          write_line_and_char d ++
          ".")
      Expected file_name a b c ->
        (
          "Expected definition of " ++
          a ++
          " at " ++
          write_file_name_and_line_and_char file_name b ++
          ", found " ++
          c ++
          " instead.")
      Failed_to_find_the_file a b c ->
        "Failed to find the file " ++ a ++ " requested at " ++ write_file_name_and_line_and_char b c ++ "."
      Illegal_instance file_name a b c d ->
        a ++ " " ++ b ++ " at " ++ write_file_name_and_line_and_char file_name c ++ " is an illegal instance as " ++ d ++ "."
      Int_starting_with_zero file_name line_and_char ->
        "Int starting with zero at " ++ write_file_name_and_line_and_char file_name line_and_char ++ "."
      Invalid_modular x file_name line_and_char ->
        "Invalid modular " ++ write_modular x ++ " at " ++ write_file_name_and_line_and_char file_name line_and_char ++ "."
      Kind_mismatch a b -> "Kind mismatch at " ++ write_file_name_and_line_and_char a b ++ "."
      Kind_mismatch_in_class file_name a -> "Kind mismatch in class at " ++ write_file_name_and_line_and_char file_name a ++ "."
      Kind_mismatch_in_constraint file_name a b c ->
        (
          "Kind mismatch in constraint at " ++
          write_file_name_and_line_and_char file_name a ++
          " between class " ++
          b ++
          " and type variable " ++
          c ++
          ".")
      Method_requires_instance_or_constraint file_name a b c d e ->
        (
          "Method" ++
          a ++
          (case b of
            Nothing -> ""
            Just (Name _ c1) -> " in " ++ c1) ++
          " at " ++
          write_file_name_and_line_and_char file_name c ++
          " requires instance or constraint " ++
          d ++
          " " ++
          e ++
          ".")
      Missing_definition file_name a b c d ->
        (
          "Missing definition of " ++
          a ++
          " in instance " ++
          b ++
          " " ++
          c ++
          " at " ++
          write_file_name_and_line_and_char file_name d ++
          ".")
      Missing_end_comment file_name -> "Missing end comment in " ++ file_name ++ "."
      Mixed_operator_associativities kind file_name line_and_char_0 line_and_char_1 ->
        (
          "Mixed " ++
          kind ++
          " operator associativities in " ++
          file_name ++
          " from " ++
          write_line_and_char line_and_char_0 ++
          " to " ++
          write_line_and_char line_and_char_1 ++
          ".")
      Parse_error file_name line_and_char ->
        "Parse error at " ++ write_file_name_and_line_and_char file_name line_and_char ++ "."
      Pattern_error file_name line_and_char ->
        "Pattern error at " ++ write_file_name_and_line_and_char file_name line_and_char ++ "."
      Too_many_arguments file_name a b ->
        "Type " ++ b ++ " at " ++ write_file_name_and_line_and_char file_name a ++ " has been given too many arguments."
      Type_mismatch a ->
        (
          "Type mismatch in " ++
          (case a of
            Def_loc b c d -> "definition " ++ d ++ " at " ++ write_file_name_and_line_and_char b c
            Input_loc -> "input"
            Method_loc b c d e ->
              "method implementation " ++ d ++ " " ++ e ++ " at " ++ write_file_name_and_line_and_char b c) ++
          ".")
      Type_variable_is_not_of_kind_Nat name file_name location ->
        "Type variable " ++ name ++ " at " ++ write_file_name_and_line_and_char file_name location ++ " is not of kind Nat."
      Undefined kind x file_name line_and_char ->
        "Undefined " ++ kind ++ " " ++ x ++ " at " ++ write_file_name_and_line_and_char file_name line_and_char ++ "."
      Unresolved_type_variables a ->
        (
          "Unresolved type variables in " ++
          (case a of
            Def_loc b c d -> "definition " ++ d ++ " at " ++ write_file_name_and_line_and_char b c
            Input_loc -> "input"
            Method_loc b c d e ->
              "method implementation " ++ d ++ " " ++ e ++ " at " ++ write_file_name_and_line_and_char b c) ++
          ".")
  write_file_name_and_line_and_char :: String -> Line_and_char -> String
  write_file_name_and_line_and_char file_name line_and_char = file_name ++ ":" ++ write_line_and_char line_and_char
  write_line_and_char :: Line_and_char -> String
  write_line_and_char (Line_and_char line char) = show line ++ ":" ++ show char
--------------------------------------------------------------------------------------------------------------------------------
{-
  typestr' :: Type_6 -> (String, Bool)
  typestr' a =
    case a of
      Application_type_6 b c ->
        let
          (d, e) = typestr' c
        in
          (
            (
              write_type b ++
              " " ++
              case e of
                False -> d
                True -> "(" ++ d ++ ")"),
            True)
      Name_type_6 b -> (b, False)
      Var_type_6 b -> (show b, False)
-}