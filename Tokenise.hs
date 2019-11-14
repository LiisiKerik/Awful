--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Tokenise (Tokens, Token_0 (..), current_line_and_char, nom_token, null_tokens, tokenise) where
  import Data.Bifunctor
  import Data.Char
  import Errors
  data Char' =
    Delimiter_char Delimiter |
    Name_char Char |
    Nat_char Char |
    Newline_char |
    Operator_char Char |
    Slash_char |
    Space_char |
    Tick_char |
    Tilde_char
      deriving Show
  data Delimiter =
    Comma_delimiter |
    Left_curly_delimiter |
    Left_round_delimiter |
    Left_square_delimiter |
    Right_curly_delimiter |
    Right_round_delimiter |
    Right_square_delimiter
      deriving Show
  data Token_0 =
    Algebraic_token |
    Blank_token |
    Branch_token |
    Check_token |
    Class_token |
    Comma_token |
    Data_token |
    Def_token |
    Eval_token |
    Eq_token |
    In_token |
    Instance_token |
    Left_curly_token |
    Left_round_token |
    Left_square_token |
    Let_token |
    Load_token |
    Match_token |
    Name_token String |
    Nat_token Integer |
    Operator_token String |
    Right_curly_token |
    Right_round_token |
    Right_square_token |
    Struct_token |
    Term_token |
    Type_token
      deriving (Eq, Ord, Show)
  data Token_1 = Token_1 Line_and_char Token_0 deriving (Eq, Ord, Show)
  data Tokens = Tokens [Token_1] Line_and_char deriving Show
  add_token :: Line_and_char -> Token_0 -> Tokens -> Tokens
  add_token line_and_char_0 token (Tokens tokens line_and_char_1) =
    Tokens (Token_1 line_and_char_0 token : tokens) line_and_char_1
  char_to_char' :: Char -> Char'
  char_to_char' char =
    case char of
      '\n' -> Newline_char
      ' ' -> Space_char
      '(' -> Delimiter_char Left_round_delimiter
      ')' -> Delimiter_char Right_round_delimiter
      ',' -> Delimiter_char Comma_delimiter
      '/' -> Slash_char
      '[' -> Delimiter_char Left_square_delimiter
      ']' -> Delimiter_char Right_square_delimiter
      '`' -> Tick_char
      '{' -> Delimiter_char Left_curly_delimiter
      '}' -> Delimiter_char Right_curly_delimiter
      '~' -> Tilde_char
      _ ->
        case (elem char ['\'', '_'] || isLetter char, isDigit char) of
          (False, False) -> Operator_char char
          (_, True) -> Nat_char char
          (True, _) -> Name_char char
  current_line_and_char :: Tokens -> Line_and_char
  current_line_and_char (Tokens tokens line_and_char_0) =
    case tokens of
      [] -> line_and_char_0
      Token_1 line_and_char_1 _ : _ -> line_and_char_1
  delimiter_to_token :: Delimiter -> Token_0
  delimiter_to_token delimiter =
    case delimiter of
      Comma_delimiter -> Comma_token
      Left_curly_delimiter -> Left_curly_token
      Left_round_delimiter -> Left_round_token
      Left_square_delimiter -> Left_square_token
      Right_curly_delimiter -> Right_curly_token
      Right_round_delimiter -> Right_round_token
      Right_square_delimiter -> Right_square_token
  end_tokens :: Line_and_char -> Err Tokens
  end_tokens line_and_char = Right (Tokens [] line_and_char)
  gather_token :: String -> (Char' -> Maybe Char) -> (String -> Token_0) -> Line_and_char -> [Char'] -> Err Tokens
  gather_token file_name token_char string_to_token line_and_char text =
    (
      (\(token, tokens) -> add_token line_and_char (string_to_token token) tokens) <$>
      gather_token' file_name token_char line_and_char text)
  gather_token' :: String -> (Char' -> Maybe Char) -> Line_and_char -> [Char'] -> Err (String, Tokens)
  gather_token' file_name token_char line_and_char text_0 =
    case text_0 of
      [] -> (,) "" <$> end_tokens line_and_char
      char' : text_1 ->
        case token_char char' of
          Just char -> first ((:) char) <$> gather_token' file_name token_char (next_char line_and_char) text_1
          Nothing -> (,) "" <$> tokenise' file_name line_and_char text_0
  name_char :: Char' -> Maybe Char
  name_char char' =
    case char' of
      Name_char char -> Just char
      Nat_char char -> Just char
      _ -> Nothing
  nat_char :: Char' -> Maybe Char
  nat_char char' =
    case char' of
      Nat_char char -> Just char
      _ -> Nothing
  nom_token :: (Token_0 -> Maybe t) -> Tokens -> Either Line_and_char (Tokens, t)
  nom_token f (Tokens tokens_0 line_and_char_0) =
    case tokens_0 of
      [] -> Left line_and_char_0
      Token_1 line_and_char_1 token : tokens_1 ->
        case f token of
          Nothing -> Left line_and_char_1
          Just x -> Right (Tokens tokens_1 line_and_char_0, x)
  null_tokens :: Tokens -> Bool
  null_tokens (Tokens tokens _) = null tokens
  next_char :: Line_and_char -> Line_and_char
  next_char (Line_and_char line char) = Line_and_char line (char + 1)
  next_line :: Line_and_char -> Line_and_char
  next_line (Line_and_char line _) = Line_and_char (line + 1) 1
  operator_char :: Char' -> Maybe Char
  operator_char char' =
    case char' of
      Operator_char char -> Just char
      Slash_char -> Just '/'
      Tilde_char -> Just '~'
      _ -> Nothing
  operator_token :: String -> Token_0
  operator_token operator =
    case operator of
      "=" -> Eq_token
      _ -> Operator_token operator
  tokenise :: String -> String -> Err Tokens
  tokenise file_name text = tokenise' file_name (Line_and_char 1 1) (char_to_char' <$> text)
  tokenise' :: String -> Line_and_char -> [Char'] -> Err Tokens
  tokenise' file_name line_and_char_0 text_0 =
    let
      line_and_char_1 = next_char line_and_char_0
    in
      case text_0 of
        [] -> end_tokens line_and_char_0
        char' : text_1 ->
          case char' of
            Delimiter_char delimiter ->
              add_token line_and_char_0 (delimiter_to_token delimiter) <$> tokenise' file_name line_and_char_1 text_1
            Name_char _ -> gather_token file_name name_char word_token line_and_char_0 text_0
            Nat_char char ->
              case (char, text_1) of
                ('0', Nat_char _ : _) -> Left (Int_starting_with_zero file_name line_and_char_0)
                _ -> tokenise_nat file_name line_and_char_0 text_0
            Newline_char -> tokenise' file_name (next_line line_and_char_0) text_1
            Operator_char _ -> tokenise_operator file_name line_and_char_0 text_0
            Slash_char -> tokenise_operator file_name line_and_char_0 text_0
            Space_char -> tokenise' file_name line_and_char_1 text_1
            Tick_char -> tokenise_single_line file_name line_and_char_1 text_1
            Tilde_char ->
              case text_1 of
                Slash_char : text_2 -> tokenise_multiline file_name 1 (next_char line_and_char_1) text_2
                _ -> tokenise_operator file_name line_and_char_0 text_0
  tokenise_multiline :: String -> Integer -> Line_and_char -> [Char'] -> Err Tokens
  tokenise_multiline file_name depth line_and_char_0 text_0 =
    let
      line_and_char_1 = next_char line_and_char_0
    in
      case text_0 of
        [] -> Left (Missing_end_comment file_name)
        char' : text_1 ->
          case char' of
            Newline_char -> tokenise_multiline file_name depth (next_line line_and_char_0) text_1
            Slash_char -> tokenise_slash file_name depth line_and_char_1 text_1
            Tilde_char -> tokenise_tilde file_name depth line_and_char_1 text_1
            _ -> tokenise_multiline file_name depth line_and_char_1 text_1
  tokenise_nat :: String -> Line_and_char -> [Char'] -> Err Tokens
  tokenise_nat file_name = gather_token file_name nat_char (Nat_token <$> read)
  tokenise_operator :: String -> Line_and_char -> [Char'] -> Err Tokens
  tokenise_operator file_name = gather_token file_name operator_char operator_token
  tokenise_single_line :: String -> Line_and_char -> [Char'] -> Err Tokens
  tokenise_single_line file_name line_and_char text_0 =
    case text_0 of
      [] -> end_tokens line_and_char
      char' : text_1 ->
        case char' of
          Newline_char -> tokenise' file_name (next_line line_and_char) text_1
          _ -> tokenise_single_line file_name (next_char line_and_char) text_1
  tokenise_slash :: String -> Integer -> Line_and_char -> [Char'] -> Err Tokens
  tokenise_slash file_name depth line_and_char_0 text_0 =
    let
      line_and_char_1 = next_char line_and_char_0
    in
      case text_0 of
        Tilde_char : text_1 ->
          case depth of
            1 -> tokenise' file_name line_and_char_1 text_1
            _ -> tokenise_multiline file_name (depth - 1) line_and_char_1 text_1
        _ -> tokenise_multiline file_name depth line_and_char_0 text_0
  tokenise_tilde :: String -> Integer -> Line_and_char -> [Char'] -> Err Tokens
  tokenise_tilde file_name depth line_and_char text_0 =
    case text_0 of
      Slash_char : text_1 -> tokenise_multiline file_name (depth + 1) (next_char line_and_char) text_1
      _ -> tokenise_multiline file_name depth line_and_char text_0
  word_token :: String -> Token_0
  word_token word =
    case word of
      "_" -> Blank_token
      "Algebraic" -> Algebraic_token
      "Branch" -> Branch_token
      "Check" -> Check_token
      "Class" -> Class_token
      "Data" -> Data_token
      "Def" -> Def_token
      "Eval" -> Eval_token
      "In" -> In_token
      "Instance" -> Instance_token
      "Let" -> Let_token
      "Load" -> Load_token
      "Match" -> Match_token
      "Struct" -> Struct_token
      "Term" -> Term_token
      "Type" -> Type_token
      _ -> Name_token word
--------------------------------------------------------------------------------------------------------------------------------