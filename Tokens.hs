--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE StandaloneDeriving #-}
module Tokens (Token (..), Tokens, current_line_and_char, nom_token, Tokens.null, tokenise) where
  import Data.Bifunctor (first)
  import Data.Char (isDigit, isLetter)
  import Errors (Err', Error' (..), Line_and_char, init_line_and_char, next_char, next_line)
  data Char' =
    Delimiter_char Token |
    Invalid_char |
    Minus_char |
    Nat_char Char |
    Newline_char |
    Operator_char Char |
    Slash_char |
    Space_char |
    Tick_char |
    Tilde_char |
    Word_char Char
      deriving Show
{-
    In_token |
    Let_token
-}
  data Token =
    Algebraic_token |
    Blank_token |
    Branch_token |
    Check_token |
    Class_token |
    Comma_token |
    Data_token |
    Def_token |
    Eq_token |
    Eval_token |
    Instance_token |
    Import_token |
    Left_curly_token |
    Left_round_token |
    Left_square_token |
    Match_token |
    Name_token String |
    Nat_token Integer |
    Negate_token |
    Operator_token String |
    Right_curly_token |
    Right_round_token |
    Right_square_token |
    Run_token |
    Struct_token |
    Term_token |
    Test_token |
    Type_token
      deriving Show
  data Token' = Token' Line_and_char Token
      deriving Show
  data Tokens = Tokens [Token'] Line_and_char
      deriving Show
  deriving instance Eq Token
  add_token :: Line_and_char -> Token -> Tokens -> Tokens
  add_token line_and_char token (Tokens tokens end_line_and_char) =
    Tokens (Token' line_and_char token : tokens) end_line_and_char
  current_line_and_char :: Tokens -> Line_and_char
  current_line_and_char (Tokens tokens end_line_and_char) =
    case tokens of
      [] -> end_line_and_char
      Token' line_and_char _ : _ -> line_and_char
  end_tokens :: Line_and_char -> Err' Tokens
  end_tokens line_and_char = Right (Tokens [] line_and_char)
  gather_token :: (Char' -> Maybe Char) -> (String -> Token) -> Line_and_char -> [Char'] -> Err' Tokens
  gather_token token_char string_to_token line_and_char text =
    (\(token, tokens) -> add_token line_and_char (string_to_token token) tokens) <$> gather_token' token_char line_and_char text
  gather_token' :: (Char' -> Maybe Char) -> Line_and_char -> [Char'] -> Err' (String, Tokens)
  gather_token' token_char line_and_char text =
    case text of
      [] -> (,) "" <$> end_tokens line_and_char
      char' : text' ->
        case token_char char' of
          Just char -> first ((:) char) <$> gather_token' token_char (next_char line_and_char) text'
          Nothing -> (,) "" <$> tokenise' line_and_char text
  is_letter :: Char -> Bool
  is_letter char = elem char ['\'', '_'] || isLetter char
  is_operator :: Char -> Bool
  is_operator char = elem char ['!', '#', '$', '%', '&', '*', '+', '.', ':', ';', '<', '=', '>', '?', '@', '\\', '^', '|']
  nat_char :: Char' -> Maybe Char
  nat_char char' =
    case char' of
      Nat_char char -> Just char
      _ -> Nothing
  nat_token :: String -> Token
  nat_token n = Nat_token (read n)
  nom_token :: (Token -> Maybe t) -> Tokens -> Maybe (t, Tokens)
  nom_token f (Tokens tokens end_line_and_char) =
    case tokens of
      [] -> Nothing
      Token' _ token : tokens' ->
        do
          x <- f token
          Just (x, Tokens tokens' end_line_and_char)
  null :: Tokens -> Bool
  null (Tokens tokens _) = Prelude.null tokens
  operator_char :: Char' -> Maybe Char
  operator_char char' =
    case char' of
      Operator_char char -> Just char
      Minus_char -> Just '-'
      Slash_char -> Just '/'
      Tilde_char -> Just '~'
      _ -> Nothing
  operator_token :: String -> Token
  operator_token operator =
    case operator of
      "=" -> Eq_token
      _ -> Operator_token operator
  tokenise :: String -> Err' Tokens
  tokenise text = tokenise' init_line_and_char (transf_char <$> text)
  tokenise' :: Line_and_char -> [Char'] -> Err' Tokens
  tokenise' line_and_char text =
    case text of
      [] -> end_tokens line_and_char
      char' : text' ->
        case char' of
          Delimiter_char delimiter_token ->
            add_token line_and_char delimiter_token <$> tokenise' (next_char line_and_char) text'
          Invalid_char -> Left (Invalid_character line_and_char)
          Minus_char ->
            case text' of
              Nat_char char : _ ->
                case char of
                  '0' -> Left (Negation_of_int_starting_with_zero line_and_char)
                  _ -> add_token line_and_char Negate_token <$> tokenise_nat (next_char line_and_char) text'
              _ -> tokenise_operator line_and_char text
          Nat_char char ->
            case (char, text') of
              ('0', Nat_char _ : _) -> Left (Int_starting_with_zero line_and_char)
              _ -> tokenise_nat line_and_char text
          Newline_char -> tokenise' (next_line line_and_char) text'
          Operator_char _ -> tokenise_operator line_and_char text
          Slash_char -> tokenise_operator line_and_char text
          Space_char -> tokenise' (next_char line_and_char) text'
          Tick_char -> tokenise_single_line_comment (next_char line_and_char) text'
          Tilde_char ->
            case text' of
              Slash_char : text'' ->
                case text'' of
                  [] -> Left Missing_end_comment
                  char'' : _ ->
                    case operator_char char'' of
                      Nothing -> tokenise_multiline_comment (next_char (next_char line_and_char)) text''
                      Just _ -> tokenise_operator line_and_char text
              _ -> tokenise_operator line_and_char text
          Word_char _ -> gather_token word_char word_token line_and_char text
  tokenise_multiline_comment :: Line_and_char -> [Char'] -> Err' Tokens
  tokenise_multiline_comment line_and_char text =
    case text of
      [] -> Left Missing_end_comment
      char' : text' ->
        case char' of
          Newline_char -> tokenise_multiline_comment (next_line line_and_char) text'
          Slash_char -> tokenise_multiline_comment_slash (next_char line_and_char) text'
          _ ->
            case operator_char char' of
              Nothing -> tokenise_multiline_comment (next_char line_and_char) text'
              Just _ -> tokenise_multiline_comment_operator (next_char line_and_char) text'
  tokenise_multiline_comment_slash :: Line_and_char -> [Char'] -> Err' Tokens
  tokenise_multiline_comment_slash line_and_char text =
    case text of
      [] -> Left Missing_end_comment
      char' : text' ->
        case operator_char char' of
          Nothing -> tokenise_multiline_comment  line_and_char text
          Just _ ->
            case char' of
              Tilde_char -> tokenise_multiline_comment_slash_tilde (next_char line_and_char) text'
              _ -> tokenise_multiline_comment_operator (next_char line_and_char) text'
  tokenise_multiline_comment_slash_tilde :: Line_and_char -> [Char'] -> Err' Tokens
  tokenise_multiline_comment_slash_tilde line_and_char text =
    case text of
      [] -> end_tokens line_and_char
      char' : text' ->
        case operator_char char' of
          Nothing -> tokenise' line_and_char text
          Just _ -> tokenise_multiline_comment_operator (next_char line_and_char) text'
  tokenise_multiline_comment_operator :: Line_and_char -> [Char'] -> Err' Tokens
  tokenise_multiline_comment_operator line_and_char text =
    case text of
      [] -> Left Missing_end_comment
      char' : text' ->
        case operator_char char' of
          Nothing -> tokenise_multiline_comment line_and_char text
          Just _ -> tokenise_multiline_comment_operator (next_char line_and_char) text'
  tokenise_nat :: Line_and_char -> [Char'] -> Err' Tokens
  tokenise_nat = gather_token nat_char nat_token
  tokenise_operator :: Line_and_char -> [Char'] -> Err' Tokens
  tokenise_operator = gather_token operator_char operator_token
  tokenise_single_line_comment :: Line_and_char -> [Char'] -> Err' Tokens
  tokenise_single_line_comment line_and_char text =
    case text of
      [] -> end_tokens line_and_char
      char' : text' ->
        case char' of
          Newline_char -> tokenise' (next_line line_and_char) text'
          _ -> tokenise_single_line_comment (next_char line_and_char) text'
  transf_char :: Char -> Char'
  transf_char char =
    case char of
      '\n' -> Newline_char
      ' ' -> Space_char
      '(' -> Delimiter_char Left_round_token
      ')' -> Delimiter_char Right_round_token
      ',' -> Delimiter_char Comma_token
      '-' -> Minus_char
      '/' -> Slash_char
      '[' -> Delimiter_char Left_square_token
      ']' -> Delimiter_char Right_square_token
      '`' -> Tick_char
      '{' -> Delimiter_char Left_curly_token
      '}' -> Delimiter_char Right_curly_token
      '~' -> Tilde_char
      _ ->
        case (is_operator char, is_letter char, isDigit char) of
          (True, False, False) -> Operator_char char
          (False, True, False) -> Word_char char
          (False, False, True) -> Nat_char char
          _ -> Invalid_char
  word_char :: Char' -> Maybe Char
  word_char char' =
    case char' of
      Nat_char char -> Just char
      Word_char char -> Just char
      _ -> Nothing
  word_token :: String -> Token
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
      "Import" -> Import_token
      -- "In" -> In_token
      "Instance" -> Instance_token
      -- "Let" -> Let_token
      "Match" -> Match_token
      "Run" -> Run_token
      "Struct" -> Struct_token
      "Term" -> Term_token
      "Test" -> Test_token
      "Type" -> Type_token
      _ -> Name_token word
--------------------------------------------------------------------------------------------------------------------------------