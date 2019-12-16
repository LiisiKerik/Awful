--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Tokens (Token (..), Tokens, current_line_and_char, init_line_and_char, nom_token, null_tokens, tokenise) where
  import Data.Bifunctor
  import Data.Char
  import Errors
  data Char' =
    Delimiter_char Token |
    Minus_char |
    Nat_char Char |
    Newline_char |
    Operator_char Char |
    Space_char |
    Word_char Char
      deriving Show
  data Token =
    Comma_token |
    Eval_token |
    Left_curly_token |
    Left_round_token |
    Match_token |
    Name_token String |
    Nat_token Integer |
    Negate_token |
    Operator_token String |
    Right_curly_token |
    Right_round_token
      deriving (Eq, Ord, Show)
  data Token' = Token' Line_and_char Token deriving Show
  data Tokens = Tokens [Token'] Line_and_char deriving Show
  add_token :: Line_and_char -> Token -> Tokens -> Tokens
  add_token line_and_char token (Tokens tokens end_line_and_char) =
    Tokens (Token' line_and_char token : tokens) end_line_and_char
  current_line_and_char :: Tokens -> Line_and_char
  current_line_and_char (Tokens tokens end_line_and_char) =
    case tokens of
      [] -> end_line_and_char
      Token' line_and_char _ : _ -> line_and_char
  end_tokens :: Line_and_char -> Err Tokens
  end_tokens line_and_char = Right (Tokens [] line_and_char)
  gather_token :: (Char' -> Maybe Char) -> (String -> Token) -> Line_and_char -> [Char'] -> Err Tokens
  gather_token token_char string_to_token line_and_char text =
    (\(token, tokens) -> add_token line_and_char (string_to_token token) tokens) <$> gather_token' token_char line_and_char text
  gather_token' :: (Char' -> Maybe Char) -> Line_and_char -> [Char'] -> Err (String, Tokens)
  gather_token' token_char line_and_char text =
    case text of
      [] -> (,) "" <$> end_tokens line_and_char
      char' : text' ->
        case token_char char' of
          Just char -> first ((:) char) <$> gather_token' token_char (next_char line_and_char) text'
          Nothing -> (,) "" <$> tokenise' line_and_char text
  init_line_and_char :: Line_and_char
  init_line_and_char = Line_and_char 1 1
  letter :: Char -> Bool
  letter char = elem char ['\'', '_'] || isLetter char
  nat_char :: Char' -> Maybe Char
  nat_char char' =
    case char' of
      Nat_char char -> Just char
      _ -> Nothing
  nat_token :: String -> Token
  nat_token nat = Nat_token (read nat)
  next_char :: Line_and_char -> Line_and_char
  next_char (Line_and_char line char) = Line_and_char line (char + 1)
  next_line :: Line_and_char -> Line_and_char
  next_line (Line_and_char line _) = Line_and_char (line + 1) 1
  nom_token :: (Token -> Maybe t) -> Tokens -> Either Line_and_char (Tokens, t)
  nom_token f (Tokens tokens end_line_and_char) =
    case tokens of
      [] -> Left end_line_and_char
      Token' line_and_char token : tokens' ->
        case f token of
          Nothing -> Left line_and_char
          Just x -> Right (Tokens tokens' end_line_and_char, x)
  null_tokens :: Tokens -> Bool
  null_tokens (Tokens tokens _) = null tokens
  operator_char :: Char' -> Maybe Char
  operator_char char' =
    case char' of
      Operator_char char -> Just char
      -- Slash_char -> Just '/'
      -- Tilde_char -> Just '~'
      _ -> Nothing
  operator_token :: String -> Token
  operator_token operator = Operator_token operator
{-
    case operator of
      "=" -> Eq_token
      _ -> Operator_token operator
-}
  tokenise :: String -> Err Tokens
  tokenise text = tokenise' init_line_and_char (transf_char <$> text)
{-
            Slash_char -> tokenise_operator file_name line_and_char_0 text_0
            Tick_char -> tokenise_single_line file_name line_and_char_1 text_1
            Tilde_char ->
              case text_1 of
                Slash_char : text_2 -> tokenise_multiline file_name 1 (next_char line_and_char_1) text_2
                _ -> tokenise_operator file_name line_and_char_0 text_0
-}
  tokenise' :: Line_and_char -> [Char'] -> Err Tokens
  tokenise' line_and_char text =
    case text of
      [] -> end_tokens line_and_char
      char' : text' ->
        case char' of
          Delimiter_char delimiter_token ->
            add_token line_and_char delimiter_token <$> tokenise' (next_char line_and_char) text'
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
          Space_char -> tokenise' (next_char line_and_char) text'
          Word_char _ -> gather_token word_char word_token line_and_char text
  tokenise_nat :: Line_and_char -> [Char'] -> Err Tokens
  tokenise_nat = gather_token nat_char nat_token
  tokenise_operator :: Line_and_char -> [Char'] -> Err Tokens
  tokenise_operator = gather_token operator_char operator_token
  transf_char :: Char -> Char'
  transf_char char =
    case char of
      '\n' -> Newline_char
      ' ' -> Space_char
      '(' -> Delimiter_char Left_round_token
      ')' -> Delimiter_char Right_round_token
      ',' -> Delimiter_char Comma_token
      '-' -> Minus_char
      -- '/' -> Slash_char
      -- '[' -> Delimiter_char Left_square_delimiter
      -- ']' -> Delimiter_char Right_square_delimiter
      -- '`' -> Tick_char
      '{' -> Delimiter_char Left_curly_token
      '}' -> Delimiter_char Right_curly_token
      -- '~' -> Tilde_char
      _ ->
        case (letter char, isDigit char) of
          (False, False) -> Operator_char char
          (_, True) -> Nat_char char
          (True, _) -> Word_char char
  word_char :: Char' -> Maybe Char
  word_char char' =
    case char' of
      Nat_char char -> Just char
      Word_char char -> Just char
      _ -> Nothing
  word_token :: String -> Token
  word_token word =
    case word of
      -- "_" -> Blank_token
      -- "Algebraic" -> Algebraic_token
      -- "Branch" -> Branch_token
      -- "Check" -> Check_token
      -- "Class" -> Class_token
      -- "Data" -> Data_token
      -- "Def" -> Def_token
      "Eval" -> Eval_token
      -- "In" -> In_token
      -- "Instance" -> Instance_token
      -- "Let" -> Let_token
      -- "Load" -> Load_token
      "Match" -> Match_token
      -- "Struct" -> Struct_token
      -- "Term" -> Term_token
      -- "Type" -> Type_token
      _ -> Name_token word
--------------------------------------------------------------------------------------------------------------------------------