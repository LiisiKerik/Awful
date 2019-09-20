--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Tokenise where
  import Data.Bifunctor
  import Data.Char
  import Errors
  data Char' =
    Delimiter_char Delimiter |
    Int_char Char |
    Minus_char |
    Name_char Char |
    Newline_char |
    Operator_char Char |
    Quote_char |
    Slash_char |
    Space_char |
    Tick_char |
    Tilde_char
      deriving Show
  data Delimiter =
    Comma_delimiter |
    Dollar_delimiter |
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
    Case_token |
    Char_token Char |
    Check_token |
    Class_token |
    Comma_token |
    Data_token |
    Def_token |
    Eval_token |
    In_token |
    Instance_token |
    Int_token Integer |
    Left_curly_token |
    Left_round_token |
    Left_square_token |
    Let_token |
    Load_token |
    Match_token |
    Name_token String |
    Negate_token |
    Of_token |
    Operator_token String |
    Right_curly_token |
    Right_round_token |
    Right_square_token |
    Struct_token |
    Syntax_name_token |
    Syntax_token |
    Term_token |
    Type_token
      deriving (Eq, Show)
  data Token_1 = Token_1 Location_0 Token_0 deriving Show
  data Tokens = Tokens [Token_1] Location_0 deriving Show
  add_token :: Location_0 -> Token_0 -> Tokens -> Tokens
  add_token location token (Tokens tokens end_location) = Tokens (Token_1 location token : tokens) end_location
  char'_to_char :: Char' -> Char
  char'_to_char char' =
    case char' of
      Delimiter_char delimiter -> delimiter_to_char delimiter
      Int_char char -> char
      Minus_char -> '-'
      Name_char char -> char
      Newline_char -> '\n'
      Operator_char char -> char
      Quote_char -> '"'
      Slash_char -> '/'
      Space_char -> ' '
      Tick_char -> '`'
      Tilde_char -> '~'
  char_to_char' :: Char -> Char'
  char_to_char' a =
    case a of
      '\n' -> Newline_char
      ' ' -> Space_char
      '"' -> Quote_char
      '$' -> Delimiter_char Dollar_delimiter
      '(' -> Delimiter_char Left_round_delimiter
      ')' -> Delimiter_char Right_round_delimiter
      ',' -> Delimiter_char Comma_delimiter
      '-' -> Minus_char
      '/' -> Slash_char
      '[' -> Delimiter_char Left_square_delimiter
      ']' -> Delimiter_char Right_square_delimiter
      '`' -> Tick_char
      '{' -> Delimiter_char Left_curly_delimiter
      '}' -> Delimiter_char Right_curly_delimiter
      '~' -> Tilde_char
      _ ->
        (if_sequence
          a
          [
            (flip elem ['!', '#', '%', '&', '*', '+', '.', ':', ';', '|', '<', '=', '>', '?', '@', '\\', '^'], Operator_char),
            (isDigit, Int_char)]
          Name_char)
            a
  delimiter_to_char :: Delimiter -> Char
  delimiter_to_char delimiter =
    case delimiter of
      Comma_delimiter -> ','
      Dollar_delimiter -> '$'
      Left_curly_delimiter -> '{'
      Left_round_delimiter -> '('
      Left_square_delimiter -> '['
      Right_curly_delimiter -> '}'
      Right_round_delimiter -> ')'
      Right_square_delimiter -> ']'
  delimiter_to_token :: Delimiter -> Token_0
  delimiter_to_token delimiter =
    case delimiter of
      Comma_delimiter -> Comma_token
      Dollar_delimiter -> Syntax_name_token
      Left_curly_delimiter -> Left_curly_token
      Left_round_delimiter -> Left_round_token
      Left_square_delimiter -> Left_square_token
      Right_curly_delimiter -> Right_curly_token
      Right_round_delimiter -> Right_round_token
      Right_square_delimiter -> Right_square_token
  end_tokens :: Location_0 -> Err Tokens
  end_tokens location = Right (Tokens [] location)
  gather_token :: (Char' -> Maybe Char) -> (String -> Token_0) -> Location_0 -> [Char'] -> Err Tokens
  gather_token token_char string_to_token location file =
    (\(token, tokens) -> add_token location (string_to_token token) tokens) <$> gather_token' token_char location file
  gather_token' :: (Char' -> Maybe Char) -> Location_0 -> [Char'] -> Err (String, Tokens)
  gather_token' token_char location file_0 =
    let
      d = (,) "" <$> tokenise' location file_0
    in
      case file_0 of
        [] -> d
        e : f ->
          case token_char e of
            Just g -> first ((:) g) <$> gather_token' token_char (next_char location) f
            Nothing -> d
  if_sequence :: t -> [(t -> Bool, u)] -> u -> u
  if_sequence a b c =
    case b of
      [] -> c
      (d, e) : f -> if d a then e else if_sequence a f c
  int_char :: Char' -> Maybe Char
  int_char a =
    case a of
      Int_char b -> Just b
      _ -> Nothing
  name_char :: Char' -> Maybe Char
  name_char char =
    case char of
      Int_char char' -> Just char'
      Name_char char' -> Just char'
      _ -> Nothing
  next_char :: Location_0 -> Location_0
  next_char (Location_0 line char) = Location_0 line (char + 1)
  next_line :: Location_0 -> Location_0
  next_line (Location_0 line _) = Location_0 (line + 1) 1
  operator_char :: Char' -> Maybe Char
  operator_char char =
    case char of
      Minus_char -> Just '-'
      Operator_char char' -> Just char'
      Slash_char -> Just '/'
      Tilde_char -> Just '~'
      _ -> Nothing
  tokenise :: String -> Err Tokens
  tokenise file = tokenise' (Location_0 1 1) (char_to_char' <$> file)
  tokenise' :: Location_0 -> [Char'] -> Err Tokens
  tokenise' location_0 file_0 =
    let
      location_1 = next_char location_0
    in
      case file_0 of
        [] -> end_tokens location_0
        char : file_1 ->
          case char of
            Delimiter_char delimiter ->
              add_token location_0 (delimiter_to_token delimiter) <$> tokenise' location_1 file_1
            Int_char digit ->
              case (digit, file_1) of
                ('0', Int_char _ : _) -> Left (Int_starting_with_zero location_0)
                _ -> gather_token int_char (Int_token <$> read) location_0 file_0
            Minus_char ->
              case file_1 of
                Int_char digit : _ ->
                  case digit of
                    '0' -> Left (Negation_of_int_starting_with_zero location_0)
                    _ ->
                      (
                        add_token location_0 Negate_token <$>
                        gather_token int_char (Int_token <$> read) location_1 file_1)
                _ -> tokenise_operator location_0 file_0
            Name_char _ -> gather_token name_char word_token location_0 file_0
            Newline_char -> tokenise' (next_line location_0) file_1
            Operator_char _ -> tokenise_operator location_0 file_0
            Quote_char ->
              (\(char', tokens) -> add_token location_0 (Char_token char') tokens) <$> tokenise_char location_1 file_1
            Slash_char -> tokenise_operator location_0 file_0
            Space_char -> tokenise' location_1 file_1
            Tick_char -> tokenise_single location_1 file_1
            Tilde_char ->
              case file_1 of
                Slash_char : file_2 -> tokenise_multiline 1 (next_char location_1) file_2
                _ -> tokenise_operator location_0 file_0
  tokenise_char :: Location_0 -> [Char'] -> Err (Char, Tokens)
  tokenise_char location file_0 =
    case file_0 of
      [] -> Left Missing_char_and_end_quote
      char : file_1 ->
        case char'_to_char char of
          '\n' -> Left (Newline_inside_quotes location)
          char' -> (,) char' <$> tokenise_quote (next_char location) file_1
  tokenise_multiline :: Integer -> Location_0 -> [Char'] -> Err Tokens
  tokenise_multiline depth location_0 file_0 =
    let
      location_1 = next_char location_0
    in
      case file_0 of
        [] -> Left Missing_end_comment
        char : file_1 ->
          case char of
            Newline_char -> tokenise_multiline depth (next_line location_0) file_1
            Slash_char -> tokenise_slash depth location_1 file_1
            Tilde_char -> tokenise_tilde depth location_1 file_1
            _ -> tokenise_multiline depth location_1 file_1
  tokenise_operator :: Location_0 -> [Char'] -> Err Tokens
  tokenise_operator location = gather_token operator_char Operator_token location
  tokenise_quote :: Location_0 -> [Char'] -> Err Tokens
  tokenise_quote a b =
    case b of
      Quote_char : d -> tokenise' (next_char a) d
      _ -> Left Missing_end_quote
  tokenise_single :: Location_0 -> [Char'] -> Err Tokens
  tokenise_single a b =
    case b of
      [] -> end_tokens a
      c : d ->
        case c of
          Newline_char -> tokenise' (next_line a) d
          _ -> tokenise_single (next_char a) d
  tokenise_slash :: Integer -> Location_0 -> [Char'] -> Err Tokens
  tokenise_slash depth e c =
    case c of
      Tilde_char : d ->
        (case depth of
          1 -> tokenise' (next_char e)
          _ -> tokenise_multiline (depth - 1) (next_char e))
            d
      _ -> tokenise_multiline depth e c
  tokenise_tilde :: Integer -> Location_0 -> [Char'] -> Err Tokens
  tokenise_tilde depth b file_0 =
    case file_0 of
      Slash_char : file_1 -> tokenise_multiline (depth + 1) (next_char b) file_1
      _ -> tokenise_multiline depth b file_0
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
      "Newline" -> Char_token '\n'
      "Struct" -> Struct_token
      "Syntax" -> Syntax_token
      "Term" -> Term_token
      "Type" -> Type_token
      "case" -> Case_token
      "of" -> Of_token
      _ -> Name_token word
--------------------------------------------------------------------------------------------------------------------------------