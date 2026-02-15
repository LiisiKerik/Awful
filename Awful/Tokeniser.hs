module Awful.Tokeniser (Err, Location_1 (..), Token (..), classify_char, location, location', next_location, tokenise) where
  import Data.Char
  import Data.Functor
  import Parser.Locations
  import Parser.Parser
  data Char_class =
    Delimiter_char Token |
    Invalid_char |
    Letter_char Char |
    Name_char Char |
    Newline_char |
    Nonzero_nat_char Char |
    Operator_char Char |
    Whitespace_char |
    Zero_char
  type Err t = Either String t
  data Location_1 = Location_1 String Location deriving Show
  data Token =
    Arrow_token |
    Blank_token |
    Branching_token |
    Case_token |
    Class_token |
    Comma_token |
    Def_token |
    Default_token |
    In_token |
    Instance_token |
    Int_token Integer |
    Left_curly_bracket_token |
    Left_round_bracket_token |
    Left_square_bracket_token |
    Let_token |
    Load_token |
    Match_token |
    Name_token String |
    Named_struct_token |
    Opdecl_token |
    Operator_token String |
    Right_curly_bracket_token |
    Right_round_bracket_token |
    Right_square_bracket_token |
    Unnamed_algebraic_token
  type Tokeniser = Tokeniser' Char_class Token ((Location -> Location_1) -> String)
  deriving instance Eq Char_class
  deriving instance Eq Token
  deriving instance Show Char_class
  deriving instance Show Token
  classify_char :: Char -> Char_class
  classify_char c =
    case c of
      '\n' -> Newline_char
      ' ' -> Whitespace_char
      _ | elem c "!\"#$%&*+-./:;<=>?@\\^`|~" -> Operator_char c
      _ | elem c "'_" || isLetter c -> Letter_char c
      '(' -> Delimiter_char Left_round_bracket_token
      ')' -> Delimiter_char Right_round_bracket_token
      ',' -> Delimiter_char Comma_token
      '0' -> Zero_char
      _ | isDigit c && c /= '0' -> Nonzero_nat_char c
      '[' -> Delimiter_char Left_square_bracket_token
      ']' -> Delimiter_char Right_square_bracket_token
      '{' -> Delimiter_char Left_curly_bracket_token
      '}' -> Delimiter_char Right_curly_bracket_token
      _ -> Invalid_char
  delimiter_char :: Char_class -> Maybe Token
  delimiter_char char_class =
    case char_class of
      Delimiter_char token -> Just token
      _ -> Nothing
  location :: Location_1 -> String
  location (Location_1 a b) = " at " ++ a ++ ":" ++ write_location b
  location' :: Location_1 -> String
  location' a = location a ++ "."
  letter_char :: Char_class -> Maybe Char
  letter_char char_class =
    case char_class of
      Letter_char c -> Just c
      _ -> Nothing
  letter_or_nat_char :: Char_class -> Maybe Char
  letter_or_nat_char char_class =
    case char_class of
      Letter_char c -> Just c
      Zero_char -> Just '0'
      Nonzero_nat_char c -> Just c
      _ -> Nothing
  nat_char :: Char_class -> Maybe Char
  nat_char char_class =
    case char_class of
      Zero_char -> Just '0'
      Nonzero_nat_char c -> Just c
      _ -> Nothing
  next_location :: Char_class -> Location -> Location
  next_location char_class =
    case char_class of
      Newline_char -> next_line
      _ -> next_char
  nonzero_nat_char :: Char_class -> Maybe Char
  nonzero_nat_char char_class =
    case char_class of
      Nonzero_nat_char c -> Just c
      _ -> Nothing
  operator_char :: Char_class -> Maybe Char
  operator_char a =
    case a of
      Operator_char b -> Just b
      _ -> Nothing
  operator_token :: String -> Token
  operator_token operator =
    case operator of
      "->" -> Arrow_token
      _ -> Operator_token operator
  tokenise :: Tokeniser ()
  tokenise = void (parse_many tokenise_1)
  tokenise_1 :: Tokeniser ()
  tokenise_1 =
    tokenise_delimiter <+> tokenise_int <+> tokenise_newline <+> tokenise_operator <+> tokenise_whitespace <+> tokenise_word
  tokenise_delimiter :: Tokeniser ()
  tokenise_delimiter = add_token (parse_token' delimiter_char)
  tokenise_int :: Tokeniser ()
  tokenise_int = add_token (Int_token <$> (tokenise_negative_int <+> tokenise_zero <+> tokenise_positive_int))
  tokenise_negative_int :: Tokeniser Integer
  tokenise_negative_int =
    do
      parse_token (Operator_char '-')
      i <- tokenise_positive_int
      return (negate i)
  tokenise_newline :: Tokeniser ()
  tokenise_newline = parse_token Newline_char
  tokenise_operator :: Tokeniser ()
  tokenise_operator = add_token (operator_token <$> parse_some (parse_token' operator_char))
  tokenise_positive_int :: Tokeniser Integer
  tokenise_positive_int = read <$> ((:) <$> parse_token' nonzero_nat_char <*> parse_many (parse_token' nat_char))
  tokenise_whitespace :: Tokeniser ()
  tokenise_whitespace = parse_token Whitespace_char
  tokenise_word :: Tokeniser ()
  tokenise_word = add_token (word_token <$> ((:) <$> parse_token' letter_char <*> parse_many (parse_token' letter_or_nat_char)))
  tokenise_zero :: Tokeniser Integer
  tokenise_zero =
    do
      parse_token Zero_char
      return 0
  word_token :: String -> Token
  word_token a =
    case a of
      "_" -> Blank_token
      "Branching" -> Branching_token
      "Case" -> Case_token
      "Class" -> Class_token
      "Def" -> Def_token
      "Default" -> Default_token
      "In" -> In_token
      "Instance" -> Instance_token
      "Let" -> Let_token
      "Load" -> Load_token
      "Match" -> Match_token
      "Named_struct" -> Named_struct_token
      "Operator" -> Opdecl_token
      "Unnamed_algebraic" -> Unnamed_algebraic_token
      _ -> Name_token a