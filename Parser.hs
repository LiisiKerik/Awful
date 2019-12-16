--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Parser (
  Arrow (..),
  Name (..),
  Term (..),
  Term_pattern_0 (..),
  Term_pattern_1 (..),
  --parse_input,
  parse_term) where
  import Data.Bifunctor
  import Errors
  import Tokens
  import Transf
  infixl 3 <+>
  data Arrow = Arrow Term_pattern_1 Term deriving Show
  data Name = Name Line_and_char String deriving Show
  type Parser = Transf () (Tokens, Line_and_char) (Either Line_and_char)
  data Term =
    Application_term Term [Term] |
    Arrow_term Arrow |
    Int_term Integer |
    Match_term Term [Arrow] |
    Name_term Name
      deriving Show
  data Term_pattern_0 =
    Int_term_pattern_0 Integer |
    Name_term_pattern_0 Name
      deriving Show
  data Term_pattern_1 = Term_pattern_1 Line_and_char Term_pattern_0 deriving Show
  (<+>) :: Parser t -> Parser t -> Parser t
  (<+>) =
    f_transf
      (\result_0 -> \result_1 ->
        case (result_0, result_1) of
          (Left line_and_char_0, Left line_and_char_1) -> Left (max line_and_char_0 line_and_char_1)
          (Left line_and_char_0, Right ((tokens, line_and_char_1), x)) ->
            Right ((tokens, max line_and_char_0 line_and_char_1), x)
          (Right ((tokens, line_and_char_0), x), Left line_and_char_1) ->
            Right ((tokens, max line_and_char_0 line_and_char_1), x)
          (Right ((tokens_0, line_and_char_0), x), Right ((tokens_1, line_and_char_1), y)) ->
            case compare (current_line_and_char tokens_0) (current_line_and_char tokens_1) of
              LT -> Right ((tokens_1, line_and_char_1), y)
              EQ -> Left (max line_and_char_0 line_and_char_1)
              GT -> Right ((tokens_0, line_and_char_0), x))
  filter_parser :: (t -> Bool) -> Parser t -> Parser t
  filter_parser f parse_t =
    do
      x <- parse_t
      case f x of
        False ->
          do
            (_, line_and_char) <- get_state
            return_result (Left line_and_char)
        True -> return x
  name_token :: Token -> Maybe String
  name_token token =
    case token of
      Name_token name -> Just name
      _ -> Nothing
  nat_token' :: Token -> Maybe Integer
  nat_token' token =
    case token of
      Nat_token nat -> Just nat
      _ -> Nothing
  parse :: Parser t -> String -> Err t
  parse parse_t text =
    do
      tokens <- tokenise text
      first Parse_error (run (parse_t <* parse_end) () (tokens, init_line_and_char))
  parse_arrow :: Parser Arrow
  parse_arrow = parse_arrow' Arrow parse_term_pattern parse_term'
  parse_arrow' :: (t -> u -> v) -> Parser t -> Parser u -> Parser v
  parse_arrow' f parse_t parse_u = f <$> parse_t <* parse_certain_operator "->" <*> parse_u
  parse_application_term :: Parser Term
  parse_application_term = Application_term <$> parse_atomic_term <*> parse_some parse_atomic_term
{-
  parse_application_term_pattern :: Parser Term_pattern_0
  parse_application_term_pattern =
    Application_term_pattern_0 <$> parse_atomic_term_pattern <*> parse_some parse_atomic_term_pattern
-}
  parse_arrow_term :: Parser Term
  parse_arrow_term = Arrow_term <$> parse_arrow
  parse_atomic_term :: Parser Term
  parse_atomic_term = parse_round parse_term' <+> parse_int_term <+> parse_name_term -- parse_modular_term
  parse_atomic_term_pattern :: Parser Term_pattern_0
  parse_atomic_term_pattern = parse_round parse_term_pattern' <+> parse_int_term_pattern <+> parse_name_term_pattern
  parse_brackets :: Token -> Token -> Parser t -> Parser t
  parse_brackets left_bracket right_bracket parse_t = parse_token left_bracket *> parse_t <* parse_token right_bracket
  parse_certain_operator :: String -> Parser ()
  parse_certain_operator x = parse_token (Operator_token x)
  parse_comma :: Parser ()
  parse_comma = parse_token Comma_token
  parse_curly :: Parser t -> Parser t
  parse_curly = parse_brackets Left_curly_token Right_curly_token
  parse_end :: Parser ()
  parse_end = () <$ filter_parser (\(tokens, _) -> null_tokens tokens) get_state
  parse_int :: Parser Integer
  parse_int = negate <$ parse_token Negate_token <*> parse_nat <+> parse_nat
  parse_int_term :: Parser Term
  parse_int_term = Int_term <$> parse_int -- parse_int_t Int_term
  parse_int_term_pattern :: Parser Term_pattern_0
  parse_int_term_pattern = Int_term_pattern_0 <$> parse_int
  parse_line_and_char :: Parser Line_and_char
  parse_line_and_char = (\(tokens, _) -> current_line_and_char tokens) <$> get_state
  parse_list :: Parser t -> Parser [t]
  parse_list parse_t = (:) <$> parse_t <*> parse_many (parse_comma *> parse_t)
  parse_many :: Parser t -> Parser [t]
  parse_many parse_t = parse_optional_0 (parse_some parse_t)
  parse_match_term :: Parser Term
  parse_match_term = Match_term <$ parse_token Match_token <*> parse_term' <*> parse_curly (parse_list parse_arrow)
  parse_name :: Parser Name
  parse_name = Name <$> parse_line_and_char <*> parse_name'
  parse_name' :: Parser String
  parse_name' = parse_token' name_token
  parse_name_term :: Parser Term
  parse_name_term = Name_term <$> parse_name -- parse_name_t Name_term
  parse_name_term_pattern :: Parser Term_pattern_0
  parse_name_term_pattern = Name_term_pattern_0 <$> parse_name
  parse_nat :: Parser Integer
  parse_nat = parse_token' nat_token'
  parse_optional_0 :: Parser [t] -> Parser [t]
  parse_optional_0 parse_t = return [] <+> parse_t
  parse_round :: Parser t -> Parser t
  parse_round = parse_brackets Left_round_token Right_round_token
  parse_simple_term :: Parser Term
  parse_simple_term = parse_application_term <+> parse_atomic_term
  parse_simple_term_pattern :: Parser Term_pattern_0
  parse_simple_term_pattern = parse_atomic_term_pattern -- parse_application_term_pattern <+> parse_atomic_term_pattern
{-
    parse_simple_t
      Application_term_pattern_0
      (parse_atomic_t
        parse_term_pattern'
        (parse_blank_term_pattern <+> parse_int_term_pattern <+> parse_modular_term_pattern <+> parse_name_term_pattern))
-}
  parse_some :: Parser t -> Parser [t]
  parse_some parse_t = (:) <$> parse_t <*> parse_many parse_t
  parse_term :: String -> Err Term
  parse_term = parse parse_term'
  parse_term' :: Parser Term
  parse_term' = parse_simple_term <+> parse_arrow_term <+> parse_match_term
{-
    (
      parse_branch_term <+>
      parse_let_term <+>
      parse_operators_term)
-}
  parse_term_pattern :: Parser Term_pattern_1
  parse_term_pattern = Term_pattern_1 <$> parse_line_and_char <*> parse_term_pattern'
  parse_term_pattern' :: Parser Term_pattern_0
  parse_term_pattern' = parse_simple_term_pattern -- <+> parse_operators_term_pattern
  parse_token :: Token -> Parser ()
  parse_token token =
    parse_token'
      (\token' ->
        case token == token' of
          False -> Nothing
          True -> Just ())
  parse_token' :: (Token -> Maybe t) -> Parser t
  parse_token' f =
    do
      (tokens, line_and_char) <- get_state
      case nom_token f tokens of
        Left line_and_char' -> return_result (Left (max line_and_char line_and_char'))
        Right (tokens', x) ->
          do
            set_state (tokens', max line_and_char (current_line_and_char tokens))
            return x
--------------------------------------------------------------------------------------------------------------------------------