--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Parser (
  Arrow (..),
  Code_file (..),
  Code_file' (..),
  Def_or_instance (..),
  Input (..),
  Kind (..),
  Term (..),
  Term_pattern (..),
  Test (..),
  Test_file (..),
  Type (..),
  Type_variable,
  parse_code_file,
  parse_input,
  parse_test_file) where
  import Control.Applicative (Alternative (..))
  import Control.Monad.State.Strict (MonadState (..), StateT (..))
  import Control.Monad.Writer.Strict (MonadWriter (..))
  import Errors (Err, Err', Error' (..), L (..), Line_and_char, add_file_name)
  import Tokens (Token (..), Tokens, current_line_and_char, nom_token, null, tokenise)
  import Transf (PairT (..), SPT, evalSPT, runSPT)
  data Arrow = Arrow (L Term_pattern) Term
      deriving Show
  data Code_file = Code_file [Def_or_instance]
      deriving Show
  data Code_file' = Code_file' [L String] Code_file
      deriving Show
  type Parser = SPT Tokens Line_and_char Maybe
{-
    Def_0 Line_and_char Status String [Type_variable_0] [Constraint_0] [(Term_pattern_1, Type_0)] Type_0 Term_0 |
    Instance_0 Line_and_char (L String) Type_pattern_1 [Constraint_0] [Binding_0]
-}
  data Def_or_instance = Def Line_and_char String [Type_variable] (L Type) Term
      deriving Show
  data Input = Check [L String] | Eval [L String] Term | Run [L String]
      deriving Show
  data Kind = Arrow_kind Kind Kind | Nat_kind | Type_kind | Variable_kind Integer
      deriving Show
  data Term =
    Application_term Term Term |
    Arrow_term Arrow |
    Int_term Integer |
    Match_term Term [Arrow] |
    Name_term (L String)
      deriving Show
  data Term_pattern =
    Blank_term_pattern |
    Int_term_pattern Integer |
    Name_term_pattern (L String)
      deriving Show
  data Test = Test Line_and_char Term Term
      deriving Show
  data Test_file = Test_file [L String] [Test]
      deriving Show
{-
  data Type_0 = Nat_type_0 Integer | Operators_type_0 Type_0 [(Name, Type_0)]
-}
  data Type = Application_type Type Type | Name_type (L String)
      deriving Show
  type Type_variable = (L String, Kind)
  infixl 3 <+>
  (<+>) :: Parser t -> Parser t -> Parser t
  parse_0 <+> parse_1 =
    StateT
      (\tokens ->
        let
          (line_and_char_0, maybe_result_0) = runSPT parse_0 tokens
          (line_and_char_1, maybe_result_1) = runSPT parse_1 tokens
      in
        PairT
          (
            max line_and_char_0 line_and_char_1,
            case (maybe_result_0, maybe_result_1) of
              (Nothing, Nothing) -> Nothing
              (Nothing, Just result) -> Just result
              (Just result, Nothing) -> Just result
              (Just result_0, Just result_1) ->
                case compare (Parser.current_line_and_char result_0) (Parser.current_line_and_char result_1) of
                  LT -> Just result_1
                  EQ -> Nothing
                  GT -> Just result_0))
  current_line_and_char :: (t, Tokens) -> Line_and_char
  current_line_and_char (_, tokens) = Tokens.current_line_and_char tokens
  name_token :: Token -> Maybe String
  name_token token =
    case token of
      Name_token name -> Just name
      _ -> Nothing
  nat_token :: Token -> Maybe Integer
  nat_token token =
    case token of
      Nat_token nat -> Just nat
      _ -> Nothing
  parse :: Parser t -> String -> Err' t
  parse parse_t text =
    do
      tokens <- tokenise text
      let (line_and_char, maybe_x) = evalSPT (parse_end parse_t) tokens
      case maybe_x of
        Nothing -> Left (Parse_error line_and_char)
        Just x -> Right x
  parse_application_term :: Parser Term
  parse_application_term = foldl Application_term <$> parse_atomic_term <*> parse_some parse_atomic_term
{-
  parse_application_term_pattern :: Parser Term_pattern_0
  parse_application_term_pattern =
    Application_term_pattern_0 <$> parse_atomic_term_pattern <*> parse_some parse_atomic_term_pattern
-}
  parse_application_type :: Parser Type
  parse_application_type = foldl Application_type <$> parse_atomic_type <*> parse_some parse_atomic_type
  parse_arrow :: Parser Arrow
  parse_arrow = parse_arrow' Arrow (parse_l parse_term_pattern) parse_term
  parse_arrow' :: (t -> u -> v) -> Parser t -> Parser u -> Parser v
  parse_arrow' f parse_t parse_u =
    do
      x <- parse_t
      parse_certain_operator "->"
      y <- parse_u
      return (f x y)
  parse_arrow_kind :: Parser Kind
  parse_arrow_kind = parse_arrow' Arrow_kind parse_simple_kind parse_kind
  parse_arrow_term :: Parser Term
  parse_arrow_term = Arrow_term <$> parse_arrow
  parse_atomic_term :: Parser Term
  parse_atomic_term = parse_round parse_term <+> parse_int_term <+> parse_name_term -- parse_modular_term
  parse_atomic_term_pattern :: Parser Term_pattern
  parse_atomic_term_pattern =
    (
      parse_round parse_term_pattern <+>
      parse_blank_term_pattern <+>
      parse_int_term_pattern <+>
      parse_name_term_pattern)  -- parse_modular_term_pattern
  parse_atomic_type :: Parser Type
  parse_atomic_type = parse_round parse_type <+> parse_name_type -- parse_nat_type
  parse_blank :: Parser ()
  parse_blank = parse_token Blank_token
  parse_blank_term_pattern :: Parser Term_pattern
  parse_blank_term_pattern =
    do
      parse_blank
      return Blank_term_pattern
  parse_brackets :: Token -> Token -> Parser t -> Parser t
  parse_brackets left_bracket right_bracket parse_t =
    do
      parse_token left_bracket
      x <- parse_t
      parse_token right_bracket
      return x
  parse_certain_name :: String -> Parser ()
  parse_certain_name name = parse_token (Name_token name)
  parse_certain_operator :: String -> Parser ()
  parse_certain_operator operator = parse_token (Operator_token operator)
  parse_check :: Parser Input
  parse_check =
    do
      parse_token Check_token
      file_names <- parse_imports
      return (Check file_names)
  parse_code_file :: String -> Err' Code_file'
  parse_code_file = parse (Code_file' <$> parse_import <*> (Code_file <$> parse_many parse_def_or_instance))
  parse_colon :: Parser ()
  parse_colon = parse_certain_operator ":"
  parse_comma :: Parser ()
  parse_comma = parse_token Comma_token
  parse_curly :: Parser t -> Parser t
  parse_curly = parse_brackets Left_curly_token Right_curly_token
{-
      parse_constraints <*>
      parse_optional_1
        parse_round
        (
          (\line_and_char ->
            first
              (\(term_pattern, operator_sequence) ->
                Term_pattern_1 line_and_char (Operators_term_pattern_0 term_pattern operator_sequence))) <$>
          parse_line_and_char <*>
          parse_term_pattern_and_type) <*
-}
  parse_def :: Parser Def_or_instance
  parse_def =
    do
      line_and_char <- parse_line_and_char
      parse_token Def_token
      name <- parse_name'
      type_variables <- parse_type_variables
      parse_colon
      typ <- parse_l parse_type
      parse_eq
      term <- parse_term
      return (Def line_and_char name type_variables typ term)
  parse_def_or_instance :: Parser Def_or_instance
  parse_def_or_instance = parse_def -- <+> parse_instance
  parse_element :: Parser t -> Parser t
  parse_element parse_t =
    do
      parse_comma
      parse_t
  parse_end :: Parser t -> Parser t
  parse_end parse_t =
    do
      x <- parse_t
      parse_end'
      return x
  parse_end' :: Parser ()
  parse_end' =
    do
      tokens <- get
      tell (Tokens.current_line_and_char tokens)
      case Tokens.null tokens of
        False -> empty
        True -> return ()
  parse_eq :: Parser ()
  parse_eq = parse_token Eq_token
  parse_eval :: Parser Input
  parse_eval =
    do
      parse_token Eval_token
      file_names <- parse_imports
      term <- parse_term
      return (Eval file_names term)
  parse_file_name :: String -> Parser (L String)
  parse_file_name extension =
    do
      line_and_char <- parse_line_and_char
      name <- parse_name'
      parse_certain_operator "."
      parse_certain_name extension
      return (L line_and_char (name ++ "." ++ extension))
  parse_file_names :: String -> Parser [L String]
  parse_file_names ext = parse_round (parse_optional_0 (parse_list (parse_file_name ext)))
  parse_import :: Parser [L String]
  parse_import =
    do
      parse_token Import_token
      parse_imports
  parse_imports :: Parser [L String]
  parse_imports = parse_file_names "awf"
  parse_input :: String -> Err Input
  parse_input input = add_file_name "input" (parse (parse_check <+> parse_eval <+> parse_run) input)
  parse_int :: Parser Integer
  parse_int = parse_negative_int <+> parse_nat
  parse_int_term :: Parser Term
  parse_int_term = Int_term <$> parse_int
  parse_int_term_pattern :: Parser Term_pattern
  parse_int_term_pattern = Int_term_pattern <$> parse_int
  parse_kind :: Parser Kind
  parse_kind = parse_arrow_kind <+> parse_simple_kind
  parse_l :: Parser t -> Parser (L t)
  parse_l parse_t = L <$> parse_line_and_char <*> parse_t
  parse_line_and_char :: Parser Line_and_char
  parse_line_and_char =
    do
      tokens <- get
      return (Tokens.current_line_and_char tokens)
  parse_list :: Parser t -> Parser [t]
  parse_list parse_t = (:) <$> parse_t <*> parse_many (parse_element parse_t)
  parse_many :: Parser t -> Parser [t]
  parse_many parse_t = parse_optional_0 (parse_some parse_t)
  parse_match_term :: Parser Term
  parse_match_term =
    do
      parse_token Match_token
      term <- parse_term
      arrows <- parse_curly (parse_list parse_arrow)
      return (Match_term term arrows)
-- rem parse_name
  parse_name :: Parser (L String)
  parse_name = parse_l parse_name'
  parse_name' :: Parser String
  parse_name' = parse_token' name_token
  parse_name_term :: Parser Term
  parse_name_term = Name_term <$> parse_name
  parse_name_term_pattern :: Parser Term_pattern
  parse_name_term_pattern = Name_term_pattern <$> parse_name
  parse_name_type :: Parser Type
  parse_name_type = Name_type <$> parse_l parse_name'
  parse_nat :: Parser Integer
  parse_nat = parse_token' nat_token
  parse_nat_kind :: Parser Kind
  parse_nat_kind =
    do
      parse_certain_name "Nat"
      return Nat_kind
  parse_negative_int :: Parser Integer
  parse_negative_int =
    do
      parse_token Negate_token
      i <- parse_nat
      return (negate i)
  parse_optional_0 :: Parser [t] -> Parser [t]
  parse_optional_0 parse_t = return [] <+> parse_t
  parse_optional_1 :: (Parser [t] -> Parser [t]) -> Parser t -> Parser [t]
  parse_optional_1 f parse_t = parse_optional_0 (f (parse_list parse_t))
  parse_round :: Parser t -> Parser t
  parse_round = parse_brackets Left_round_token Right_round_token
  parse_run :: Parser Input
  parse_run =
    do
      parse_token Run_token
      file_names <- parse_file_names "test"
      return (Run file_names)
  parse_simple_kind :: Parser Kind
  parse_simple_kind = parse_round parse_kind <+> parse_nat_kind <+> parse_type_kind
  parse_simple_term :: Parser Term
  parse_simple_term = parse_application_term <+> parse_atomic_term
  parse_simple_term_pattern :: Parser Term_pattern
  parse_simple_term_pattern = parse_atomic_term_pattern -- parse_application_term_pattern <+> parse_atomic_term_pattern
  parse_simple_type :: Parser Type
  parse_simple_type = parse_application_type <+> parse_atomic_type
  parse_some :: Parser t -> Parser [t]
  parse_some parse_t = (:) <$> parse_t <*> parse_many parse_t
  parse_square :: Parser t -> Parser t
  parse_square = parse_brackets Left_square_token Right_square_token
  parse_term :: Parser Term
  parse_term = parse_simple_term <+> parse_arrow_term <+> parse_match_term
{-
    (
      parse_branch_term <+>
      parse_let_term <+>
      parse_operators_term)
-}
  parse_term_pattern :: Parser Term_pattern
  parse_term_pattern = parse_simple_term_pattern -- <+> parse_operators_term_pattern
  parse_test :: Parser Test
  parse_test =
    do
      line_and_char <- parse_line_and_char
      parse_token Test_token
      term_0 <- parse_term
      parse_eq
      term_1 <- parse_term
      return (Test line_and_char term_0 term_1)
  parse_test_file :: String -> Err' Test_file
  parse_test_file = parse (Test_file <$> parse_import <*> parse_many parse_test)
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
      tokens <- get
      tell (Tokens.current_line_and_char tokens)
      case nom_token f tokens of
        Nothing -> empty
        Just (x, tokens') ->
          do
            put tokens'
            return x
  parse_type :: Parser Type
  parse_type = parse_simple_type -- <+> parse_operators_type
  parse_type_keyword :: Parser ()
  parse_type_keyword = parse_token Type_token
  parse_type_kind :: Parser Kind
  parse_type_kind =
    do
      parse_type_keyword
      return Type_kind
  parse_type_variable :: Parser Type_variable
  parse_type_variable =
    do
      name <- parse_name
      parse_colon
      kind <- parse_kind
      return (name, kind)
  parse_type_variables :: Parser [Type_variable]
  parse_type_variables = parse_optional_1 parse_square parse_type_variable
--------------------------------------------------------------------------------------------------------------------------------