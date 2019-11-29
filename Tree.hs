--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Tree (
  Arrow_0 (..),
  Associativity (..),
  Binding_0 (..),
  Class_0 (..),
  Constraint_0 (..),
  Constructor_0 (..),
  Data_branch_0 (..),
  Data_0 (..),
  Def_or_instance_0 (..),
  Field_0 (..),
  File_0 (..),
  File_1 (..),
  Input (..),
  Kind_0 (..),
  Method_0 (..),
  Modular_1 (..),
  Operator_0 (..),
  Operator_1 (..),
  Term_0 (..),
  Term_pattern_0 (..),
  Term_pattern_1 (..),
  Type_0 (..),
  Type_1 (..),
  Type_pattern_0 (..),
  Type_pattern_1 (..),
  Type_variable_0 (..),
  parse_file,
  parse_input) where
  import Data.Bifunctor
  import Dictionary
  import Errors
  import Modular
  import Tokenise
  import Transf
  infixl 3 <+>
  data Arrow_0 = Arrow_0 Term_pattern_1 Term_0 deriving Show
  data Associativity = Left_associativity | Right_associativity deriving (Eq, Show)
  data Binding_0 = Binding_0 Term_pattern_1 Term_0 deriving Show
  data Class_0 = Class_0 Line_and_char String Type_variable_0 [Constraint_0] [Method_0] deriving Show
  data Constraint_0 = Constraint_0 Name Name deriving Show
  data Constructor_0 = Constructor_0 Line_and_char Status String [Type_1] deriving Show
  data Data_0 = Data_0 Line_and_char Status String [Type_variable_0] Data_branch_0 deriving Show
  data Data_branch_0 =
    Algebraic_data_0 [Constructor_0] |
    Branch_data_0 Name (Data_branch_0, (Name, Data_branch_0)) |
    Struct_data_0 Line_and_char String [Field_0]
      deriving Show
  data Def_or_instance_0 =
    Def_0 Line_and_char Status String [Type_variable_0] [Constraint_0] [(Term_pattern_1, Type_0)] Type_0 Term_0 |
    Instance_0 Line_and_char Name Type_pattern_1 [Constraint_0] [Binding_0]
      deriving Show
  data Field_0 = Field_0 Name Type_1 deriving Show
  data File_0 = File_0 [Operator_1] [Operator_1] [Data_0] [Class_0] [Def_or_instance_0] deriving Show
  data File_1 = File_1 [Name] File_0 deriving Show
  data Input = Check [Name] | Eval [Name] Term_0 deriving Show
  data Kind_0 = Arrow_kind_0 Kind_0 Kind_0 | Nat_kind_0 | Star_kind_0 deriving (Eq, Show)
  data Method_0 = Method_0 Name [Type_variable_0] Type_1 deriving Show
  data Modular_1 = Modular_1 Line_and_char Modular_0 deriving Show
  data Operator_0 = Operator_0 String Integer Associativity deriving Show
  data Operator_1 = Operator_1 Line_and_char String Operator_0 deriving Show
  type Parser = Transf () (Tokens, Line_and_char) (Either Line_and_char)
  data Term_0 =
    Application_term_0 Term_0 [Term_0] |
    Arrow_term_0 Arrow_0 |
    Branch_term_0 Name (Term_0, (Type_pattern_0, Term_0)) |
    Int_term_0 Integer |
    Let_term_0 [Binding_0] Term_0 |
    Match_term_0 Term_0 [Arrow_0] |
    Modular_term_0 Modular_1 |
    Name_term_0 Name |
    Operators_term_0 Term_0 [(Name, Term_0)]
      deriving Show
  data Term_pattern_0 =
    Application_term_pattern_0 Term_pattern_0 [Term_pattern_0] |
    Blank_term_pattern_0 |
    Int_term_pattern_0 Integer |
    Modular_term_pattern_0 Modular_1 |
    Name_term_pattern_0 Name |
    Operators_term_pattern_0 Term_pattern_0 [(Name, Term_pattern_0)]
      deriving Show
  data Term_pattern_1 = Term_pattern_1 Line_and_char Term_pattern_0 deriving Show
  data Type_0 =
    Application_type_0 Type_0 [Type_0] | Name_type_0 Name | Nat_type_0 Integer | Operators_type_0 Type_0 [(Name, Type_0)]
      deriving Show
  data Type_1 = Type_1 Line_and_char Type_0 deriving Show
  data Type_pattern_0 = Blank_type_pattern_0 | Name_type_pattern_0 Name deriving Show
  data Type_pattern_1 =
    Application_type_pattern_1 Name [Type_pattern_0] | Operator_type_pattern_1 Type_pattern_0 Name Type_pattern_0
      deriving Show
  data Type_variable_0 = Type_variable_0 Name Kind_0 deriving Show
  (<+>) :: Parser t -> Parser t -> Parser t
  Transf parse_0 <+> Transf parse_1 =
    Transf
      (return
        (\state ->
          case (parse_0 () state, parse_1 () state) of
            (Left line_and_char_0, Left line_and_char_1) -> Left (max line_and_char_0 line_and_char_1)
            (Left line_and_char_0, Right ((tokens, line_and_char_1), x)) ->
              Right ((tokens, max line_and_char_0 line_and_char_1), x)
            (Right ((tokens, line_and_char_0), x), Left line_and_char_1) ->
              Right ((tokens, max line_and_char_0 line_and_char_1), x)
            (Right ((tokens_0, line_and_char_0), x), Right ((tokens_1, line_and_char_1), y)) ->
              Right
                (case compare (current_line_and_char tokens_0) (current_line_and_char tokens_1) of
                  LT -> ((tokens_1, line_and_char_1), y)
                  _ -> ((tokens_0, line_and_char_0), x))))
  filter_parser :: (t -> Bool) -> Parser t -> Parser t
  filter_parser f parse_t =
    (
      parse_t >>=
      \x ->
        case f x of
          False -> Transf (return (\(_, line_and_char) -> Left line_and_char))
          True -> return x)
  init_line_and_char :: Line_and_char
  init_line_and_char = Line_and_char 0 0
  invalid_term_operators :: [String]
  invalid_term_operators = ["#", "-", "->"]
  invalid_type_operators :: [String]
  invalid_type_operators = [":"]
  parse :: Parser t -> String -> String -> Err t
  parse parse_t file_name text =
    (
      tokenise file_name text >>=
      \tokens -> bimap (Parse_error file_name) snd (transf (parse_t <* parse_end) () (tokens, init_line_and_char)))
  parse_algebraic_data :: Parser Data_branch_0
  parse_algebraic_data = Algebraic_data_0 <$ parse_token Algebraic_token <*> parse_curly (parse_list parse_constructor)
  parse_angular :: Parser t -> Parser t
  parse_angular parse_t = parse_brackets (Operator_token "<") parse_t (Operator_token ">")
  parse_application_t :: (t -> [t] -> t) -> Parser t -> Parser t
  parse_application_t application parse_t = application <$> parse_t <*> parse_some parse_t
  parse_application_type_pattern :: Parser Type_pattern_1
  parse_application_type_pattern = Application_type_pattern_1 <$> parse_name <*> parse_many parse_type_pattern_0
  parse_arrow :: Parser Arrow_0
  parse_arrow = parse_arrow' Arrow_0 parse_term_pattern parse_term
  parse_arrow' :: (t -> u -> v) -> Parser t -> Parser u -> Parser v
  parse_arrow' f parse_t parse_u = f <$> parse_t <* parse_certain_operator "->" <*> parse_u
  parse_arrow_kind :: Parser Kind_0
  parse_arrow_kind = parse_arrow' Arrow_kind_0 (parse_round parse_arrow_kind <+> parse_atomic_kind) parse_kind
  parse_arrow_term :: Parser Term_0
  parse_arrow_term = Arrow_term_0 <$> parse_arrow
  parse_associativity :: Parser Associativity
  parse_associativity = parse_left_associativity <+> parse_right_associativity
  parse_atomic_kind :: Parser Kind_0
  parse_atomic_kind = parse_nat_kind <+> parse_star_kind
  parse_atomic_t :: Parser t -> Parser t -> Parser t
  parse_atomic_t parse_0 parse_1 = parse_round parse_0 <+> parse_1
  parse_atomic_type :: Parser Type_0
  parse_atomic_type = parse_atomic_t parse_type_0 (parse_name_type <+> parse_nat_type)
  parse_blank_t :: t -> Parser t
  parse_blank_t blank = blank <$ parse_token Blank_token
  parse_blank_term_pattern :: Parser Term_pattern_0
  parse_blank_term_pattern = parse_blank_t Blank_term_pattern_0
  parse_blank_type_pattern :: Parser Type_pattern_0
  parse_blank_type_pattern = parse_blank_t Blank_type_pattern_0
  parse_binding :: Parser Binding_0
  parse_binding = Binding_0 <$> parse_term_pattern <* parse_eq <*> parse_term
  parse_brackets :: Token_0 -> Parser t -> Token_0 -> Parser t
  parse_brackets left_bracket parse_t right_bracket = parse_token left_bracket *> parse_t <* parse_token right_bracket
  parse_branch :: (Name -> (t, (u, t)) -> t) -> Parser t -> Parser u -> Parser t
  parse_branch branch parse_t parse_u =
    (
      branch <$
      parse_token Branch_token <*>
      parse_name <*>
      parse_curly
        (
          (,) <$>
          parse_arrow' (return id) (parse_certain_name "Zero") parse_t <*
          parse_comma <*>
          parse_arrow' (,) (parse_certain_name "Next" *> parse_u) parse_t))
  parse_branch_data :: Parser Data_branch_0
  parse_branch_data = parse_branch Branch_data_0 parse_data_0 parse_name
  parse_branch_term :: Parser Term_0
  parse_branch_term = parse_branch Branch_term_0 parse_term parse_type_pattern_0
  parse_certain_name :: String -> Parser ()
  parse_certain_name x = parse_token (Name_token x)
  parse_certain_operator :: String -> Parser ()
  parse_certain_operator x = parse_token (Operator_token x)
  parse_check :: Parser Input
  parse_check = Check <$ parse_token Check_token <*> parse_file_names
  parse_class :: Parser Class_0
  parse_class =
    (
      Class_0 <$>
      parse_line_and_char <*
      parse_token Class_token <*>
      parse_name' <*>
      parse_curly parse_type_variable <*>
      parse_constraints <*>
      parse_optional_1 parse_round parse_method)
  parse_colon :: Parser ()
  parse_colon = parse_certain_operator ":"
  parse_comma :: Parser ()
  parse_comma = parse_token Comma_token
  parse_constraint :: Parser Constraint_0
  parse_constraint = Constraint_0 <$> parse_name <*> parse_name
  parse_constraints :: Parser [Constraint_0]
  parse_constraints = parse_optional_1 parse_angular parse_constraint
  parse_constructor :: Parser Constructor_0
  parse_constructor =
    (
      Constructor_0 <$>
      parse_line_and_char <*>
      parse_status <*>
      parse_name' <*>
      parse_many (Type_1 <$> parse_line_and_char <*> parse_atomic_type))
  parse_curly :: Parser t -> Parser t
  parse_curly parser = parse_brackets Left_curly_token parser Right_curly_token
  parse_data_0 :: Parser Data_branch_0
  parse_data_0 = parse_algebraic_data <+> parse_branch_data <+> parse_struct_data
  parse_data_1 :: Parser Data_0
  parse_data_1 =
    (
      Data_0 <$>
      parse_line_and_char <*>
      parse_status <*
      parse_token Data_token <*>
      parse_name' <*>
      parse_type_variables <*
      parse_eq <*>
      parse_data_0)
  parse_def :: Parser Def_or_instance_0
  parse_def =
    (
      Def_0 <$>
      parse_line_and_char <*>
      parse_status <*
      parse_token Def_token <*>
      parse_name' <*>
      parse_type_variables <*>
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
      parse_colon <*>
      parse_type_0 <*
      parse_eq <*>
      parse_term)
  parse_def_or_instance :: Parser Def_or_instance_0
  parse_def_or_instance = parse_def <+> parse_instance
  parse_end :: Parser ()
  parse_end = () <$ filter_parser (\(tokens, _) -> null_tokens tokens) get_state
  parse_eq :: Parser ()
  parse_eq = parse_token Eq_token
  parse_eval :: Parser Input
  parse_eval = Eval <$ parse_token Eval_token <*> parse_optional_0 parse_file_names <*> parse_term
  parse_file :: String -> String -> Err File_1
  parse_file file_name =
    parse
      (
        File_1 <$>
        parse_optional_0 (parse_token Load_token *> parse_file_names) <*>
        (
          File_0 <$>
          parse_many (parse_operator_definition Type_token invalid_type_operators) <*>
          parse_many (parse_operator_definition Term_token invalid_term_operators) <*>
          parse_many parse_data_1 <*>
          parse_many parse_class <*>
          parse_many parse_def_or_instance))
      file_name
  parse_file_name :: Parser Name
  parse_file_name =
    (
      (\line_and_char -> \file_name -> Name line_and_char (file_name ++ ".awf")) <$>
      parse_line_and_char <*>
      parse_name' <*
      parse_certain_operator "." <*
      parse_certain_name "awf")
  parse_file_names :: Parser [Name]
  parse_file_names = parse_round (parse_list parse_file_name)
  parse_input :: String -> Err Input
  parse_input = parse (parse_check <+> parse_eval) "input"
  parse_instance :: Parser Def_or_instance_0
  parse_instance =
    (
      Instance_0 <$>
      parse_line_and_char <*
      parse_token Instance_token <*>
      parse_name <*>
      parse_curly parse_type_pattern_1 <*>
      parse_constraints <*>
      parse_optional_1 parse_round parse_binding)
  parse_int :: Parser Integer
  parse_int = negate <$ parse_certain_operator "-" <*> filter_parser ((/=) 0) parse_nat <+> parse_nat
  parse_int_t :: (Integer -> t) -> Parser t
  parse_int_t int = int <$> parse_int
  parse_int_term :: Parser Term_0
  parse_int_term = parse_int_t Int_term_0
  parse_int_term_pattern :: Parser Term_pattern_0
  parse_int_term_pattern = parse_int_t Int_term_pattern_0
  parse_kind :: Parser Kind_0
  parse_kind = parse_arrow_kind <+> parse_atomic_kind
  parse_left_associativity :: Parser Associativity
  parse_left_associativity = Left_associativity <$ parse_certain_name "Left"
  parse_let_term :: Parser Term_0
  parse_let_term = Let_term_0 <$ parse_token Let_token <*> parse_list parse_binding <* parse_token In_token <*> parse_term
  parse_line_and_char :: Parser Line_and_char
  parse_line_and_char = (\(tokens, _) -> current_line_and_char tokens) <$> get_state
  parse_list :: Parser t -> Parser [t]
  parse_list parse_t = (:) <$> parse_t <*> parse_many (parse_comma *> parse_t)
  parse_many :: Parser t -> Parser [t]
  parse_many parse_t = parse_optional_0 (parse_some parse_t)
  parse_match_term :: Parser Term_0
  parse_match_term = Match_term_0 <$ parse_token Match_token <*> parse_term <*> parse_curly (parse_list parse_arrow)
  parse_method :: Parser Method_0
  parse_method = Method_0 <$> parse_name <*> parse_type_variables <* parse_colon <*> parse_type_1
  parse_modular :: Parser Modular_1
  parse_modular =
    Modular_1 <$> parse_line_and_char <*> (flip Modular_0 <$> parse_nat <* parse_certain_operator "#" <*> parse_nat)
  parse_modular_t :: (Modular_1 -> t) -> Parser t
  parse_modular_t modular = modular <$> parse_modular
  parse_modular_term :: Parser Term_0
  parse_modular_term = parse_modular_t Modular_term_0
  parse_modular_term_pattern :: Parser Term_pattern_0
  parse_modular_term_pattern = parse_modular_t Modular_term_pattern_0
  parse_name :: Parser Name
  parse_name = Name <$> parse_line_and_char <*> parse_name'
  parse_name' :: Parser String
  parse_name' =
    parse_token'
      (\token ->
        case token of
          Name_token x -> Just x
          _ -> Nothing)
  parse_name_t :: (Name -> t) -> Parser t
  parse_name_t name = name <$> parse_name
  parse_name_term :: Parser Term_0
  parse_name_term = parse_name_t Name_term_0
  parse_name_term_pattern :: Parser Term_pattern_0
  parse_name_term_pattern = parse_name_t Name_term_pattern_0
  parse_name_type :: Parser Type_0
  parse_name_type = parse_name_t Name_type_0
  parse_name_type_pattern :: Parser Type_pattern_0
  parse_name_type_pattern = parse_name_t Name_type_pattern_0
  parse_nat :: Parser Integer
  parse_nat =
    parse_token'
      (\token ->
        case token of
          Nat_token x -> Just x
          _ -> Nothing)
  parse_nat_kind :: Parser Kind_0
  parse_nat_kind = Nat_kind_0 <$ parse_certain_name "Nat"
  parse_nat_type :: Parser Type_0
  parse_nat_type = Nat_type_0 <$> parse_nat
  parse_operator :: [String] -> Parser Name
  parse_operator invalid_operators = Name <$> parse_line_and_char <*> parse_operator' invalid_operators
  parse_operator' :: [String] -> Parser String
  parse_operator' invalid_operators =
    filter_parser
      (\operator -> notElem operator invalid_operators)
      (parse_token'
        (\token ->
          case token of
            Operator_token x -> Just x
            _ -> Nothing))
  parse_operator_definition :: Token_0 -> [String] -> Parser Operator_1
  parse_operator_definition level invalid_operators =
    (
      Operator_1 <$>
      parse_line_and_char <*
      parse_token level <*>
      parse_operator' invalid_operators <*>
      (Operator_0 <$> parse_name' <*> parse_nat <*> parse_associativity))
  parse_operator_type_pattern :: Parser Type_pattern_1
  parse_operator_type_pattern =
    Operator_type_pattern_1 <$> parse_type_pattern_0 <*> parse_type_operator <*> parse_type_pattern_0
  parse_operators_t :: (t -> [(Name, t)] -> t) -> Parser t -> [String] -> Parser t
  parse_operators_t operators parse_t invalid_operators =
    operators <$> parse_t <*> parse_some ((,) <$> parse_operator invalid_operators <*> parse_t)
  parse_operators_term :: Parser Term_0
  parse_operators_term = parse_operators_t Operators_term_0 parse_simple_term invalid_term_operators
  parse_operators_term_pattern :: Parser Term_pattern_0
  parse_operators_term_pattern = parse_operators_t Operators_term_pattern_0 parse_simple_term_pattern invalid_term_operators
  parse_operators_type :: Parser Type_0
  parse_operators_type = parse_operators_t Operators_type_0 parse_simple_type invalid_type_operators
  parse_optional_0 :: Parser [t] -> Parser [t]
  parse_optional_0 parse_t = return [] <+> parse_t
  parse_optional_1 :: (Parser [t] -> Parser [t]) -> Parser t -> Parser [t]
  parse_optional_1 f parse_t = parse_optional_0 (f (parse_list parse_t))
  parse_right_associativity :: Parser Associativity
  parse_right_associativity = Right_associativity <$ parse_certain_name "Right"
  parse_round :: Parser t -> Parser t
  parse_round parser = parse_brackets Left_round_token parser Right_round_token
  parse_simple_t :: (t -> [t] -> t) -> Parser t -> Parser t
  parse_simple_t application parse_t = parse_application_t application parse_t <+> parse_t
  parse_simple_term :: Parser Term_0
  parse_simple_term =
    parse_simple_t Application_term_0 (parse_atomic_t parse_term (parse_int_term <+> parse_modular_term <+> parse_name_term))
  parse_simple_term_pattern :: Parser Term_pattern_0
  parse_simple_term_pattern =
    parse_simple_t
      Application_term_pattern_0
      (parse_atomic_t
        parse_term_pattern'
        (parse_blank_term_pattern <+> parse_int_term_pattern <+> parse_modular_term_pattern <+> parse_name_term_pattern))
  parse_simple_type :: Parser Type_0
  parse_simple_type = parse_simple_t Application_type_0 (parse_atomic_t parse_type_0 (parse_name_type <+> parse_nat_type))
  parse_some :: Parser t -> Parser [t]
  parse_some parse_t = (:) <$> parse_t <*> parse_many parse_t
  parse_square :: Parser t -> Parser t
  parse_square parse_t = parse_brackets Left_square_token parse_t Right_square_token
  parse_star_kind :: Parser Kind_0
  parse_star_kind = Star_kind_0 <$ parse_certain_name "Star"
  parse_status :: Parser Status
  parse_status = Private <$ parse_certain_operator "!" <+> return Public
  parse_struct_data :: Parser Data_branch_0
  parse_struct_data =
    (
      Struct_data_0 <$>
      parse_line_and_char <*
      parse_token Struct_token <*>
      parse_name' <*>
      parse_optional_1 parse_round (Field_0 <$> parse_name <* parse_colon <*> parse_type_1))
  parse_term :: Parser Term_0
  parse_term =
    (
      parse_simple_term <+>
      parse_arrow_term <+>
      parse_branch_term <+>
      parse_let_term <+>
      parse_match_term <+>
      parse_operators_term)
  parse_term_operator :: Parser Name
  parse_term_operator = parse_operator ["->"]
  parse_term_pattern :: Parser Term_pattern_1
  parse_term_pattern = Term_pattern_1 <$> parse_line_and_char <*> parse_term_pattern'
  parse_term_pattern' :: Parser Term_pattern_0
  parse_term_pattern' = parse_simple_term_pattern <+> parse_operators_term_pattern
  parse_term_pattern_and_type :: Parser ((Term_pattern_0, [(Name, Term_pattern_0)]), Type_0)
  parse_term_pattern_and_type =
    (
      (\a -> first ((,) a)) <$>
      parse_simple_term_pattern <*>
      (
        (,) [] <$ parse_colon <*> parse_type_0 <+>
        (\a -> first (\(d, e) -> (a, d) : e)) <$> parse_term_operator <*> parse_term_pattern_and_type))
  parse_token :: Token_0 -> Parser ()
  parse_token token_0 =
    parse_token'
      (\token_1 ->
        case token_0 == token_1 of
          False -> Nothing
          True -> Just ())
  parse_token' :: (Token_0 -> Maybe t) -> Parser t
  parse_token' f =
    Transf
      (return
        (\(tokens_0, line_and_char) ->
          bimap
            (max line_and_char)
            (first (\tokens_1 -> (tokens_1, max line_and_char (current_line_and_char tokens_0))))
            (nom_token f tokens_0)))
  parse_type_0 :: Parser Type_0
  parse_type_0 = parse_simple_type <+> parse_operators_type
  parse_type_1 :: Parser Type_1
  parse_type_1 = Type_1 <$> parse_line_and_char <*> parse_type_0
  parse_type_operator :: Parser Name
  parse_type_operator = parse_operator invalid_type_operators
  parse_type_pattern_0 :: Parser Type_pattern_0
  parse_type_pattern_0 = parse_blank_type_pattern <+> parse_name_type_pattern
  parse_type_pattern_1 :: Parser Type_pattern_1
  parse_type_pattern_1 = parse_application_type_pattern <+> parse_operator_type_pattern
  parse_type_variable :: Parser Type_variable_0
  parse_type_variable = Type_variable_0 <$> parse_name <* parse_colon <*> parse_kind
  parse_type_variables :: Parser [Type_variable_0]
  parse_type_variables = parse_optional_1 parse_square parse_type_variable
--------------------------------------------------------------------------------------------------------------------------------