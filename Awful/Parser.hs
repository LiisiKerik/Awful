{-
expr op op expr - esimene op on binaarne operaator, teine op on unaarne miinus
expr op expr - op on binaarne operaator
op expr - op on unaarne miinus
-}
-- todo: add back syntactic sugar for lists (and other recursive structs)
module Awful.Parser (
  Assoc (..),
  Brnch_0 (..),
  Class_0 (..),
  Constraint_0 (..),
  Data_0 (..),
  Data_br_0 (..),
  Data_branch_0 (..),
  Def_0 (..),
  Eqq (..),
  Expression_0 (..),
  Kind (..),
  Match_Int_0 (..),
  Match_Modular_0 (..),
  Match_unnamed_algebraic_0 (..),
  Matches_0 (..),
  Method (..),
  Modular (..),
  Name (..),
  Opdecl_0 (..),
  Pat (..),
  Pat_branch (..),
  Pattern_0 (..),
  Pattern_1 (..),
  Tree_0 (..),
  Tree_1 (..),
  Type_0 (..),
  Type_7 (..),
  Unnamed_form_0 (..),
  Get_location (..),
  parse_expression,
  parse_tree) where
  import Awful.Tokeniser
  import Control.Applicative
  import Data.Bifunctor
  import Data.Maybe
  import Parser.Locations
  import Parser.Parser
  data Assoc = Lft | Rght deriving (Eq, Show)
  data Brnch_0 = Brnch_0 Name [Name] Name [(Name, Type_7)] deriving Show
  data Class_0 = Class_0 Name (Name, Kind) (Maybe Name) [Method] deriving Show
  data Constraint_0 = Constraint_0 Name Name deriving Show
  data Data_0 = Data_0 Name Data_br_0 deriving Show
  data Data_br_0 = Branching_data_0 [(Name, Kind)] [Brnch_0] | Plain_data_0 [(Name, Kind)] Data_branch_0 deriving Show
  data Data_branch_0 = Named_struct_data_0 [(Name, Type_7)] | Unnamed_algebraic_data_0 [Unnamed_form_0] deriving Show
  data Def_0 =
    Basic_def_0 Name [(Name, Kind)] [Constraint_0] [(Pat, Type_7)] Type_7 Expression_0 |
    Instance_def_0 Location Name Name [Kind] [Pattern_1] [Constraint_0] [(Name, ([Pat], Expression_0))]
      deriving Show
  data Eqq = Eqq Name [Pat] Expression_0 deriving Show
  data Expression_0 =
    Application_expression_0 Expression_0 [Expression_0] |
    Function_expression_0 Pat Expression_0 |
    Int_expression_0 Integer |
    Let_expression_0 Eqq Expression_0 |
    Match_expression_0 Location Expression_0 Matches_0 |
    Modular_expression_0 Modular |
    Name_expression_0 Name (Maybe Type_7) [Type_7] |
    Op_expression_0 Expression_0 [(Name, Expression_0)]
      deriving Show
  data Unnamed_form_0 = Unnamed_form_0 Name [Type_7] deriving Show
  data Kind = Function_kind Kind Kind | Nat_kind | Type_kind deriving (Eq, Show)
  data Match_Int_0 = Match_Int_0 Location Integer Expression_0 deriving Show
  data Match_Modular_0 = Match_Modular_0 Location Modular Expression_0 deriving Show
  data Match_unnamed_algebraic_0 = Match_unnamed_algebraic_0 Name [Pat] Expression_0 deriving Show
  data Matches_0 =
    Matches_Int_0 [Match_Int_0] Expression_0 |
    Matches_Modular_0 [Match_Modular_0] (Maybe (Location, Expression_0)) |
    Matches_unnamed_algebraic_0 [Match_unnamed_algebraic_0] (Maybe (Location, Expression_0))
      deriving Show
  data Method = Method Name [(Name, Kind)] [Constraint_0] Type_7 deriving Show
  data Modular = Modular Location Integer Integer deriving Show
  data Name = Name Location String deriving Show
  data Opdecl_0 = Opdecl_0 Location String Name Integer Assoc deriving Show
  data Pat = Pat Location Pat_branch deriving Show
  data Pat_branch = Application_pat String [Pat] | Blank_pat | Name_pat String deriving Show
  data Pattern_1 = Pattern_1 Location Pattern_0 deriving Show
  data Pattern_0 = Blank_pattern | Name_pattern String deriving Show
  type Parser = Parser' Token ((Location -> Location_1) -> String)
  data Tree_0 = Tree_0 [Data_0] [Class_0] [Opdecl_0] [Def_0] deriving Show
  data Tree_1 = Tree_1 [Name] Tree_0 deriving Show
  data Type_0 = Application_type_0 Type_0 [Type_0] | Name_type_0 Name | Op_type_0 Type_0 [(Name, Type_0)] deriving Show
  data Type_7 = Type_7 Location Type_0 deriving Show
  class Get_location t where
    get_location :: t -> Location
  infixl 4 <&
  (<&) :: (Location -> t) -> Parser () -> Parser t
  f <& p = f <$> parse_location <* p
  infixl 4 <&>
  (<&>) :: (Location -> t -> u) -> Parser t -> Parser u
  f <&> p = f <$> parse_location <*> p
  instance Get_location Pat where
    get_location (Pat a _) = a
  instance Get_location Pattern_1 where
    get_location (Pattern_1 a _) = a
  int_to_nat_type_0 :: Location -> Integer -> Type_0
  int_to_nat_type_0 l x =
    case x of
      0 -> Name_type_0 (Name l "Zr")
      _ -> Application_type_0 (Name_type_0 (Name l "Next")) [int_to_nat_type_0 l (x - 1)]
{-
  mk_let :: [Eqq] -> Expression_0 -> Expression_branch_0
  mk_let x y = (\(Expression_0 _ z) -> z) (Prelude.foldr mk_let' y x)
  mk_let' :: Eqq -> Expression_0 -> Expression_0
  mk_let' (Eqq (Name l x) y z) w =
    Expression_0
      l
      (Application_expression_0
        (Expression_0 l (Function_expression_0 (Pat l (Name_pat x)) w))
        (Prelude.foldr (\(Pat m a) -> \b -> Expression_0 m (Function_expression_0 (Pat m a) b)) z y))
-}
  parse :: Show t => Parser t -> (Location -> Location_1) -> String -> Either String t
  parse a b c = first (\ f -> f b) (fromJust (parse' classify_char next_location tokenise a (flip parse_error) c))
  parse_ap_expr :: Parser Expression_0
  parse_ap_expr = Application_expression_0 <$> parse_br_expr <*> parse_some parse_br_expr
  parse_ap_type :: Parser Type_0
  parse_ap_type = Application_type_0 <$> parse_br_type <*> parse_some parse_br_type
  parse_application_pat :: Parser Pat
  parse_application_pat =
    (
      (\a -> \x -> \y -> \z -> Pat a (Application_pat x (y : z))) <&>
      parse_name <*>
      parse_brack_pat <*>
      parse_many parse_brack_pat)
  parse_argument :: Parser t -> Parser u -> Parser (t, u)
  parse_argument p = (<*>) ((<*) ((,) <$> p) parse_colon)
  parse_arguments :: (Parser [(t, u)] -> Parser [(t, u)]) -> Parser t -> Parser u -> Parser [(t, u)]
  parse_arguments a b c = parse_optional a (parse_argument b c)
  parse_arguments' :: Parser t -> Parser [(t, Type_7)]
  parse_arguments' a = parse_arguments parse_round a parse_type
{-
  parse_arguments'' :: Parser t -> Parser [(t, Kind)]
  parse_arguments'' a = parse_arguments parse_round a parse_kind
-}
  parse_arrow :: Parser ()
  parse_arrow = parse_token Arrow_token
  parse_arrow' :: Parser (Expression_0 -> t) -> Parser t
  parse_arrow' p = p <* parse_arrow <*> parse_expression'
  parse_arrow_kind :: Parser Kind
  parse_arrow_kind =
    do
      x <- parse_bracketed_kind
      parse_arrow
      y <- parse_kind
      return (Function_kind x y)
  parse_basic :: Parser Def_0
  parse_basic =
    (
      Basic_def_0 <$>
      parse_name'' Def_token <*>
      parse_kinds <*>
      parse_constraints <*>
      parse_arguments' parse_pat <*
      parse_colon <*>
      parse_type <*
      parse_eq <*>
      parse_expression')
  parse_blank :: Parser Pattern_0
  parse_blank = Blank_pattern <$ parse_token Blank_token
  parse_blank_pat :: Parser Pat
  parse_blank_pat = (\x -> Pat x Blank_pat) <& parse_token Blank_token
  parse_br_expr :: Parser Expression_0
  parse_br_expr = parse_round (parse_comp_expr <+> parse_mid_expr) <+> parse_elementary_expression
  parse_br_expr' :: Parser Expression_0
  parse_br_expr' = parse_round parse_comp_expr <+> parse_mid_expr <+> parse_elementary_expression
  parse_brack_pat :: Parser Pat
  parse_brack_pat = parse_round parse_application_pat <+> parse_elementary_pat
  parse_bracketed_kind :: Parser Kind
  parse_bracketed_kind = parse_round parse_arrow_kind <+> parse_name_kind
  parse_br_type :: Parser Type_0
  parse_br_type = parse_round (parse_op_type <+> parse_ap_type) <+> parse_elementary_type
  parse_br_type' :: Parser Type_0
  parse_br_type' = parse_round parse_op_type <+> parse_ap_type <+> parse_elementary_type
  parse_brnch :: Parser Brnch_0
  parse_brnch =
    Brnch_0 <$> parse_name' <*> parse_many parse_name' <* parse_arrow <*> parse_name' <*> parse_arguments' parse_name'
  parse_brnchs :: Parser Data_0
  parse_brnchs =
    (
      Data_0 <$>
      parse_name'' Branching_token <*>
      (
        Branching_data_0 <$>
        parse_kinds <*
        parse_token Left_round_bracket_token <*>
        parse_non_empty_list Comma_token parse_brnch <*
        parse_token Right_round_bracket_token))
  parse_class :: Parser Class_0
  parse_class =
    (
      Class_0 <$>
      parse_name'' Class_token <*
      parse_token Left_curly_bracket_token <*>
      ((,) <$> parse_pattern' <* parse_colon <*> parse_kind) <*
      parse_token Right_curly_bracket_token <*>
      (Just <$ parse_operator "<" <*> parse_name' <* parse_operator ">" <+> pure Nothing) <*>
      parse_optional parse_round parse_method)
  parse_colon :: Parser ()
  parse_colon = parse_operator ":"
  parse_comma :: Parser ()
  parse_comma = parse_token Comma_token
  parse_comp_expr :: Parser Expression_0
  parse_comp_expr = parse_let_expression <+> parse_match_expression <+> parse_function <+> parse_op_expr
  parse_constraint :: Parser Constraint_0
  parse_constraint = Constraint_0 <$> parse_name' <*> parse_name'
  parse_constraints :: Parser [Constraint_0]
  parse_constraints =
    parse_optional' (parse_operator "<" *> parse_non_empty_list Comma_token parse_constraint <* parse_operator ">")
  parse_data :: Parser Data_0
  parse_data = parse_brnchs <+> parse_named_struct <+> parse_unnamed_algebraic
  parse_data' :: (t -> Data_branch_0) -> Token -> Parser t -> Parser Data_0
  parse_data' f a b = (\x -> \y -> \z -> Data_0 x (Plain_data_0 y (f z))) <$> parse_name'' a <*> parse_kinds <*> b
  parse_def :: Parser Def_0
  parse_def = parse_basic <+> parse_instance
  parse_default_pat :: Parser Expression_0
  parse_default_pat = parse_comma *> parse_token Default_token *> parse_arrow *> parse_expression'
  parse_default_pat' :: Parser (Maybe (Location, Expression_0))
  parse_default_pat' =
    Just <$ parse_comma <*> ((,) <& parse_token Default_token <* parse_arrow <*> parse_expression') <+> pure Nothing
  parse_elementary_expression :: Parser Expression_0
  parse_elementary_expression =
    (
      parse_int_expression <+>
      parse_name_expression)
  parse_elementary_pat :: Parser Pat
  parse_elementary_pat = parse_blank_pat <+> parse_name_pat
  parse_elementary_type :: Parser Type_0
  parse_elementary_type = parse_name_type <+> (int_to_nat_type_0 <&> parse_int)
  parse_eq :: Parser ()
  parse_eq = parse_operator "="
  parse_eq' :: Parser Eqq
  parse_eq' = Eqq <$> parse_name' <*> parse_many parse_brack_pat <* parse_eq <*> parse_expression'
  parse_error :: (Location -> Location_1) -> Location -> String
  parse_error a b = "Parse error" ++ location' (a b)
  parse_expression :: String -> Err Expression_0
  parse_expression = parse parse_expression' (Location_1 "input")
  parse_expression' :: Parser Expression_0
  parse_expression' = parse_comp_expr <+> parse_mid_expr <+> parse_elementary_expression
  parse_function :: Parser Expression_0
  parse_function = parse_arrow' (Function_expression_0 <$> parse_pat)
  parse_instance :: Parser Def_0
  parse_instance =
    (
      Instance_def_0 <&
      parse_token Instance_token <*>
      parse_name' <*
      parse_token Left_curly_bracket_token <*>
      parse_name' <*>
      parse_optional'
        (parse_brackets Left_square_bracket_token Right_square_bracket_token (parse_non_empty_list Comma_token parse_kind)) <*>
      parse_many parse_pattern_1 <*
      parse_token Right_curly_bracket_token <*>
      parse_constraints <*>
      parse_optional
        parse_round
        ((\x -> \y -> \z -> (x, (y, z))) <$> parse_name' <*> parse_many parse_brack_pat <* parse_eq <*> parse_expression'))
  parse_int :: Parser Integer
  parse_int =
    parse_token'
      (\a ->
        case a of
          Int_token b -> Just b
          _ -> Nothing)
  parse_int_expression :: Parser Expression_0
  parse_int_expression = Int_expression_0 <$> parse_int
  parse_let_expression :: Parser Expression_0
  parse_let_expression =
    (
      flip (foldr Let_expression_0) <$
      parse_token Let_token <*>
      parse_non_empty_list Comma_token parse_eq' <*
      parse_token In_token <*>
      parse_expression')
  parse_kind :: Parser Kind
  parse_kind = parse_arrow_kind <+> parse_name_kind
  parse_kinds :: Parser [(Name, Kind)]
  parse_kinds = parse_arguments (parse_brackets Left_square_bracket_token Right_square_bracket_token) parse_name' parse_kind
  parse_load :: Parser Name
  parse_load =
    do
      name <- parse_name_3 Load_token (flip (++) ".awf" <$> parse_name)
      parse_operator "."
      parse_name_4 "awf"
      return name
  parse_match_expression :: Parser Expression_0
  parse_match_expression =
    (
      Match_expression_0 <&
      parse_token Match_token <*>
      parse_expression' <*
      parse_token Left_curly_bracket_token <*>
      parse_matches <*
      parse_token Right_curly_bracket_token)
  parse_match_int :: Parser Match_Int_0
  parse_match_int = parse_arrow' (Match_Int_0 <&> parse_int)
  parse_match_modular :: Parser Match_Modular_0
  parse_match_modular = parse_arrow' (Match_Modular_0 <&> parse_modular)
  parse_match_unnamed_algebraic :: Parser Match_unnamed_algebraic_0
  parse_match_unnamed_algebraic = parse_arrow' (Match_unnamed_algebraic_0 <$> parse_name' <*> parse_many parse_brack_pat)
  parse_matches :: Parser Matches_0
  parse_matches = parse_matches_int <+> parse_matches_modular <+> parse_matches_unnamed_algebraic
  parse_matches_int :: Parser Matches_0
  parse_matches_int = Matches_Int_0 <$> parse_non_empty_list Comma_token parse_match_int <*> parse_default_pat
  parse_matches_modular :: Parser Matches_0
  parse_matches_modular = Matches_Modular_0 <$> parse_non_empty_list Comma_token parse_match_modular <*> parse_default_pat'
  parse_matches_unnamed_algebraic :: Parser Matches_0
  parse_matches_unnamed_algebraic =
    Matches_unnamed_algebraic_0 <$> parse_non_empty_list Comma_token parse_match_unnamed_algebraic <*> parse_default_pat'
  parse_method :: Parser Method
  parse_method = Method <$> parse_name' <*> parse_kinds <*> parse_constraints <* parse_colon <*> parse_type
  parse_mid_expr :: Parser Expression_0
  parse_mid_expr = parse_ap_expr <+> Modular_expression_0 <$> parse_modular
  parse_modular :: Parser Modular
  parse_modular = (\x -> flip (Modular x)) <&> parse_int <* parse_token (Operator_token "#") <*> parse_int
  parse_name :: Parser String
  parse_name =
    parse_token'
      (\a ->
        case a of
          Name_token b -> Just b
          _ -> Nothing)
  parse_name' :: Parser Name
  parse_name' = Name <&> parse_name
  parse_name'' :: Token -> Parser Name
  parse_name'' = flip parse_name_3 parse_name
  parse_name_3 :: Token -> Parser String -> Parser Name
  parse_name_3 a b = Name <& parse_token a <*> b
  parse_name_4 :: String -> Parser ()
  parse_name_4 = parse_token <$> Name_token
  parse_name_expression :: Parser Expression_0
  parse_name_expression =
    (
      Name_expression_0 <$>
      parse_name' <*>
      parse_optional' (Just <$> parse_brackets Left_curly_bracket_token Right_curly_bracket_token parse_type) <*>
      parse_optional'
        (parse_brackets Left_square_bracket_token Right_square_bracket_token (parse_non_empty_list Comma_token parse_type)))
  parse_name_kind :: Parser Kind
  parse_name_kind = parse_nat_kind <+> parse_type_kind
  parse_name_pat :: Parser Pat
  parse_name_pat = (\x -> \y -> Pat x (Name_pat y)) <&> parse_name
  parse_name_pattern :: Parser Pattern_0
  parse_name_pattern = Name_pattern <$> parse_name
  parse_name_type :: Parser Type_0
  parse_name_type = Name_type_0 <$> (Name <&> parse_name)
  parse_named_struct :: Parser Data_0
  parse_named_struct = parse_data' Named_struct_data_0 Named_struct_token (parse_arguments' parse_name')
  parse_nat_kind :: Parser Kind
  parse_nat_kind =
    do
      parse_name_4 "Nat"
      return Nat_kind
{-
  parse_nothing :: Parser ()
  parse_nothing = Parser (\a -> Right ((), a))
-}
{-
  parse_op :: Parser Name
  parse_op = Name <&> parse_op''
-}
  parse_op' :: Parser Name
  parse_op' =
    (
      Name <&>
      parse_token'
        (\a ->
          case a of
            Arrow_token -> Just "->"
            Operator_token b ->
              case b of
                "=" -> Nothing
                _ -> Just b
            _ -> Nothing))
  parse_op'' :: Parser String
  parse_op'' =
    parse_token'
      (\a ->
        case a of
          Operator_token b -> Just b
          _ -> Nothing)
  parse_op_0 :: Parser Name
  parse_op_0 = Name <&> filter_parser ((/=) "#") (flip parse_error) parse_op''
  parse_op_expr :: Parser Expression_0
  parse_op_expr = Op_expression_0 <$> parse_br_expr' <*> parse_some ((,) <$> parse_op_0 <*> parse_br_expr')
  parse_op_type :: Parser Type_0
  parse_op_type = Op_type_0 <$> parse_br_type' <*> parse_some ((,) <$> parse_op' <*> parse_br_type')
  parse_opdecl :: Parser Opdecl_0
  parse_opdecl =
    (
      Opdecl_0 <&
      parse_token Opdecl_token <*>
      parse_op'' <*>
      parse_name' <*>
      parse_int <*>
      (Lft <$ parse_name_4 "Left" <+> Rght <$ parse_name_4 "Right"))
  parse_operator :: String -> Parser ()
  parse_operator x = parse_token (Operator_token x)
  parse_optional :: (Parser [t] -> Parser [t]) -> Parser t -> Parser [t]
  parse_optional a b = parse_optional' (a (parse_non_empty_list Comma_token b))
  parse_optional' :: Alternative f => Parser (f t) -> Parser (f t)
  parse_optional' a = a <+> pure empty
  parse_pat :: Parser Pat
  parse_pat = parse_application_pat <+> parse_elementary_pat
  parse_pattern_0 :: Parser Pattern_0
  parse_pattern_0 = parse_blank <+> parse_name_pattern
  parse_pattern_1 :: Parser Pattern_1
  parse_pattern_1 = Pattern_1 <&> parse_pattern_0
  parse_pattern' :: Parser Name
  parse_pattern' = Name <&> ("_" <$ parse_token Blank_token <+> parse_name)
  parse_round :: Parser t -> Parser t
  parse_round = parse_brackets Left_round_bracket_token Right_round_bracket_token
  parse_tree :: (Location -> Location_1) -> String -> Err Tree_1
  parse_tree = parse parse_tree'
  parse_tree' :: Parser Tree_1
  parse_tree' =
    (
      Tree_1 <$>
      parse_many parse_load <*>
      (Tree_0 <$> parse_many parse_data <*> parse_many parse_class <*> parse_many parse_opdecl <*> parse_many parse_def))
  parse_type :: Parser Type_7
  parse_type = Type_7 <&> (parse_op_type <+> parse_ap_type <+> parse_elementary_type)
  parse_type_kind :: Parser Kind
  parse_type_kind =
    do
      parse_name_4 "Type"
      return Type_kind
  parse_unnamed_algebraic :: Parser Data_0
  parse_unnamed_algebraic =
    parse_data'
      Unnamed_algebraic_data_0
      Unnamed_algebraic_token
      (parse_round (parse_non_empty_list Comma_token parse_unnamed_form))
  parse_unnamed_form :: Parser Unnamed_form_0
  parse_unnamed_form = Unnamed_form_0 <$> parse_name' <*> parse_many (Type_7 <&> parse_br_type)