-----------------------------------------------------------------------------------------------------------------------------
{-
expr op op expr - esimene op on binaarne operaator, teine op on unaarne miinus
expr op expr - op on binaarne operaator
op expr - op on unaarne miinus
-}
{-# OPTIONS_GHC -Wall #-}
module Tree where
  import Control.Applicative
  import Control.Monad
  import Data.Bifunctor
  import Tokenise
  data Alg_pat =
    Application_alg_pat Location_0 String [Alg_pat] |
    Blank_alg_pat |
    Char_alg_pat Char |
    Int_alg_pat Integer |
    Modular_alg_pat Modular |
    Name_alg_pat Name
      deriving Show
  data Assoc = Lft | Rght deriving (Eq, Show)
  data Brnch_0 = Brnch_0 Name [Name] Name [(Name, Type_7)] deriving Show
  data Case_0 = Case_0 Alg_pat Expression_0 deriving Show
  data Class_0 = Class_0 Name (Name, Kind_0) (Maybe Name) [Method] deriving Show
  data Constraint_0 = Constraint_0 Name Name deriving Show
  data Data_0 =
    Algebraic_data_0 Location_0 String [(Name, Kind_0)] [Form_0] |
    Branching_data_0 Location_0 String [(Name, Kind_0)] Data_br_0 Name Data_br_0 |
    Struct_data_0 Location_0 Stat String [(Name, Kind_0)] [(Name, Type_7)] (Maybe (Location_0, Expression_0))
      deriving Show
  data Data_br_0 = Data_br_0 Name [(Name, Type_7)] deriving Show
  data Def_0 =
    Basic_def_0 Name [(Name, Kind_0)] [Constraint_0] [(Pat, Type_7)] Type_7 Expression_0 |
    Instance_def_0 Location_0 Name Name [Pattern_1] [Constraint_0] [(Name, ([Pat], Expression_0))]
      deriving Show
  data Eqq = Eqq Name [Pat] Expression_0 deriving Show
  data Expression_0 =
    Application_expression_0 Expression_0 [Expression_0] |
    Branch_expression_0 Name Expression_0 Name Expression_0 |
    Char_expression_0 Char |
    Function_expression_0 Pat Expression_0 |
    Int_expression_0 Integer |
    Let_expression_0 Eqq Expression_0 |
    Match_expression_0 Location_0 Expression_0 [Case_0] |
    Modular_expression_0 Modular |
    Name_expression_0 Name (Maybe Type_7) [Type_7] |
    Op_expression_0 Expression_0 [(Name, Expression_0)]
      deriving Show
  data Form_0 = Form_0 Name [Type_7] deriving Show
  data Input = Check [Name] | Eval [Name] Expression_0 deriving Show
  data Kind_0 = Arrow_kind_0 Kind_0 Kind_0 | Nat_kind_0 | Star_kind_0 deriving (Eq, Show)
  data Method = Method Name [(Name, Kind_0)] [Constraint_0] Type_7 deriving Show
  data Modular = Modular Location_0 Integer Integer deriving Show
  data Name = Name Location_0 String deriving Show
  data Opdecl_0 = Opdecl_0 Location_0 String Name Integer Assoc deriving Show
  data Pat = Pat Location_0 Pat_branch deriving Show
  data Pat_branch = Application_pat String [Pat] | Blank_pat | Name_pat String deriving Show
  data Pattern_1 = Pattern_1 Location_0 Pattern_0 deriving Show
  data Pattern_0 = Blank_pattern | Name_pattern String deriving Show
  newtype Parser s f t = Parser {parser :: s -> f (t, s)}
  type Parser' = Parser State (Either Location_0)
  data State = State Tokens Location_0 deriving Show
  data Stat = Standard | Restricted deriving Show
  data Tree_0 = Tree_0 [Data_0] [Class_0] [Opdecl_0] [Def_0] deriving Show
  data Tree_1 = Tree_1 [Name] Tree_0 deriving Show
  data Type_0 = Application_type_0 Type_0 [Type_0] | Name_type_0 Name | Nat_type_0 Integer | Op_type_0 Type_0 [(Name, Type_0)]
    deriving Show
  data Type_7 = Type_7 Location_0 Type_0 deriving Show
  class Get_location t where
    get_location :: t -> Location_0
  infixl 4 <&
  (<&) :: (Location_0 -> t) -> Parser' () -> Parser' t
  f <& p = f <$> parse_location <* p
  infixl 4 <&>
  (<&>) :: (Location_0 -> t -> u) -> Parser' t -> Parser' u
  f <&> p = f <$> parse_location <*> p
  infixl 3 <+>
  (<+>) :: Parser' t -> Parser' t -> Parser' t
  Parser a <+> Parser b = Parser (\c -> left_bind (\d -> b (update_location c d)) (a c))
  instance Monad f => Applicative (Parser s f) where
    Parser a <*> Parser b = Parser (a >=> \(c, d) -> first c <$> b d)
    pure x = Parser (\y -> return (x, y))
  instance Functor f => Functor (Parser s f) where
    fmap a (Parser b) = Parser (\c -> first a <$> b c)
  instance Get_location Pat where
    get_location (Pat a _) = a
  instance Get_location Pattern_1 where
    get_location (Pattern_1 a _) = a
  instance Monad f => Monad (Parser s f) where
    Parser p >>= f = Parser (p >=> \(y, z) -> parser (f y) z)
  empty_parser :: Parser' t
  empty_parser = Parser (\(State _ l) -> Left l)
  init_location :: Location_0
  init_location = Location_0 0 0
  left_bind :: (t -> Either u v) -> Either t v -> Either u v
  left_bind a b =
    case b of
      Left c -> a c
      Right c -> Right c
  mk_list :: Location_0 -> [Expression_0] -> Expression_0
  mk_list l =
    Prelude.foldr
      (\x -> \y ->
        Application_expression_0 (Application_expression_0 (Name_expression_0 (Name l "Construct_List") Nothing []) [x]) [y])
      (Name_expression_0 (Name l "Empty_List") Nothing [])
  parse :: Parser' t -> (Location_0 -> Location_1) -> String -> Err t
  parse a b c =
    let
       d = parse_error b
    in
      (
        tokenise b c >>=
        \e ->
          case parser a (State e init_location) of
            Left f -> d f
            Right (f, State (Tokens h _) g) ->
              case h of
                [] -> Right f
                _ -> d g)
  parse_alg_pattern :: Parser' Alg_pat
  parse_alg_pattern = parse_comp_alg_pattern <+> parse_elementary_alg_pattern
  parse_algebraic :: Parser' Data_0
  parse_algebraic =
    (
      Algebraic_data_0 <&
      parse_token Algebraic_token <*>
      parse_name <*>
      parse_kinds <*
      parse_token Left_curly_token <*>
      parse_list 2 parse_form <*
      parse_token Right_curly_token)
  parse_ap_expr :: Parser' Expression_0
  parse_ap_expr = Application_expression_0 <$> parse_br_expr <*> parse_some parse_br_expr
  parse_ap_type :: Parser' Type_0
  parse_ap_type = Application_type_0 <$> parse_br_type <*> parse_some parse_br_type
  parse_application_alg_pattern :: Parser' Alg_pat
  parse_application_alg_pattern =
    (
      (\t -> \x -> \y -> \z -> Application_alg_pat t x (y : z)) <&>
      parse_name <*>
      parse_brack_alg_pattern <*>
      parse_many parse_brack_alg_pattern)
  parse_application_pat :: Parser' Pat
  parse_application_pat =
    (
      (\a -> \x -> \y -> \z -> Pat a (Application_pat x (y : z))) <&>
      parse_name <*>
      parse_brack_pat <*>
      parse_many parse_brack_pat)
  parse_argument :: Parser' t -> Parser' u -> Parser' (t, u)
  parse_argument p = (<*>) ((<*) ((,) <$> p) parse_colon)
  parse_arguments :: (Parser' [(t, u)] -> Parser' [(t, u)]) -> Parser' t -> Parser' u -> Parser' [(t, u)]
  parse_arguments a b c = parse_optional a (parse_argument b c)
  parse_arguments' :: Parser' t -> Parser' [(t, Type_7)]
  parse_arguments' a = parse_arguments parse_round a parse_type
  parse_arguments'' :: Parser' t -> Parser' [(t, Kind_0)]
  parse_arguments'' a = parse_arguments parse_round a parse_kind
  parse_arrow :: Parser' ()
  parse_arrow = parse_operator "->"
  parse_arrow' :: Parser' (Expression_0 -> t) -> Parser' t
  parse_arrow' p = p <* parse_arrow <*> parse_expression'
  parse_arrow_kind :: Parser' Kind_0
  parse_arrow_kind = Arrow_kind_0 <$> (parse_round parse_arrow_kind <+> parse_name_kind) <* parse_arrow <*> parse_kind
  parse_basic :: Parser' Def_0
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
  parse_blank :: Parser' Pattern_0
  parse_blank = Blank_pattern <$ parse_token Blank_token
  parse_blank_alg_pattern :: Parser' Alg_pat
  parse_blank_alg_pattern = Blank_alg_pat <$ parse_token Blank_token
  parse_blank_pat :: Parser' Pat
  parse_blank_pat = (\x -> Pat x Blank_pat) <& parse_token Blank_token
  parse_br_expr :: Parser' Expression_0
  parse_br_expr = parse_round (parse_comp_expr <+> parse_mid_expr) <+> parse_elementary_expression
  parse_br_expr' :: Parser' Expression_0
  parse_br_expr' = parse_round parse_comp_expr <+> parse_mid_expr <+> parse_elementary_expression
  parse_br_expression :: Parser' Expression_0
  parse_br_expression = parse_arrow *> parse_expression'
  parse_br_type :: Parser' Type_0
  parse_br_type = parse_round (parse_op_type <+> parse_ap_type) <+> parse_elementary_type
  parse_br_type' :: Parser' Type_0
  parse_br_type' = parse_round parse_op_type <+> parse_ap_type <+> parse_elementary_type
  parse_brack_alg_pattern :: Parser' Alg_pat
  parse_brack_alg_pattern = parse_round parse_comp_alg_pattern <+> parse_elementary_alg_pattern
  parse_brack_pat :: Parser' Pat
  parse_brack_pat = parse_round parse_application_pat <+> parse_elementary_pat
  parse_brackets :: Token_0 -> Parser' t -> Token_0 -> Parser' t
  parse_brackets a b c = parse_token a *> b <* parse_token c
  parse_branch_expression :: Parser' Expression_0
  parse_branch_expression =
    (
      Branch_expression_0 <$
      parse_token Branch_token <*>
      parse_name' <*
      parse_token Left_curly_token <*
      parse_name_4 "Zero" <*>
      parse_br_expression <*
      parse_comma <*
      parse_name_4 "Next" <*>
      parse_name' <*>
      parse_br_expression <*
      parse_token Right_curly_token)
  parse_brnchs :: Parser' Data_0
  parse_brnchs =
    (
      Branching_data_0 <&
      parse_token Branching_token <*>
      parse_name <*>
      parse_kinds <*
      parse_token Left_curly_token <*
      parse_name_4 "Zero" <*>
      parse_data_br <*
      parse_comma <*
      parse_name_4 "Next" <*>
      parse_name' <*>
      parse_data_br <*
      parse_token Right_curly_token)
  parse_char :: Parser' Char
  parse_char =
    parse_elementary
      (\a ->
        case a of
          Char_token b -> Just b
          _ -> Nothing)
  parse_char_alg_pattern :: Parser' Alg_pat
  parse_char_alg_pattern = Char_alg_pat <$> parse_char
  parse_char_expression :: Parser' Expression_0
  parse_char_expression = Char_expression_0 <$> parse_char
  parse_check :: Parser' Input
  parse_check = Check <$ parse_token Check_token <*> parse_files
  parse_class :: Parser' Class_0
  parse_class =
    (
      Class_0 <$>
      parse_name'' Class_token <*
      parse_token Left_curly_token <*>
      ((,) <$> parse_pattern' <* parse_colon <*> parse_kind) <*
      parse_token Right_curly_token <*>
      (Just <$ parse_operator "<" <*> parse_name' <* parse_operator ">" <+> pure Nothing) <*>
      parse_optional parse_round parse_method)
  parse_colon :: Parser' ()
  parse_colon = parse_operator ":"
  parse_comma :: Parser' ()
  parse_comma = parse_token Comma_token
  parse_comp_alg_pattern :: Parser' Alg_pat
  parse_comp_alg_pattern = parse_application_alg_pattern <+> parse_modular_alg_pattern
  parse_comp_expr :: Parser' Expression_0
  parse_comp_expr =
    (
      parse_list_expr <+>
      parse_let_expression <+>
      parse_branch_expression <+>
      parse_match_expression <+>
      parse_function <+>
      parse_op_expr)
  parse_constraint :: Parser' Constraint_0
  parse_constraint = Constraint_0 <$> parse_name' <*> parse_name'
  parse_constraints :: Parser' [Constraint_0]
  parse_constraints = parse_optional' (parse_operator "<" *> parse_list 1 parse_constraint <* parse_operator ">")
  parse_data :: Parser' Data_0
  parse_data = parse_algebraic <+> parse_brnchs <+> parse_struct
  parse_data_br :: Parser' Data_br_0
  parse_data_br = Data_br_0 <$ parse_arrow <*> parse_name' <*> parse_arguments' parse_name'
  parse_def :: Parser' Def_0
  parse_def = parse_basic <+> parse_instance
  parse_elementary :: (Token_0 -> Maybe t) -> Parser' t
  parse_elementary a =
    Parser
      (\(State (Tokens b c) d) ->
        case b of
          [] -> Left c
          Token_1 e f : g ->
            let
              h = max d e
            in
              case a f of
                Just i -> Right (i, (State (Tokens g c) h))
                Nothing -> Left h)
  parse_elementary_alg_pattern :: Parser' Alg_pat
  parse_elementary_alg_pattern =
    parse_blank_alg_pattern <+> parse_char_alg_pattern <+> parse_int_alg_pattern <+> parse_name_alg_pattern
  parse_elementary_expression :: Parser' Expression_0
  parse_elementary_expression =
    (
      parse_char_expression <+>
      parse_int_expression <+>
      (\x -> Name_expression_0 (Name x "Empty_List") Nothing []) <& parse_name_4 "List" <+>
      parse_name_expression)
  parse_elementary_pat :: Parser' Pat
  parse_elementary_pat = parse_blank_pat <+> parse_name_pat
  parse_elementary_type :: Parser' Type_0
  parse_elementary_type = Name_type_0 <$> parse_name' <+> Nat_type_0 <$> parse_int'
  parse_eq :: Parser' ()
  parse_eq = parse_operator "="
  parse_eq' :: Parser' Eqq
  parse_eq' = Eqq <$> parse_name' <*> parse_many parse_brack_pat <* parse_eq <*> parse_expression'
  parse_error :: (Location_0 -> Location_1) -> Location_0 -> Err t
  parse_error a b = Left ("Parse error" ++ location' (a b))
  parse_eval :: Parser' Input
  parse_eval = Eval <$ parse_token Eval_token <*> parse_optional parse_round parse_file <*> parse_expression'
  parse_expression' :: Parser' Expression_0
  parse_expression' = parse_comp_expr <+> parse_mid_expr <+> parse_elementary_expression
  parse_file :: Parser' Name
  parse_file = (\(Name x y) -> Name x (y ++ ".awf")) <$> parse_name' <* parse_operator "." <* parse_name_4 "awf"
  parse_files :: Parser' [Name]
  parse_files = parse_round (parse_list 1 parse_file)
  parse_form :: Parser' Form_0
  parse_form = Form_0 <$> parse_name' <*> parse_many (Type_7 <&> parse_br_type)
  parse_function :: Parser' Expression_0
  parse_function = parse_arrow' (Function_expression_0 <$> parse_pat)
  parse_input :: String -> Err Input
  parse_input = parse (parse_check <+> parse_eval) (Location_1 "input")
  parse_instance :: Parser' Def_0
  parse_instance =
    (
      Instance_def_0 <&
      parse_token Instance_token <*>
      parse_name' <*
      parse_token Left_curly_token <*>
      parse_name' <*>
      parse_many parse_pattern_1 <*
      parse_token Right_curly_token <*>
      parse_constraints <*>
      parse_optional
        parse_round
        ((\x -> \y -> \z -> (x, (y, z))) <$> parse_name' <*> parse_many parse_brack_pat <* parse_eq <*> parse_expression'))
  parse_int :: Parser' Integer
  parse_int =
    parse_int' <+> (negate <$ parse_operator "-" <*> parse_int' >>= \x -> if x == 0 then empty_parser else return x)
  parse_int' :: Parser' Integer
  parse_int' =
    parse_elementary
      (\a ->
        case a of
          Int_token b -> Just b
          _ -> Nothing)
  parse_int_alg_pattern :: Parser' Alg_pat
  parse_int_alg_pattern = Int_alg_pat <$> parse_int
  parse_int_expression :: Parser' Expression_0
  parse_int_expression = Int_expression_0 <$> parse_int
  parse_let_expression :: Parser' Expression_0
  parse_let_expression =
    (
      flip (foldr Let_expression_0) <$
      parse_token Let_token <*>
      parse_list 1 parse_eq' <*
      parse_token In_token <*>
      parse_expression')
  parse_kind :: Parser' Kind_0
  parse_kind = parse_arrow_kind <+> parse_name_kind
  parse_kinds :: Parser' [(Name, Kind_0)]
  parse_kinds = parse_arguments (\a -> parse_brackets Left_square_token a Right_square_token) parse_name' parse_kind
  parse_list :: Integer -> Parser' t -> Parser' [t]
  parse_list i p =
    case i of
      1 -> (:) <$> p <*> parse_many (parse_comma *> p)
      _ -> (:) <$> p <* parse_comma <*> parse_list (i - 1) p
  parse_list_expr :: Parser' Expression_0
  parse_list_expr = mk_list <& parse_name_4 "List" <*> parse_round (parse_list 1 parse_expression')
  parse_location :: Parser' Location_0
  parse_location = Parser (\a -> Right (state_location a, a))
  parse_many :: Parser' t -> Parser' [t]
  parse_many a = parse_some a <+> return []
  parse_match_expression :: Parser' Expression_0
  parse_match_expression =
    (
      Match_expression_0 <&
      parse_token Match_token <*>
      parse_expression' <*
      parse_token Left_curly_token <*>
      parse_list 2 (parse_arrow' (Case_0 <$> parse_alg_pattern)) <*
      parse_token Right_curly_token)
  parse_method :: Parser' Method
  parse_method = Method <$> parse_name' <*> parse_kinds <*> parse_constraints <* parse_colon <*> parse_type
  parse_mid_expr :: Parser' Expression_0
  parse_mid_expr = parse_ap_expr <+> Modular_expression_0 <$> parse_modular
  parse_modular :: Parser' Modular
  parse_modular = (\x -> flip (Modular x)) <&> parse_int' <* parse_token (Operator_token "#") <*> parse_int'
  parse_modular_alg_pattern :: Parser' Alg_pat
  parse_modular_alg_pattern = Modular_alg_pat <$> parse_modular
  parse_name :: Parser' String
  parse_name =
    parse_elementary
      (\a ->
        case a of
          Name_token b -> Just b
          _ -> Nothing)
  parse_name' :: Parser' Name
  parse_name' = Name <&> parse_name
  parse_name'' :: Token_0 -> Parser' Name
  parse_name'' = flip parse_name_3 parse_name
  parse_name_3 :: Token_0 -> Parser' String -> Parser' Name
  parse_name_3 a b = Name <& parse_token a <*> b
  parse_name_4 :: String -> Parser' ()
  parse_name_4 = parse_token <$> Name_token
  parse_name_alg_pattern :: Parser' Alg_pat
  parse_name_alg_pattern = Name_alg_pat <$> parse_name'
  parse_name_expression :: Parser' Expression_0
  parse_name_expression =
    (
      Name_expression_0 <$>
      parse_name' <*>
      parse_optional' (Just <$> parse_brackets Left_curly_token parse_type Right_curly_token) <*>
      parse_optional' (parse_brackets Left_square_token (parse_list 1 parse_type) Right_square_token))
  parse_name_kind :: Parser' Kind_0
  parse_name_kind = Nat_kind_0 <$ parse_name_4 "Nat" <+> Star_kind_0 <$ parse_name_4 "Star"
  parse_name_pat :: Parser' Pat
  parse_name_pat = (\x -> \y -> Pat x (Name_pat y)) <&> parse_name
  parse_name_pattern :: Parser' Pattern_0
  parse_name_pattern = Name_pattern <$> parse_name
  parse_op :: Parser' Name
  parse_op = Name <&> parse_op''
  parse_op' :: Parser' Name
  parse_op' =
    (
      Name <&>
      parse_elementary
        (\a ->
          case a of
            Operator_token b ->
              case b of
                "=" -> Nothing
                _ -> Just b
            _ -> Nothing))
  parse_op'' :: Parser' String
  parse_op'' =
    parse_elementary
      (\a ->
        case a of
          Operator_token b -> Just b
          _ -> Nothing)
  parse_op_0 :: Parser' Name
  parse_op_0 = Name <&> (parse_op'' >>= \x -> if elem x ["#", "->"] then empty_parser else return x)
  parse_op_expr :: Parser' Expression_0
  parse_op_expr = Op_expression_0 <$> parse_br_expr' <*> parse_some ((,) <$> parse_op_0 <*> parse_br_expr')
  parse_op_type :: Parser' Type_0
  parse_op_type = Op_type_0 <$> parse_br_type' <*> parse_some ((,) <$> parse_op' <*> parse_br_type')
  parse_opdecl :: Parser' Opdecl_0
  parse_opdecl =
    (
      Opdecl_0 <&
      parse_token Opdecl_token <*>
      parse_op'' <*>
      parse_name' <*>
      parse_int <*>
      (Lft <$ parse_name_4 "Left" <+> Rght <$ parse_name_4 "Right"))
  parse_operator :: String -> Parser' ()
  parse_operator x = parse_token (Operator_token x)
  parse_optional :: (Parser' [t] -> Parser' [t]) -> Parser' t -> Parser' [t]
  parse_optional a b = parse_optional' (a (parse_list 1 b))
  parse_optional' :: Alternative f => Parser' (f t) -> Parser' (f t)
  parse_optional' a = a <+> return empty
  parse_pat :: Parser' Pat
  parse_pat = parse_application_pat <+> parse_elementary_pat
  parse_pattern_0 :: Parser' Pattern_0
  parse_pattern_0 = parse_blank <+> parse_name_pattern
  parse_pattern_1 :: Parser' Pattern_1
  parse_pattern_1 = Pattern_1 <&> parse_pattern_0
  parse_pattern' :: Parser' Name
  parse_pattern' = Name <&> ("_" <$ parse_token Blank_token <+> parse_name)
  parse_round :: Parser' t -> Parser' t
  parse_round a = parse_brackets Left_round_token a Right_round_token
  parse_some :: Parser' t -> Parser' [t]
  parse_some a = (:) <$> a <*> parse_many a
  parse_struct :: Parser' Data_0
  parse_struct =
    (
      Struct_data_0 <&>
      parse_struct_status <*
      parse_token Struct_token <*>
      parse_name <*>
      parse_kinds <*>
      parse_arguments' parse_name' <*>
      parse_optional' ((\a -> \b -> Just (a, b)) <& parse_operator "=" <*> parse_expression'))
  parse_struct_status :: Parser' Stat
  parse_struct_status = Restricted <$ parse_token Restricted_token <+> return Standard
  parse_token :: Token_0 -> Parser' ()
  parse_token a = parse_elementary (\b -> if b == a then Just () else Nothing)
  parse_tree :: (Location_0 -> Location_1) -> String -> Err Tree_1
  parse_tree = parse parse_tree'
  parse_tree' :: Parser' Tree_1
  parse_tree' =
    (
      Tree_1 <$>
      parse_optional' (parse_token Load_token *> parse_files) <*>
      (Tree_0 <$> parse_many parse_data <*> parse_many parse_class <*> parse_many parse_opdecl <*> parse_many parse_def))
  parse_type :: Parser' Type_7
  parse_type = Type_7 <&> (parse_op_type <+> parse_ap_type <+> parse_elementary_type)
  state_location :: State -> Location_0
  state_location (State (Tokens a b) _) =
    case a of
      [] -> b
      Token_1 c _ : _ -> c
  update_location :: State -> Location_0 -> State
  update_location (State a b) c = State a (max b c)
-----------------------------------------------------------------------------------------------------------------------------