--------------------------------------------------------------------------------------------------------------------------------
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
    Name_alg_pat Name |
    Op_alg_pat Alg_pat [(Name, Alg_pat)]
      deriving Show
  data Assoc = Lft | Rght deriving (Eq, Show)
  data Brnch_0 = Brnch_0 Name [Name] Name [(Name, Type_7)] deriving Show
  data Case_0 = Case_0 Alg_pat Expression_0 deriving Show
  data Class_0 = Class_0 Name (Name, Kind_0) (Maybe Name) [Method] deriving Show
  data Constraint_0 = Constraint_0 Name Name deriving Show
  data Data_0 =
    Algebraic_data_0 Location_0 Status String [(Name, Kind_0)] [Form_0] |
    Branching_data_0 Location_0 Status String [(Name, Kind_0)] Data_br_0 Name Data_br_0 |
    Struct_data_0 Location_0 Status String [(Name, Kind_0)] [(Name, Type_7)]
      deriving Show
  data Data_br_0 = Data_br_0 Name [(Name, Type_7)] deriving Show
  data Def_0 =
    Basic_def_0 Name Kinds_constraints [(Pat, Type_7)] Type_7 Expression_0 |
    Instance_def_0 Location_0 Name Name [Pattern_1] [Constraint_0] [(Name, ([Pat], Expression_0))]
      deriving Show
  data Expression_0 =
    Application_expression_0 Expression_0 Expression_0 |
    Branch_expression_0 Name Expression_0 Pattern_1 Expression_0 |
    Char_expression_0 Char |
    Function_expression_0 Pat Expression_0 |
    Int_expression_0 Integer |
    Let_expression_0 Name [Pat] Expression_0 Expression_0 |
    List_expression_0 [Expression_0] |
    Match_expression_0 Location_0 Expression_0 [Case_0] |
    Modular_expression_0 Modular |
    Name_expression_0 Name (Maybe Type_7) [Type_7] |
    Op_expression_0 Expression_0 [(Name, Expression_0)] |
    Syntax_expression_0 Name
      deriving Show
  data Form_0 = Form_0 Name [Type_7] deriving Show
  data Input = Check [Name] | Eval [Name] Expression_0 deriving Show
  data Kind_0 = Arrow_kind_0 Kind_0 Kind_0 | Nat_kind_0 | Star_kind_0 deriving (Eq, Show)
  data Kinds_constraints = Kinds_constraints [(Name, Kind_0)] [Constraint_0] deriving Show
  data Method = Method Name Kinds_constraints Type_7 deriving Show
  data Modular = Modular Location_0 Integer Integer deriving Show
  data Name = Name Location_0 String deriving Show
  data Op = Op Integer Assoc String deriving Show
  data Opdecl_0 = Opdecl_0 Location_0 String Name Integer Assoc deriving Show
  data Pat = Application_pat Name [Pat] | Blank_pat | Name_pat Name | Op_pat Pat [(Name, Pat)] deriving Show
  data Pattern_1 = Pattern_1 Location_0 Pattern_0 deriving Show
  data Pattern_0 = Blank_pattern | Name_pattern String deriving Show
  newtype Parser s f t = Parser {parser :: s -> f (t, s)}
  type Parser' = Parser State (Either Location_0)
  -- data Stat = Hidden | Restricted | Standard deriving Show
  data State = State Tokens Location_0 deriving Show
  data Status = New | Old deriving (Eq, Show)
  data Syntax = Syntax Location_0 String [(Name, Syntax_type)] Syntax_type Syntax_expr deriving Show
  data Syntax_expr =
    Application_syntax Syntax_expr Syntax_expr |
    Case_syntax Location_0 Name Syntax_expr (Name, Name) Syntax_expr |
    Char_syntax Char |
    Int_syntax Integer |
    Lst_syntax [Syntax_expr] |
    Modular_syntax Modular |
    Name_syntax String (Maybe Type_0) [Type_0] |
    Op_syntax Syntax_expr [(String, Syntax_expr)] |
    Syntax_syntax Name
      deriving Show
  data Syntax_type = Arrow_syntax Syntax_type Syntax_type | Expr_syntax | List_syntax Syntax_type deriving (Eq, Show)
  data Tree_0 = Tree_0 [Syntax] [Data_0] [Class_0] [Opdecl_0] [Def_0] deriving Show
  data Tree_1 = Tree_1 [Name] Tree_0 deriving Show
  data Type_0 = Application_type_0 Type_0 [Type_0] | Name_type_0 Name | Nat_type_0 Integer | Op_type_0 Type_0 [(Name, Type_0)]
    deriving Show
  data Type_7 = Type_7 Location_0 Type_0 deriving Show
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
  instance Monad f => Monad (Parser s f) where
    Parser p >>= f = Parser (p >=> \(y, z) -> parser (f y) z)
  empty_parser :: Parser' t
  empty_parser = Parser (\(State _ l) -> Left l)
  filter_parser :: (t -> Bool) -> Parser' t -> Parser' t
  filter_parser f p = p >>= \x -> if f x then return x else empty_parser
  init_location :: Location_0
  init_location = Location_0 0 0
  left_bind :: (t -> Either u v) -> Either t v -> Either u v
  left_bind a b =
    case b of
      Left c -> a c
      Right c -> Right c
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
  parse_alg_pattern = parse_op_alg_pattern <+> parse_comp_alg_pattern <+> parse_elementary_alg_pattern
  parse_algebraic :: Parser' Data_0
  parse_algebraic =
    (
      Algebraic_data_0 <&>
      parse_struct_status <*
      parse_token Algebraic_token <*>
      parse_name <*>
      parse_kinds <*
      parse_token Left_curly_token <*>
      parse_list 2 parse_form <*
      parse_token Right_curly_token)
  parse_ap_expr :: Parser' Expression_0
  parse_ap_expr = parse_appl Application_expression_0 parse_br_expr
  parse_ap_pat :: Parser' Pat
  parse_ap_pat = Application_pat <$> parse_name' <*> parse_some parse_br_pat
  parse_ap_type :: Parser' Type_0
  parse_ap_type = Application_type_0 <$> parse_br_type <*> parse_some parse_br_type
  parse_appl :: (t -> t -> t) -> Parser' t -> Parser' t
  parse_appl f p = Prelude.foldl f <$> p <*> parse_some p
  parse_application_alg_pattern :: Parser' Alg_pat
  parse_application_alg_pattern =
    (
      (\t -> \x -> \y -> \z -> Application_alg_pat t x (y : z)) <&>
      parse_name <*>
      parse_br_alg_pattern <*>
      parse_many parse_br_alg_pattern)
  parse_argument :: Parser' t -> Parser' u -> Parser' (t, u)
  parse_argument p q = (,) <$> p <* parse_colon <*> q
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
      parse_kinds' <*>
      parse_optional parse_round (first (uncurry Op_pat) <$> parse_pat_and_type) <*
      parse_colon <*>
      parse_type <*
      parse_eq <*>
      parse_expression')
  parse_blank :: Parser' Pattern_0
  parse_blank = Blank_pattern <$ parse_token Blank_token
  parse_blank_alg_pattern :: Parser' Alg_pat
  parse_blank_alg_pattern = Blank_alg_pat <$ parse_token Blank_token
  parse_blank_pat :: Parser' Pat
  parse_blank_pat = Blank_pat <$ parse_token Blank_token
  parse_br_alg_pattern :: Parser' Alg_pat
  parse_br_alg_pattern = parse_round (parse_op_alg_pattern <+> parse_comp_alg_pattern) <+> parse_elementary_alg_pattern
  parse_br_alg_pattern' :: Parser' Alg_pat
  parse_br_alg_pattern' = parse_comp_alg_pattern <+> parse_round parse_op_alg_pattern <+> parse_elementary_alg_pattern
  parse_br_expr :: Parser' Expression_0
  parse_br_expr = parse_round (parse_comp_expr <+> parse_mid_expr) <+> parse_elementary_expression
  parse_br_expr' :: Parser' Expression_0
  parse_br_expr' = parse_mid_expr <+> parse_round parse_comp_expr <+> parse_elementary_expression
  parse_br_expression :: Parser' Expression_0
  parse_br_expression = parse_arrow *> parse_expression'
  parse_br_pat :: Parser' Pat
  parse_br_pat = parse_round (parse_op_pat <+> parse_ap_pat) <+> parse_elementary_pat
  parse_br_pat' :: Parser' Pat
  parse_br_pat' = parse_ap_pat <+> parse_round parse_op_pat <+> parse_elementary_pat
  parse_br_type :: Parser' Type_0
  parse_br_type = parse_round (parse_op_type <+> parse_ap_type) <+> parse_elementary_type
  parse_br_type' :: Parser' Type_0
  parse_br_type' = parse_ap_type <+> parse_round parse_op_type <+> parse_elementary_type
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
      parse_pattern_1 <*>
      parse_br_expression <*
      parse_token Right_curly_token)
  parse_brnchs :: Parser' Data_0
  parse_brnchs =
    (
      Branching_data_0 <&>
      parse_struct_status <*
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
    parse_let_expression <+> parse_branch_expression <+> parse_match_expression <+> parse_function <+> parse_op_expr
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
      parse_list_expression <+>
      parse_name_expression <+>
      parse_syntax_expression)
  parse_elementary_pat :: Parser' Pat
  parse_elementary_pat = parse_blank_pat <+> parse_name_pat
  parse_elementary_type :: Parser' Type_0
  parse_elementary_type = Name_type_0 <$> parse_name' <+> Nat_type_0 <$> parse_int'
  parse_eq :: Parser' ()
  parse_eq = parse_operator "="
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
        ((\x -> \y -> \z -> (x, (y, z))) <$> parse_name' <*> parse_many parse_br_pat <* parse_eq <*> parse_expression'))
  parse_int :: Parser' Integer
  parse_int = parse_int' <+> negate <$ parse_token Negate_token <*> filter_parser ((/=) 0) parse_int'
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
      flip (foldr (\(a, b, c) -> Let_expression_0 a b c)) <$
      parse_token Let_token <*>
      parse_list 1 ((,,) <$> parse_name' <*> parse_many parse_br_pat <* parse_eq <*> parse_expression') <*
      parse_token In_token <*>
      parse_expression')
  parse_list_expression :: Parser' Expression_0
  parse_list_expression = List_expression_0 <$ parse_operator "!" <*> parse_square (parse_list 1 parse_expression')
  parse_kind :: Parser' Kind_0
  parse_kind = parse_arrow_kind <+> parse_name_kind
  parse_kinds :: Parser' [(Name, Kind_0)]
  parse_kinds = parse_arguments (\a -> parse_brackets Left_square_token a Right_square_token) parse_name' parse_kind
  parse_kinds' :: Parser' Kinds_constraints
  parse_kinds' = Kinds_constraints <$> parse_kinds <*> parse_constraints
  parse_list :: Integer -> Parser' t -> Parser' [t]
  parse_list i p =
    case i of
      1 -> (:) <$> p <*> parse_many (parse_comma *> p)
      _ -> (:) <$> p <* parse_comma <*> parse_list (i - 1) p
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
  parse_method = Method <$> parse_name' <*> parse_kinds' <* parse_colon <*> parse_type
  parse_mid_expr :: Parser' Expression_0
  parse_mid_expr = parse_ap_expr <+> Modular_expression_0 <$> parse_modular
  parse_modular :: Parser' Modular
  parse_modular = (\x -> flip (Modular x)) <&> parse_int' <* parse_operator "#" <*> parse_int'
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
  parse_name_pat = Name_pat <$> parse_name'
  parse_name_pattern :: Parser' Pattern_0
  parse_name_pattern = Name_pattern <$> parse_name
  parse_op :: Parser' String
  parse_op =
    parse_elementary
      (\a ->
        case a of
          Operator_token b -> Just b
          _ -> Nothing)
  parse_op_0 :: Parser' String
  parse_op_0 = filter_parser (\x -> notElem x ["#", "->", "="]) parse_op
  parse_op_0' :: Parser' Name
  parse_op_0' = Name <&> parse_op_0
  parse_op_alg_pattern :: Parser' Alg_pat
  parse_op_alg_pattern = Op_alg_pat <$> parse_br_alg_pattern' <*> parse_some ((,) <$> parse_op_0' <*> parse_br_alg_pattern')
  parse_op_expr :: Parser' Expression_0
  parse_op_expr = Op_expression_0 <$> parse_br_expr' <*> parse_some ((,) <$> parse_op_0' <*> parse_br_expr')
  parse_op_pat :: Parser' Pat
  parse_op_pat = Op_pat <$> parse_br_pat' <*> parse_some ((,) <$> parse_op_0' <*> parse_br_pat')
  parse_op_type :: Parser' Type_0
  parse_op_type =
    (
      Op_type_0 <$>
      parse_br_type' <*>
      parse_some ((,) <$> (Name <&> filter_parser (\x -> elem x (fst <$> typeops)) parse_op) <*> parse_br_type'))
  parse_opdecl :: Parser' Opdecl_0
  parse_opdecl =
    (
      Opdecl_0 <&
      parse_token Opdecl_token <*>
      parse_op_0 <*>
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
  parse_pat = parse_op_pat <+> parse_ap_pat <+> parse_elementary_pat
  parse_pat_and_type :: Parser' ((Pat, [(Name, Pat)]), Type_7)
  parse_pat_and_type =
    (
      (\a -> first ((,) a)) <$>
      parse_br_pat' <*>
      (
        (,) [] <$ parse_colon <*> parse_type <+>
        (
          (\a -> first (\(b, c) -> (a, b) : c)) <$>
          (Name <&> parse_op_0) <*>
          parse_pat_and_type)))
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
  parse_square :: Parser' t -> Parser' t
  parse_square a = parse_brackets Left_square_token a Right_square_token
  parse_struct :: Parser' Data_0
  parse_struct =
    (
      Struct_data_0 <&>
      parse_struct_status <*
      parse_token Struct_token <*>
      parse_name <*>
      parse_kinds <*>
      parse_arguments' parse_name')
  parse_struct_status :: Parser' Status
  parse_struct_status = Old <$ parse_token Hidden_token <+> return New
  parse_syntax :: Parser' Syntax
  parse_syntax =
    (
      Syntax <&
      parse_token Syntax_token <*
      parse_token Synvar_token <*>
      (((:) '!') <$> parse_name) <*>
      parse_optional parse_round ((,) <$> parse_syntax_name' <* parse_operator "::" <*> parse_syntax_type) <*
      parse_operator "::" <*>
      parse_syntax_type <*
      parse_operator "=" <*>
      parse_syntax_expr)
  parse_syntax_ap :: Parser' Syntax_expr
  parse_syntax_ap = Prelude.foldl Application_syntax <$> parse_syntax_br <*> parse_some parse_syntax_br
  parse_syntax_arrow :: Parser' Syntax_type
  parse_syntax_arrow = Arrow_syntax <$> parse_syntax_type_1 <* parse_operator "->" <*> parse_syntax_type
  parse_syntax_br :: Parser' Syntax_expr
  parse_syntax_br = parse_round (parse_syntax_comp <+> parse_syntax_mid) <+> parse_syntax_elem
  parse_syntax_br' :: Parser' Syntax_expr
  parse_syntax_br' = parse_syntax_mid <+> parse_round parse_syntax_comp <+> parse_syntax_elem
  parse_syntax_case :: Parser' Syntax_expr
  parse_syntax_case =
    (
      Case_syntax <&
      parse_token Case_token <*>
      parse_syntax_name' <*
      parse_token Of_token <*
      parse_token Left_curly_token <*
      parse_square (return ()) <*
      parse_operator "->" <*>
      parse_syntax_expr <*
      parse_comma <*>
      ((,) <$> parse_syntax_name' <* parse_operator ":" <*> parse_syntax_name') <*
      parse_operator "->" <*>
      parse_syntax_expr <*
      parse_token Right_curly_token)
  parse_syntax_comp :: Parser' Syntax_expr
  parse_syntax_comp = parse_syntax_case <+> parse_syntax_op
  parse_syntax_elem :: Parser' Syntax_expr
  parse_syntax_elem = Char_syntax <$> parse_char <+> parse_syntax_int <+> parse_syntax_list <+> parse_syntax_name
  parse_syntax_expr :: Parser' Syntax_expr
  parse_syntax_expr = parse_syntax_comp <+> parse_syntax_mid <+> parse_syntax_elem
  parse_syntax_expression :: Parser' Expression_0
  parse_syntax_expression = Syntax_expression_0 <$> parse_syntax_name'
  parse_syntax_int :: Parser' Syntax_expr
  parse_syntax_int = Int_syntax <$> parse_int
  parse_syntax_list :: Parser' Syntax_expr
  parse_syntax_list = Lst_syntax <$ parse_operator "!" <*> parse_square (parse_list 1 parse_syntax_expr)
  parse_syntax_mid :: Parser' Syntax_expr
  parse_syntax_mid = parse_syntax_ap <+> Modular_syntax <$> parse_modular
  parse_syntax_name :: Parser' Syntax_expr
  parse_syntax_name = 
    (
      (
        Name_syntax <$>
        parse_name <*>
        parse_optional' (Just <$> parse_brackets Left_curly_token parse_type' Right_curly_token) <*>
        parse_optional' (parse_brackets Left_square_token (parse_list 1 parse_type') Right_square_token)) <+>
      Syntax_syntax <$> parse_syntax_name')
  parse_syntax_name' :: Parser' Name
  parse_syntax_name' = (\x -> \y -> Name x ('!' : y)) <& parse_token Synvar_token <*> parse_name
  parse_syntax_op :: Parser' Syntax_expr
  parse_syntax_op = Op_syntax <$> parse_syntax_br' <*> parse_some ((,) <$> parse_op_0 <*> parse_syntax_br')
  parse_syntax_type :: Parser' Syntax_type
  parse_syntax_type = parse_syntax_arrow <+> parse_syntax_type_0
  parse_syntax_type_0 :: Parser' Syntax_type
  parse_syntax_type_0 = Expr_syntax <$ parse_token Expr_token <+> List_syntax <$> parse_square parse_syntax_type
  parse_syntax_type_1 :: Parser' Syntax_type
  parse_syntax_type_1 = parse_round parse_syntax_arrow <+> parse_syntax_type_0
  parse_token :: Token_0 -> Parser' ()
  parse_token a = parse_elementary (\b -> if b == a then Just () else Nothing)
  parse_tree :: (Location_0 -> Location_1) -> String -> Err Tree_1
  parse_tree = parse parse_tree'
  parse_tree' :: Parser' Tree_1
  parse_tree' =
    (
      Tree_1 <$>
      parse_optional' (parse_token Load_token *> parse_files) <*>
      (
        Tree_0 <$>
        parse_many parse_syntax <*>
        parse_many parse_data <*>
        parse_many parse_class <*>
        parse_many parse_opdecl <*>
        parse_many parse_def))
  parse_type :: Parser' Type_7
  parse_type = Type_7 <&> parse_type'
  parse_type' :: Parser' Type_0
  parse_type' = parse_op_type <+> parse_ap_type <+> parse_elementary_type
  state_location :: State -> Location_0
  state_location (State (Tokens a b) _) =
    case a of
      [] -> b
      Token_1 c _ : _ -> c
  typeops :: [(String, Op)]
  typeops = [("*", Op 0 Rght "Pair"), ("+", Op 1 Rght "Either"), ("->", Op 2 Rght "Function")]
  update_location :: State -> Location_0 -> State
  update_location (State a b) c = State a (max b c)
--------------------------------------------------------------------------------------------------------------------------------