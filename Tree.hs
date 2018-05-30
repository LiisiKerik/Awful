-----------------------------------------------------------------------------------------------------------------------------
{-
expr op op expr - esimene op on binaarne operaator, teine op on unaarne miinus
expr op expr - op on binaarne operaator
op expr - op on unaarne miinus
-}
{-# OPTIONS_GHC -Wall #-}
{-# Language FlexibleInstances, OverlappingInstances, TypeSynonymInstances #-}
module Tree where
  import Control.Applicative
  import Control.Monad
  import Data.Bifunctor
  import Tokenise
  data Brnch_0 = Brnch_0 Name [Name] Name [(Name, Type_0)] deriving Show
  data Class_0 = Class_0 Name (Name, Kind_0) (Maybe Name) [Method] deriving Show
  data Constraint_0 = Constraint_0 Name Name deriving Show
  data Data_0 = Data_0 Name Data_br_0 deriving Show
  data Data_br_0 = Branching_data_0 Name [Kind_0] [(Name, Kind_0)] [Brnch_0] | Plain_data_0 [(Name, Kind_0)] Data_branch_0
    deriving Show
  data Data_branch_0 = Algebraic_data_0 [Form_0] | Struct_data_0 [(Name, Type_0)]
    deriving Show
  data Def_0 =
    Basic_def_0 Name [(Name, Kind_0)] [Constraint_0] [(Pat, Type_0)] Type_0 Expression_0 |
    Instance_def_0 Location_0 Name Name [Kind_0] [Pattern_1] [Constraint_0] [(Name, ([Pat], Expression_0))]
      deriving Show
-- TODO: siin peaks saama teha ka seda: Let Pair x y = ... In ...
  data Eqq = Eqq Name [Pat] Expression_0 deriving Show
  data Expression_0 = Expression_0 Location_0 Expression_branch_0 deriving Show
  data Expression_branch_0 =
    Application_expression_0 Expression_0 Expression_0 |
    Char_expression_0 Char |
    Function_expression_0 Pat Expression_0 |
    Int_expression_0 Integer |
    Match_expression_0 Expression_0 Matches_0 |
    Modular_expression_0 Modular |
    Name_expression_0 String (Maybe Type_0) [Type_0]
      deriving Show
  data Form_0 = Form_0 Name [Type_0] deriving Show
  data Kind_0 = Kind_0 Location_0 Kind_branch_0 deriving (Eq, Show)
  data Kind_branch_0 = Application_kind_0 Kind_0 Kind_0 | Name_kind_0 String deriving (Eq, Show)
  data Match_Algebraic_0 = Match_Algebraic_0 Name [Pat] Expression_0 deriving Show
  data Match_char_0 = Match_char_0 Location_0 Char Expression_0 deriving Show
  data Match_Int_0 = Match_Int_0 Location_0 Integer Expression_0 deriving Show
  data Match_Modular_0 = Match_Modular_0 Location_0 Modular Expression_0 deriving Show
  data Matches_0 =
    Matches_Algebraic_0 [Match_Algebraic_0] (Maybe (Location_0, Expression_0)) |
    Matches_char_0 [Match_char_0] Expression_0 |
    Matches_Int_0 [Match_Int_0] Expression_0 |
    Matches_Modular_0 [Match_Modular_0] (Maybe (Location_0, Expression_0))
      deriving Show
  data Method = Method Name [(Name, Kind_0)] [Constraint_0] Type_0 deriving Show
  data Modular = Modular Integer Integer deriving Show
  data Name = Name Location_0 String deriving Show
  data Pat = Pat Location_0 Pat_branch deriving Show
  data Pat_branch = Application_pat String [Pat] | Blank_pat | Name_pat String deriving Show
  data Pattern_1 = Pattern_1 Location_0 Pattern_0 deriving Show
  data Pattern_0 = Blank_pattern | Name_pattern String deriving Show
{-
  newtype Parser t = Parser {parser :: State -> Either Location_0 (t, State)}
-}
  type Parser = Parser' State (Either Location_0)
  newtype Parser' s f t = Parser {parser :: s -> f (t, s)}
  data State = State Tokens Location_0 deriving Show
  data Tree_0 = Tree_0 [Data_0] [Class_0] [Def_0] deriving Show
  data Tree_1 = Tree_1 [Name] Tree_0 deriving Show
  data Type_0 = Type_0 Location_0 Type_branch_0 deriving Show
  data Type_branch_0 =
    Application_type_0 Type_0 Type_0 | Char_type_0 Char | Int_type_0 Integer | Name_type_0 String [Kind_0] deriving Show
  class Get_location t where
    get_location :: t -> Location_0
  infixl 4 <&
  (<&) :: (Location_0 -> t) -> Parser () -> Parser t
  f <& p = f <$> parse_location <* p
  infixl 4 <&>
  (<&>) :: (Location_0 -> t -> u) -> Parser t -> Parser u
  f <&> p = f <$> parse_location <*> p
  instance Alternative Parser where
    Parser a <|> Parser b = Parser (\c -> left_bind (\d -> b (update_location c d)) (a c))
    empty = Parser (\(State _ l) -> Left l)
  instance (Alternative f, Monad f) => Alternative (Parser' s f) where
    Parser a <|> Parser b = Parser (\c -> a c <|> b c)
    empty = Parser (\_ -> empty)
{-
  instance Applicative Parser where
    Parser a <*> Parser b = Parser (a >=> \(c, d) -> first c <$> b d)
    pure x = Parser (\y -> Right (x, y))
  instance Functor Parser where
    fmap a (Parser b) = Parser (\c -> first a <$> b c)
-}
  instance Monad f => Applicative (Parser' s f) where
    Parser a <*> Parser b = Parser (a >=> \(c, d) -> first c <$> b d)
    pure x = Parser (\y -> return (x, y))
  instance Functor f => Functor (Parser' s f) where
    fmap a (Parser b) = Parser (\c -> first a <$> b c)
  instance Get_location Kind_0 where
    get_location (Kind_0 a _) = a
  instance Get_location Pat where
    get_location (Pat a _) = a
  instance Get_location Pattern_1 where
    get_location (Pattern_1 a _) = a
  instance Get_location Type_0 where
    get_location (Type_0 a _) = a
  init_location :: Location_0
  init_location = Location_0 0 0
  int_to_nat_type_0 :: Location_0 -> Integer -> Type_branch_0
  int_to_nat_type_0 l x =
    case x of
      0 -> Name_type_0 "!Zr" []
      _ -> Application_type_0 (Type_0 l (Name_type_0 "!Next" [])) (Type_0 l (int_to_nat_type_0 l (x - 1)))
  left_bind :: (t -> Either u v) -> Either t v -> Either u v
  left_bind a b =
    case b of
      Left c -> a c
      Right c -> Right c
  mk_let :: [Eqq] -> Expression_0 -> Expression_branch_0
  mk_let x y = (\(Expression_0 _ z) -> z) (Prelude.foldr mk_let' y x)
  mk_let' :: Eqq -> Expression_0 -> Expression_0
  mk_let' (Eqq (Name l x) y z) w =
    Expression_0
      l
      (Application_expression_0
        (Expression_0 l (Function_expression_0 (Pat l (Name_pat x)) w))
        (Prelude.foldr (\(Pat m a) -> \b -> Expression_0 m (Function_expression_0 (Pat m a) b)) z y))
  mk_list :: Location_0 -> [Expression_0] -> Expression_branch_0
  mk_list l =
    Prelude.foldr
      (\x -> \y ->
        Application_expression_0
          (Expression_0 l (Application_expression_0 (Expression_0 l (Name_expression_0 "Construct_List" Nothing [])) x))
          (Expression_0 l y))
      (Name_expression_0 "Empty_List" Nothing [])
  parse :: Parser t -> (Location_0 -> Location_1) -> String -> Err t
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
  parse_algebraic :: Parser Data_0
  parse_algebraic = parse_data' Algebraic_data_0 Algebraic_token (parse_round (parse_list 2 parse_form))
  parse_application_expression :: Parser Expression_branch_0
  parse_application_expression =
    (
      (\x -> foldl (Application_expression_0 <$> Expression_0 x)) <&>
      (Application_expression_0 <$> parse_bracketed_expression <*> parse_bracketed_expression) <*>
      many parse_bracketed_expression)
  parse_application_kind :: Parser Kind_branch_0
  parse_application_kind =
    (
      (\x -> foldl (Application_kind_0 <$> Kind_0 x)) <&>
      (Application_kind_0 <$> parse_bracketed_kind <*> parse_bracketed_kind) <*>
      many parse_bracketed_kind)
  parse_application_pat :: Parser Pat
  parse_application_pat =
    (\a -> \x -> \y -> \z -> Pat a (Application_pat x (y : z))) <&> parse_name <*> parse_brack_pat <*> many parse_brack_pat
  parse_application_type :: Parser Type_branch_0
  parse_application_type =
    (
      (\x -> foldl (Application_type_0 <$> Type_0 x)) <&>
      (Application_type_0 <$> parse_bracketed_type <*> parse_bracketed_type) <*>
      many parse_bracketed_type)
  parse_argument :: Parser t -> Parser u -> Parser (t, u)
  parse_argument p = (<*>) ((<*) ((,) <$> p) parse_colon)
  parse_arguments :: (Parser [(t, u)] -> Parser [(t, u)]) -> Parser t -> Parser u -> Parser [(t, u)]
  parse_arguments a b c = parse_optional a (parse_argument b c)
  parse_arguments' :: Parser t -> Parser [(t, Type_0)]
  parse_arguments' a = parse_arguments parse_round a parse_type
  parse_arguments'' :: Parser t -> Parser [(t, Kind_0)]
  parse_arguments'' a = parse_arguments parse_round a parse_kind
  parse_arrow :: Parser ()
  parse_arrow = parse_operator "->"
  parse_arrow' :: Parser (Expression_0 -> t) -> Parser t
  parse_arrow' p = p <* parse_arrow <*> parse_expression'
  parse_arrow_kind :: Parser Kind_branch_0
  parse_arrow_kind =
    (
      (\x -> \y -> Application_kind_0 (Kind_0 y (Application_kind_0 (Kind_0 y (Name_kind_0 "!Function")) x))) <$>
      (Kind_0 <&> (parse_round parse_arrow_kind <|> parse_application_kind <|> parse_name_kind)) <*>
      parse_arrow_loc <*>
      parse_kind)
  parse_arrow_loc :: Parser Location_0
  parse_arrow_loc = id <& parse_arrow
  parse_arrow_type :: Parser Type_branch_0
  parse_arrow_type =
    (
      (\x -> \y -> Application_type_0 (Type_0 y (Application_type_0 (Type_0 y (Name_type_0 "Function" [])) x))) <$>
      (
        Type_0 <&>
        (parse_round parse_arrow_type <|> parse_star_type <|> parse_application_type <|> parse_elementary_type)) <*>
      parse_arrow_loc <*>
      parse_type)
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
  parse_brack_pat :: Parser Pat
  parse_brack_pat = parse_round parse_application_pat <|> parse_elementary_pat
  parse_bracketed_expression :: Parser Expression_0
  parse_bracketed_expression = Expression_0 <&> (parse_elementary_expression <|> parse_round parse_composite_expression)
  parse_bracketed_kind :: Parser Kind_0
  parse_bracketed_kind = Kind_0 <&> (parse_round (parse_arrow_kind <|> parse_application_kind) <|> parse_name_kind)
  parse_bracketed_type :: Parser Type_0
  parse_bracketed_type = Type_0 <&> (parse_round parse_composite_type <|> parse_elementary_type)
  parse_brackets :: Token_0 -> Parser t -> Token_0 -> Parser t
  parse_brackets a b c = parse_token a *> b <* parse_token c
  parse_brnch :: Parser Brnch_0
  parse_brnch =
    (
      Brnch_0 <$>
      ((\x -> \y -> Name x ('!' : y)) <& parse_lift <*> parse_name) <*>
      many parse_name' <*
      parse_arrow <*>
      parse_name' <*>
      parse_arguments' parse_name')
  parse_brnchs :: Parser Data_0
  parse_brnchs =
    (
      Data_0 <$>
      parse_name'' Branching_token <*>
      (
        Branching_data_0 <$
        parse_token Left_square_token <*>
        ((\x -> \y -> Name x ('!' : y)) <& parse_lift <*> parse_name) <*>
        many parse_kind <*
        parse_token Right_square_token <*>
        parse_kinds <*
        parse_token Left_round_token <*>
        parse_list 2 parse_brnch <*
        parse_token Right_round_token))
  parse_char :: Parser Char
  parse_char =
    parse_elementary
      (\a ->
        case a of
          Char_token b -> Just b
          _ -> Nothing)
  parse_char_expression :: Parser Expression_branch_0
  parse_char_expression = Char_expression_0 <$> parse_char
  parse_char_type :: Parser Type_branch_0
  parse_char_type = Char_type_0 <$ parse_lift <*> parse_char
  parse_class :: Parser Class_0
  parse_class =
    (
      Class_0 <$>
      parse_name'' Class_token <*
      parse_token Left_curly_token <*>
      ((,) <$> parse_pattern' <* parse_colon <*> parse_kind) <*
      parse_token Right_curly_token <*>
      (Just <$ parse_operator "<" <*> parse_name' <* parse_operator ">" <|> pure Nothing) <*>
      parse_optional parse_round parse_method)
  parse_colon :: Parser ()
  parse_colon = parse_operator ":"
  parse_comma :: Parser ()
  parse_comma = parse_token Comma_token
  parse_composite_expression :: Parser Expression_branch_0
  parse_composite_expression =
    (
      parse_mod_expr <|>
      parse_list_expr <|>
      parse_function <|>
      parse_application_expression <|>
      parse_match_expression <|>
      parse_let_expression)
  parse_composite_type :: Parser Type_branch_0
  parse_composite_type = parse_arrow_type <|> parse_star_type <|> parse_application_type
  parse_constraint :: Parser Constraint_0
  parse_constraint = Constraint_0 <$> parse_name' <*> parse_name'
  parse_constraints :: Parser [Constraint_0]
  parse_constraints = parse_optional' (parse_operator "<" *> parse_list 1 parse_constraint <* parse_operator ">")
  parse_data :: Parser Data_0
  parse_data = parse_algebraic <|> parse_brnchs <|> parse_struct
  parse_data' :: (t -> Data_branch_0) -> Token_0 -> Parser t -> Parser Data_0
  parse_data' f a b = (\x -> \y -> \z -> Data_0 x (Plain_data_0 y (f z))) <$> parse_name'' a <*> parse_kinds <*> b
  parse_def :: Parser Def_0
  parse_def = parse_basic <|> parse_instance
  parse_default :: Parser Expression_0
  parse_default = parse_comma *> parse_token Default_token *> parse_arrow *> parse_expression'
  parse_default' :: Parser (Maybe (Location_0, Expression_0))
  parse_default' =
    Just <$ parse_comma <*> ((,) <& parse_token Default_token <* parse_arrow <*> parse_expression') <|> pure Nothing
  parse_elementary :: (Token_0 -> Maybe t) -> Parser t
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
  parse_elementary_expression :: Parser Expression_branch_0
  parse_elementary_expression =
    (
      parse_char_expression <|>
      parse_int_expression <|>
      Name_expression_0 "Empty_List" Nothing [] <$ parse_name_4 "List" <|>
      parse_name_expression)
  parse_elementary_pat :: Parser Pat
  parse_elementary_pat = parse_blank_pat <|> parse_name_pat
  parse_elementary_type :: Parser Type_branch_0
  parse_elementary_type = parse_char_type <|> parse_int_type <|> parse_name_type <|> (int_to_nat_type_0 <&> parse_int')
  parse_eq :: Parser ()
  parse_eq = parse_operator "="
  parse_eq' :: Parser Eqq
  parse_eq' = Eqq <$> parse_name' <*> many parse_brack_pat <* parse_eq <*> parse_expression'
  parse_error :: (Location_0 -> Location_1) -> Location_0 -> Err t
  parse_error a b = Left ("Parse error" ++ location' (a b))
  parse_expression :: String -> Err Expression_0
  parse_expression = parse parse_expression' (Location_1 "input")
  parse_expression' :: Parser Expression_0
  parse_expression' = Expression_0 <&> parse_expression_branch
  parse_expression_branch :: Parser Expression_branch_0
  parse_expression_branch = parse_composite_expression <|> parse_elementary_expression
  parse_form :: Parser Form_0
  parse_form = Form_0 <$> parse_name' <*> many parse_bracketed_type
  parse_function :: Parser Expression_branch_0
  parse_function = parse_arrow' (Function_expression_0 <$> parse_pat)
  parse_instance :: Parser Def_0
  parse_instance =
    (
      Instance_def_0 <&
      parse_token Instance_token <*>
      parse_name' <*
      parse_token Left_curly_token <*>
      ((\x -> \y -> Name x ('!' : y)) <& parse_lift <*> parse_name <|> parse_name') <*>
      parse_optional' (parse_brackets Left_square_token (parse_list 1 parse_kind) Right_square_token) <*>
      many parse_pattern_1 <*
      parse_token Right_curly_token <*>
      parse_constraints <*>
      parse_optional
        parse_round
        ((\x -> \y -> \z -> (x, (y, z))) <$> parse_name' <*> many parse_brack_pat <* parse_eq <*> parse_expression'))
  parse_int :: Parser Integer
  parse_int = (negate <$ parse_operator "-" <|> pure id) <*> parse_int'
  parse_int' :: Parser Integer
  parse_int' =
    parse_elementary
      (\a ->
        case a of
          Int_token b -> Just b
          _ -> Nothing)
  parse_int_expression :: Parser Expression_branch_0
  parse_int_expression = Int_expression_0 <$> parse_int
  parse_int_type :: Parser Type_branch_0
  parse_int_type = Int_type_0 <$ parse_lift <*> parse_int
  parse_let_expression :: Parser Expression_branch_0
  parse_let_expression =
    (
      mk_let <$
      parse_token Let_token <*>
      parse_list 1 parse_eq' <*
      parse_token In_token <*>
      parse_expression')
  parse_kind :: Parser Kind_0
  parse_kind = Kind_0 <&> parse_kind_branch
  parse_kind_branch :: Parser Kind_branch_0
  parse_kind_branch = parse_arrow_kind <|> parse_application_kind <|> parse_name_kind
  parse_kinds :: Parser [(Name, Kind_0)]
  parse_kinds = parse_arguments (\a -> parse_brackets Left_square_token a Right_square_token) parse_name' parse_kind
  parse_lift :: Parser ()
  parse_lift = parse_operator "!"
  parse_list :: Integer -> Parser t -> Parser [t]
  parse_list i p =
    case i of
      1 -> (:) <$> p <*> many (parse_comma *> p)
      _ -> (:) <$> p <* parse_comma <*> parse_list (i - 1) p
  parse_list_expr :: Parser Expression_branch_0
  parse_list_expr = mk_list <& parse_name_4 "List" <*> parse_round (parse_list 1 parse_expression')
  parse_load :: Parser Name
  parse_load = parse_name_3 Load_token ((flip (++) ".awf" <$> parse_name) <* parse_operator "." <* parse_name_4 "awf")
  parse_location :: Parser Location_0
  parse_location = Parser (\a -> Right (state_location a, a))
  parse_match_algebraic :: Parser Match_Algebraic_0
  parse_match_algebraic = parse_arrow' (Match_Algebraic_0 <$> parse_name' <*> many parse_brack_pat)
  parse_match_char :: Parser Match_char_0
  parse_match_char = parse_arrow' (Match_char_0 <&> parse_char)
  parse_match_expression :: Parser Expression_branch_0
  parse_match_expression =
    (
      Match_expression_0 <$
      parse_token Match_token <*>
      parse_expression' <*
      parse_token Left_curly_token <*>
      parse_matches <*
      parse_token Right_curly_token)
  parse_match_int :: Parser Match_Int_0
  parse_match_int = parse_arrow' (Match_Int_0 <&> parse_int)
  parse_match_modular :: Parser Match_Modular_0
  parse_match_modular = parse_arrow' (Match_Modular_0 <&> parse_modular)
  parse_matches :: Parser Matches_0
  parse_matches = parse_matches_modular <|> parse_matches_algebraic <|> parse_matches_char <|> parse_matches_int
  parse_matches_algebraic :: Parser Matches_0
  parse_matches_algebraic = Matches_Algebraic_0 <$> parse_list 1 parse_match_algebraic <*> parse_default'
  parse_matches_char :: Parser Matches_0
  parse_matches_char = Matches_char_0 <$> parse_list 1 parse_match_char <*> parse_default
  parse_matches_int :: Parser Matches_0
  parse_matches_int = Matches_Int_0 <$> parse_list 1 parse_match_int <*> parse_default
  parse_matches_modular :: Parser Matches_0
  parse_matches_modular = Matches_Modular_0 <$> parse_list 1 parse_match_modular <*> parse_default'
  parse_method :: Parser Method
  parse_method = Method <$> parse_name' <*> parse_kinds <*> parse_constraints <* parse_colon <*> parse_type
  parse_mod_expr :: Parser Expression_branch_0
  parse_mod_expr = Modular_expression_0 <$> parse_modular
  parse_modular :: Parser Modular
  parse_modular = flip Modular <$> parse_int' <* parse_token Hash_token <*> parse_int'
  parse_name :: Parser String
  parse_name =
    parse_elementary
      (\a ->
        case a of
          Name_token b -> Just b
          _ -> Nothing)
  parse_name' :: Parser Name
  parse_name' = Name <&> parse_name
  parse_name'' :: Token_0 -> Parser Name
  parse_name'' = flip parse_name_3 parse_name
  parse_name_3 :: Token_0 -> Parser String -> Parser Name
  parse_name_3 a b = Name <& parse_token a <*> b
  parse_name_4 :: String -> Parser ()
  parse_name_4 = parse_token <$> Name_token
  parse_name_expression :: Parser Expression_branch_0
  parse_name_expression =
    (
      Name_expression_0 <$>
      parse_name <*>
      parse_optional' (Just <$> parse_brackets Left_curly_token parse_type Right_curly_token) <*>
      parse_optional' (parse_brackets Left_square_token (parse_list 1 parse_type) Right_square_token))
  parse_name_kind :: Parser Kind_branch_0
  parse_name_kind = Name_kind_0 <$> parse_prom
  parse_name_pat :: Parser Pat
  parse_name_pat = (\x -> \y -> Pat x (Name_pat y)) <&> parse_name
  parse_name_pattern :: Parser Pattern_0
  parse_name_pattern = Name_pattern <$> parse_name
  parse_name_type :: Parser Type_branch_0
  parse_name_type =
    (
      Name_type_0 <$>
      parse_prom <*>
      parse_optional' (parse_brackets Left_square_token (parse_list 1 parse_kind) Right_square_token))
  parse_nothing :: Parser ()
  parse_nothing = Parser (\a -> Right ((), a))
  parse_operator :: String -> Parser ()
  parse_operator = parse_token <$> Operator_token
  parse_optional :: (Parser [t] -> Parser [t]) -> Parser t -> Parser [t]
  parse_optional a b = parse_optional' (a (parse_list 1 b))
  parse_optional' :: Alternative f => Parser (f t) -> Parser (f t)
  parse_optional' a = a <|> pure empty
  parse_pat :: Parser Pat
  parse_pat = parse_application_pat <|> parse_elementary_pat
  parse_pattern_0 :: Parser Pattern_0
  parse_pattern_0 = parse_blank <|> parse_name_pattern
  parse_pattern_1 :: Parser Pattern_1
  parse_pattern_1 = Pattern_1 <&> parse_pattern_0
  parse_pattern' :: Parser Name
  parse_pattern' = Name <&> ("_" <$ parse_token Blank_token <|> parse_name)
  parse_prom :: Parser String
  parse_prom = ((:) '!' <$ parse_lift <*> parse_name <|> parse_name)
  parse_round :: Parser t -> Parser t
  parse_round a = parse_brackets Left_round_token a Right_round_token
  parse_star :: Parser Location_0
  parse_star = id <& parse_operator "*"
  parse_star_type :: Parser Type_branch_0
  parse_star_type =
    (
      (\x -> \y -> Application_type_0 (Type_0 y (Application_type_0 (Type_0 y (Name_type_0 "Pair" [])) x))) <$>
      (
        Type_0 <&>
        (parse_round (parse_arrow_type <|> parse_star_type) <|> parse_application_type <|> parse_elementary_type)) <*>
      parse_star <*>
      (Type_0 <&> (parse_round parse_arrow_type <|> parse_star_type <|> parse_application_type <|> parse_elementary_type)))
  parse_struct :: Parser Data_0
  parse_struct = parse_data' Struct_data_0 Struct_token (parse_arguments' parse_name')
  parse_token :: Token_0 -> Parser ()
  parse_token a = parse_elementary (\b -> if b == a then Just () else Nothing)
  parse_tree :: (Location_0 -> Location_1) -> String -> Err Tree_1
  parse_tree = parse parse_tree'
  parse_tree' :: Parser Tree_1
  parse_tree' = Tree_1 <$> many parse_load <*> (Tree_0 <$> many parse_data <*> many parse_class <*> many parse_def)
  parse_type :: Parser Type_0
  parse_type = Type_0 <&> parse_type_branch
  parse_type_branch :: Parser Type_branch_0
  parse_type_branch = parse_composite_type <|> parse_elementary_type
  state_location :: State -> Location_0
  state_location (State (Tokens a b) _) =
    case a of
      [] -> b
      Token_1 c _ : _ -> c
  update_location :: State -> Location_0 -> State
  update_location (State a b) c = State a (max b c)
-----------------------------------------------------------------------------------------------------------------------------