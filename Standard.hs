-----------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Standard where
  import Data.Map
  import Tokenise
  import Tree
  data Brnch_6 = Brnch_6 Name [Name] Name [(Name, Type_8)] deriving Show
  data Class_7 = Class_7 Name (Name, Kind_0) (Maybe Name) [Method_9] deriving Show
  data Def_1 =
    Basic_def_1 Name [(Name, Kind_0)] [Constraint_0] Type_8 Expression_9 |
    Instance_1 Location_0 Name Name [Kind_0] [Pattern_1] [Constraint_0] [(Name, Expression_9)]
      deriving Show
  data Data_6 = Data_6 Name Data_br_6 deriving Show
  data Data_br_6 = Branching_data_6 Name [Kind_0] [(Name, Kind_0)] [Brnch_6] | Plain_data_6 [(Name, Kind_0)] Data_branch_6
    deriving Show
  data Data_branch_6 = Algebraic_data_6 [Form_6] | Struct_data_6 [(Name, Type_8)]
    deriving Show
  data Eqq' = Eqq' Name [Pat] Expression_9 deriving Show
  data Expression_9 =
    Application_expression_9 Expression_9 Expression_9 |
    Char_expression_9 Char |
    Function_expression_9 Pat Expression_9 |
    Int_expression_9 Integer |
    Let_expression_9 Eqq' Expression_9 |
    Match_expression_9 Location_0 Expression_9 Matches_9 |
    Modular_expression_9 Modular |
    Name_expression_9 Name (Maybe Type_8) [Type_8]
      deriving Show
  data Form_6 = Form_6 Name [Type_8] deriving Show
  type Map' t = Map String t
  data Match_Algebraic_9 = Match_Algebraic_9 Name [Pat] Expression_9 deriving Show
  data Match_char_9 = Match_char_9 Location_0 Char Expression_9 deriving Show
  data Match_Int_9 = Match_Int_9 Location_0 Integer Expression_9 deriving Show
  data Match_Modular_9 = Match_Modular_9 Location_0 Modular Expression_9 deriving Show
  data Matches_9 =
    Matches_Algebraic_9 [Match_Algebraic_9] (Maybe (Location_0, Expression_9)) |
    Matches_char_9 [Match_char_9] Expression_9 |
    Matches_Int_9 [Match_Int_9] Expression_9 |
    Matches_Modular_9 [Match_Modular_9] (Maybe (Location_0, Expression_9))
      deriving Show
  data Method_9 = Method_9 Name [(Name, Kind_0)] [Constraint_0] Type_8 deriving Show
  data Op = Op Integer Assoc String deriving Show
  data Op' = Op' Location_0 Op deriving Show
  data Tree_2 = Tree_2 [Data_6] [Class_7] [Def_1] deriving Show
  data Tree_3 = Tree_3 [Name] Tree_2 deriving Show
  data Type_5 = Application_type_5 Type_5 Type_5 | Char_type_5 Char | Int_type_5 Integer | Name_type_5 Name [Kind_0]
    deriving Show
  data Type_8 = Type_8 Location_0 Type_5 deriving Show
  pop :: (t -> t -> t, Name -> t) -> [(Op', t)] -> t -> Op' -> [(Op', t)]
  pop f x expr (Op' l (Op pr assoc name)) =
    let
      u = (Op' l (Op pr assoc name), expr) : x
    in
      case x of
        [] -> u
        (Op' l' (Op pr' assoc' name'), expr') : x' ->
          case pr' < pr || pr' == pr && assoc' == Lft of
            False -> u
            True -> pop' pop f x' l' name' expr' expr (Op' l (Op pr assoc name))
  pop' ::
    (
      ((t -> t -> t, Name -> t) -> [(Op', t)] -> t -> u) ->
      (t -> t -> t, Name -> t) ->
      [(Op', t)] ->
      Location_0 ->
      String ->
      t ->
      t ->
      u)
  pop' h (f, g) x' l name expr' expr = h (f, g) x' (f (f (g (Name l name)) expr') expr)
  pop_all :: (t -> t -> t, Name -> t) -> [(Op', t)] -> t -> t
  pop_all f x expr =
    case x of
      [] -> expr
      (Op' l (Op _ _ name), expr') : x' -> pop' pop_all f x' l name expr' expr
  shunting_yard ::
    (Location_0 -> Location_1) -> (t -> Err u, u -> u -> u, Name -> u) -> Map' Op -> [(Op', u)] -> t -> [(Name, t)] -> Err u
  shunting_yard a (f, g, h) ops x expr y =
    (
      f expr >>=
      \expr'' ->
        case y of
          [] -> Right (pop_all (g, h) x expr'')
          (Name l op, expr') : y' ->
            und_err
              op
              ops
              "operator"
              (a l)
              (\op' -> shunting_yard a (f, g, h) ops (pop (g, h) x expr'' (Op' l op')) expr' y'))
  standard :: (Location_0 -> Location_1) -> String -> Err Tree_3
  standard a b = parse_tree a b >>= standard_0 a
  standard_0 :: (Location_0 -> Location_1) -> Tree_1 -> Err Tree_3
  standard_0 c (Tree_1 a b) = Tree_3 a <$> standard_1 c b
  standard_1 :: (Location_0 -> Location_1) -> Tree_0 -> Err Tree_2
  standard_1 d (Tree_0 a b c) = Tree_2 <$> traverse (std_dat d) a <*> traverse (std_cls d) b <*> standard_defs d c
  standard_arguments :: (Location_0 -> Location_1) -> [(Pat, Type_7)] -> Type_7 -> Expression_0 -> Err (Type_8, Expression_9)
  standard_arguments a b c d =
    case b of
      [] -> (,) <$> std_type a c <*> std_expr a d
      (e, Type_7 l f) : g ->
        (
          (\h -> \(Type_8 i j, k) ->
            (
              Type_8 i (Application_type_5 (Application_type_5 (Name_type_5 (Name l "Function") []) h) j),
              Function_expression_9 e k)) <$>
          std_type' a f <*>
          standard_arguments a g c d)
  standard_def :: (Location_0 -> Location_1) -> Def_0 -> Err Def_1
  standard_def i a =
    case a of
      Basic_def_0 b c g d e f -> uncurry (Basic_def_1 b c g) <$> standard_arguments i d e f
      Instance_def_0 b c d h f g e -> Instance_1 b c d h f g <$> traverse (std_inst i) e
  standard_defs :: (Location_0 -> Location_1) -> [Def_0] -> Err [Def_1]
  standard_defs a = traverse (standard_def a)
  std_cls :: (Location_0 -> Location_1) -> Class_0 -> Err Class_7
  std_cls e (Class_0 a b c d) = Class_7 a b c <$> traverse (std_mthd e) d
  std_dat :: (Location_0 -> Location_1) -> Data_0 -> Err Data_6
  std_dat a (Data_0 b c) =
    (
      Data_6 b <$>
      case c of
        Branching_data_0 d e f g ->
          (
            Branching_data_6 d e f <$>
            traverse (\(Brnch_0 h i j k) -> Brnch_6 h i j <$> traverse (\(l, m) -> (,) l <$> std_type a m) k) g)
        Plain_data_0 d e ->
          (
            Plain_data_6 d <$>
            case e of
              Algebraic_data_0 f -> Algebraic_data_6 <$> traverse (\(Form_0 g h) -> Form_6 g <$> traverse (std_type a) h) f
              Struct_data_0 f -> Struct_data_6 <$> traverse (\(g, h) -> (,) g <$> std_type a h) f))
  std_default :: (Location_0 -> Location_1) -> Maybe (Location_0, Expression_0)  -> Err (Maybe (Location_0, Expression_9))
  std_default a b =
    case b of
      Just (c, d) -> (\e -> Just (c, e)) <$> std_expr a d
      Nothing -> Right Nothing
  std_eqq :: (Location_0 -> Location_1) -> Eqq -> Err Eqq'
  std_eqq a (Eqq b c d) = Eqq' b c <$> std_expr a d
  std_expr :: (Location_0 -> Location_1) -> Expression_0 -> Err Expression_9
  std_expr a b =
    case b of
      Application_expression_0 c d -> Application_expression_9 <$> std_expr a c <*> std_expr a d
      Char_expression_0 c -> Right (Char_expression_9 c)
      Function_expression_0 c d -> Function_expression_9 c <$> std_expr a d
      Int_expression_0 c -> Right (Int_expression_9 c)
      Let_expression_0 c d -> Let_expression_9 <$> std_eqq a c <*> std_expr a d
      Match_expression_0 c d e -> Match_expression_9 c <$> std_expr a d <*> std_matches a e
      Modular_expression_0 c -> Right (Modular_expression_9 c)
      Name_expression_0 c d e -> Name_expression_9 c <$> traverse (std_type a) d <*> traverse (std_type a) e
  std_inst :: (Location_0 -> Location_1) -> (Name, ([Pat], Expression_0)) -> Err (Name, Expression_9)
  std_inst a (b, (c, d)) = (\e -> (b, Prelude.foldr Function_expression_9 e c)) <$> std_expr a d
  std_match_alg :: (Location_0 -> Location_1) -> Match_Algebraic_0 -> Err Match_Algebraic_9
  std_match_alg a (Match_Algebraic_0 b c d) = Match_Algebraic_9 b c <$> std_expr a d
  std_match_char :: (Location_0 -> Location_1) -> Match_char_0 -> Err Match_char_9
  std_match_char a (Match_char_0 b c d) = Match_char_9 b c <$> std_expr a d
  std_match_int :: (Location_0 -> Location_1) -> Match_Int_0 -> Err Match_Int_9
  std_match_int a (Match_Int_0 b c d) = Match_Int_9 b c <$> std_expr a d
  std_match_modular :: (Location_0 -> Location_1) -> Match_Modular_0 -> Err Match_Modular_9
  std_match_modular a (Match_Modular_0 b d c) = Match_Modular_9 b d <$> std_expr a c
  std_matches :: (Location_0 -> Location_1) -> Matches_0 -> Err Matches_9
  std_matches a b =
    case b of
      Matches_Algebraic_0 c d -> Matches_Algebraic_9 <$> traverse (std_match_alg a) c <*> std_default a d
      Matches_char_0 c d -> Matches_char_9 <$> traverse (std_match_char a) c <*> std_expr a d
      Matches_Int_0 c d -> Matches_Int_9 <$> traverse (std_match_int a) c <*> std_expr a d
      Matches_Modular_0 c d -> Matches_Modular_9 <$> traverse (std_match_modular a) c <*> std_default a d
  std_mthd :: (Location_0 -> Location_1) -> Method -> Err Method_9
  std_mthd a (Method b c d e) = Method_9 b c d <$> std_type a e
  std_type :: (Location_0 -> Location_1) -> Type_7 -> Err Type_8
  std_type c (Type_7 a b) = Type_8 a <$> std_type' c b
  std_type' :: (Location_0 -> Location_1) -> Type_0 -> Err Type_5
  std_type' e b =
    case b of
      Application_type_0 c d -> Prelude.foldl Application_type_5 <$> std_type' e c <*> traverse (std_type' e) d
      Char_type_0 c -> Right (Char_type_5 c)
      Int_type_0 c -> Right (Int_type_5 c)
      Name_type_0 c d -> Right (Name_type_5 c d)
      Op_type_0 a c ->
        shunting_yard
          e
          (std_type' e, Application_type_5, \f -> Name_type_5 f [])
          (fromList [("*", Op 0 Rght "Pair"), ("+", Op 1 Rght "Either"), ("->", Op 2 Rght "Function")])
          []
          a
          c
  und_err :: String -> Map' t -> String -> Location_1 -> (t -> Err u) -> Err u
  und_err a b c d f =
    case Data.Map.lookup a b of
      Just e -> f e
      Nothing -> Left ("Undefined " ++ c ++ " " ++ a ++ location' d)
-----------------------------------------------------------------------------------------------------------------------------