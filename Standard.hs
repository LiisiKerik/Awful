--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Standard where
  import Data.Bifunctor
  import Data.Map
  import Tokenise
  import Tree
  data Case_1 = Case_1 Alg_pat Expression_9 deriving Show
  data Class_7 = Class_7 Name (Name, Kind_0) (Maybe Name) [Method_9] deriving Show
  data Def_1 =
    Basic_def_1 Name [(Name, Kind_0)] [Constraint_0] Type_8 Expression_9 |
    Instance_1 Location_0 Name Name [Pattern_1] [Constraint_0] [(Name, Expression_9)]
      deriving Show
  data Data_6 = Data_6 Location_0 String [(Name, Kind_0)] Data_branch_6 deriving Show
  data Data_br_6 = Data_br_6 Name [(Name, Type_8)] deriving Show
  data Data_branch_6 = Algebraic_data_6 [Form_6] | Branching_data_6 Data_br_6 Name Data_br_6 | Struct_data_6 [(Name, Type_8)]
    deriving Show
  data Eqq' = Eqq' Name [Pat] Expression_9 deriving Show
  data Expression_9 =
    Application_expression_9 Expression_9 Expression_9 |
    Branch_expression_9 Name Expression_9 Name Expression_9 |
    Char_expression_9 Char |
    Function_expression_9 Pat Expression_9 |
    Int_expression_9 Integer |
    Let_expression_9 Eqq' Expression_9 |
    Match_expression_9 Location_0 Expression_9 [Case_1] |
    Modular_expression_9 Modular |
    Name_expression_9 Name (Maybe Type_8) [Type_8]
      deriving Show
  data Form_6 = Form_6 Name [Type_8] deriving Show
  data Location' = Language | Library Location_1 deriving Show
  type Map' t = Map String t
  data Method_9 = Method_9 Name [(Name, Kind_0)] [Constraint_0] Type_8 deriving Show
  data Op = Op Integer Assoc String deriving Show
  data Op' = Op' Location_0 Op deriving Show
  data Opdecl_1 = Opdecl_1 Location_0 String Name deriving Show
  data Status = New | Old deriving (Eq, Show)
  data Tree_2 = Tree_2 [Data_6] [Class_7] [Opdecl_1] [Def_1] deriving Show
  data Tree_3 = Tree_3 [Name] Tree_2 deriving Show
  data Type_5 = Application_type_5 Type_5 Type_5 | Name_type_5 Name deriving Show
  data Type_8 = Type_8 Location_0 Type_5 deriving Show
  gather_ops :: (Location_0 -> Location_1) -> Map' (Op, Status) -> [Opdecl_0] -> (Map' (Op, Status), [Opdecl_1])
  gather_ops a b c =
    case c of
      [] -> (b, [])
      Opdecl_0 d e (Name f g) h i : j -> second ((:) (Opdecl_1 d e (Name f g))) (gather_ops a (ins_new e (Op h i g) b) j)
  ins_new :: Ord t => t -> u -> Map t (u, Status) -> Map t (u, Status)
  ins_new a b = Data.Map.insert a (b, New)
  int_to_nat :: Integer -> Type_5
  int_to_nat a =
    case a of
      0 -> Name_type_5 (Name (Location_0 0 0) "Zero")
      _ -> Application_type_5 (Name_type_5 (Name (Location_0 0 0) "Next")) (int_to_nat (a - 1))
  old :: Map' t -> Map' (t, Status)
  old = (<$>) (flip (,) Old)
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
  rem_old :: Map' (t, Status) -> Map' t
  rem_old a = fst <$> Data.Map.filter (\(_, b) -> b == New) a
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
  standard_1 :: (Location_0 -> Location_1) -> Map' Op -> Tree_0 -> Err (Map' Op, Tree_2)
  standard_1 d f (Tree_0 a b e c) =
      let
        (i, j) = gather_ops d (old f) e
      in
        (
          (\g -> \h -> \k -> (rem_old i, Tree_2 g h j k)) <$>
          traverse (std_dat d) a <*>
          traverse (std_cls d) b <*>
          standard_defs d (fst <$> i) c)
  standard_arguments ::
    (Location_0 -> Location_1) -> Map' Op -> [(Pat, Type_7)] -> Type_7 -> Expression_0 -> Err (Type_8, Expression_9)
  standard_arguments a m b c d =
    case b of
      [] -> (,) <$> std_type a c <*> std_expr a m d
      (e, Type_7 l f) : g ->
        (
          (\h -> \(Type_8 i j, k) ->
            (
              Type_8 i (Application_type_5 (Application_type_5 (Name_type_5 (Name l "Function")) h) j),
              Function_expression_9 e k)) <$>
          std_type' a f <*>
          standard_arguments a m g c d)
  standard_def :: (Location_0 -> Location_1) -> Map' Op -> Def_0 -> Err Def_1
  standard_def i j a =
    case a of
      Basic_def_0 b c g d e f -> uncurry (Basic_def_1 b c g) <$> standard_arguments i j d e f
      Instance_def_0 b c d f g e -> Instance_1 b c d f g <$> traverse (std_inst i j) e
  standard_defs :: (Location_0 -> Location_1) -> Map' Op -> [Def_0] -> Err [Def_1]
  standard_defs a b = traverse (standard_def a b)
  std_br :: (Location_0 -> Location_1) -> Data_br_0 -> Err Data_br_6
  std_br a (Data_br_0 b c) = Data_br_6 b <$> traverse (\(g, h) -> (,) g <$> std_type a h) c
  std_case :: (Location_0 -> Location_1) -> Map' Op -> Case_0 -> Err Case_1
  std_case a b (Case_0 c e) = Case_1 c <$> std_expr a b e
  std_cls :: (Location_0 -> Location_1) -> Class_0 -> Err Class_7
  std_cls e (Class_0 a b c d) = Class_7 a b c <$> traverse (std_mthd e) d
  std_dat :: (Location_0 -> Location_1) -> Data_0 -> Err Data_6
  std_dat x y =
    case y of
      Algebraic_data_0 a b c d ->
        (\e -> Data_6 a b c (Algebraic_data_6 e)) <$> traverse (\(Form_0 g h) -> Form_6 g <$> traverse (std_type x) h) d
      Branching_data_0 a b c d e f -> (\g -> \h -> Data_6 a b c (Branching_data_6 g e h)) <$> std_br x d <*> std_br x f
      Struct_data_0 a b c d -> (\e -> Data_6 a b c (Struct_data_6 e)) <$> traverse (\(g, h) -> (,) g <$> std_type x h) d
  std_default ::
    (Location_0 -> Location_1) -> Map' Op -> Maybe (Location_0, Expression_0)  -> Err (Maybe (Location_0, Expression_9))
  std_default a f b =
    case b of
      Just (c, d) -> (\e -> Just (c, e)) <$> std_expr a f d
      Nothing -> Right Nothing
  std_eqq :: (Location_0 -> Location_1) -> Map' Op -> Eqq -> Err Eqq'
  std_eqq a e (Eqq b c d) = Eqq' b c <$> std_expr a e d
  std_expr :: (Location_0 -> Location_1) -> Map' Op -> Expression_0 -> Err Expression_9
  std_expr a f b =
    case b of
      Application_expression_0 c d -> Prelude.foldl Application_expression_9 <$> std_expr a f c <*> traverse (std_expr a f) d
      Branch_expression_0 c d e g -> (\h -> Branch_expression_9 c h e) <$> std_expr a f d <*> std_expr a f g
      Char_expression_0 c -> Right (Char_expression_9 c)
      Function_expression_0 c d -> Function_expression_9 c <$> std_expr a f d
      Int_expression_0 c -> Right (Int_expression_9 c)
      Let_expression_0 c d -> Let_expression_9 <$> std_eqq a f c <*> std_expr a f d
      Match_expression_0 c d e -> Match_expression_9 c <$> std_expr a f d <*> std_matches a f e
      Modular_expression_0 c -> Right (Modular_expression_9 c)
      Name_expression_0 c d e -> Name_expression_9 c <$> traverse (std_type a) d <*> traverse (std_type a) e
      Op_expression_0 c d ->
        shunting_yard a (std_expr a f, Application_expression_9, \e -> Name_expression_9 e Nothing []) f [] c d
  std_inst :: (Location_0 -> Location_1) -> Map' Op -> (Name, ([Pat], Expression_0)) -> Err (Name, Expression_9)
  std_inst a f (b, (c, d)) = (\e -> (b, Prelude.foldr Function_expression_9 e c)) <$> std_expr a f d
  std_matches :: (Location_0 -> Location_1) -> Map' Op -> [Case_0] -> Err [Case_1]
  std_matches a b = traverse (std_case a b)
  std_mthd :: (Location_0 -> Location_1) -> Method -> Err Method_9
  std_mthd a (Method b c d e) = Method_9 b c d <$> std_type a e
  std_type :: (Location_0 -> Location_1) -> Type_7 -> Err Type_8
  std_type c (Type_7 a b) = Type_8 a <$> std_type' c b
  std_type' :: (Location_0 -> Location_1) -> Type_0 -> Err Type_5
  std_type' e b =
    case b of
      Application_type_0 c d -> Prelude.foldl Application_type_5 <$> std_type' e c <*> traverse (std_type' e) d
      Name_type_0 c -> Right (Name_type_5 c)
      Nat_type_0 a -> Right (int_to_nat a)
      Op_type_0 a c ->
        shunting_yard
          e
          (std_type' e, Application_type_5, \f -> Name_type_5 f)
          (fromList [("*", Op 0 Rght "Pair"), ("+", Op 1 Rght "Either"), ("->", Op 2 Rght "Function")])
          []
          a
          c
  und_err :: String -> Map' t -> String -> Location_1 -> (t -> Err u) -> Err u
  und_err a b c d f =
    case Data.Map.lookup a b of
      Just e -> f e
      Nothing -> Left ("Undefined " ++ c ++ " " ++ a ++ location' d)
--------------------------------------------------------------------------------------------------------------------------------