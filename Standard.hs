--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Standard where
  import Data.Bifunctor
  import Data.Map
  import Tokenise
  import Tree
  data Case_1 = Case_1 Alg_pat Expression_9 deriving Show
  data Case_6 = Case_6 Alg_pat Expression_6 deriving Show
  data Class_7 = Class_7 Name (Name, Kind_0) (Maybe Name) [Method_9] deriving Show
  data Def_1 =
    Basic_def_1 Name [(Name, Kind_0)] [Constraint_0] Type_8 Expression_9 |
    Instance_1 Location_0 Name Name [Pattern_1] [Constraint_0] [(Name, Expression_9)]
      deriving Show
  data Data_6 = Data_6 Location_0 String [(Name, Kind_0)] Data_branch_6 deriving Show
  data Data_br_6 = Data_br_6 Name [(Name, Type_8)] deriving Show
  data Data_branch_6 =
    Algebraic_data_6 [Form_6] | Branching_data_6 Data_br_6 Name Data_br_6 | Struct_data_6 [(Name, Type_8)] Struct_status
      deriving Show
-- TODO: do something with those locations; they're excessive
  data Expression_6 =
    Application_expression_6 Expression_6 Expression_6 |
    Branch_expression_6 Name Expression_6 Name Expression_6 |
    Case_expression_6 Expression_6 Expression_6 (String, String) Expression_6 |
    Char_expression_6 Char |
    Function_expression_6 Pat Expression_6 |
    Function_syn_expression_6 String Expression_6 |
    Int_expression_6 Integer |
    Let_expression_6 Name [Pat] Expression_6 Expression_6 |
    List_expression_6 [Expression_6] |
    Match_expression_6 Location_0 Expression_6 [Case_6] |
    Modular_expression_6 Modular |
    Name_expression_6 Maybename (Maybe Type_7) [Type_7] |
    Op_expression_6 Expression_6 [(Maybename, Expression_6)] |
    Syn_app_expression_6 Expression_6 Expression_6 |
    Syntax_expression_6 Maybename
      deriving Show
  data Expression_9 =
    Application_expression_9 Expression_9 Expression_9 |
    Branch_expression_9 Name Expression_9 Name Expression_9 |
    Char_expression_9 Char |
    Function_expression_9 Pat Expression_9 |
    Int_expression_9 Integer |
    Let_expression_9 Name [Pat] Expression_9 Expression_9 |
    Match_expression_9 Location_0 Expression_9 [Case_1] |
    Modular_expression_9 Modular |
    Name_expression_9 Name (Maybe Type_8) [Type_8]
      deriving Show
  data Form_6 = Form_6 Name [Type_8] deriving Show
  data Location' = Language | Library Location_1 deriving Show
  type Locations = Map' Location'
  type Map' t = Map String t
  data Maybename = Maybename (Maybe Location_0) String deriving Show
  data Method_9 = Method_9 Name [(Name, Kind_0)] [Constraint_0] Type_8 deriving Show
  data Op = Op Integer Assoc String deriving Show
  data Op' = Op' Location_0 Op deriving Show
  data Opdecl_1 = Opdecl_1 Location_0 String Name deriving Show
  data Status = New | Old deriving (Eq, Show)
  data Struct_status = Hidden_str | Restricted_str (Location_0, Expression_9) | Standard_str deriving Show
  data Syntax_1 = Syntax_1 Location_0 String Syntax_type Syntax_expr_1 deriving Show
  data Syntax_2 = Syntax_2 Location_0 String Syntax_type Syntax_expr_2 deriving Show
  data Syntax_expr_1 =
    Application_syntax_1 Syntax_expr_1 Syntax_expr_1 |
    Case_syntax_1 Name Syntax_expr_1 (Name, Name) Syntax_expr_1 |
    Function_syntax_1 Name Syntax_expr_1 |
    Name_syntax_1 String |
    Op_syntax_1 Syntax_expr_1 [(String, Syntax_expr_1)] |
    Syntax_syntax_1 Name
      deriving Show
  data Syntax_expr_2 =
    Application_syntax_2 Syntax_expr_2 Syntax_expr_2 |
    Case_syntax_2 Name Syntax_expr_2 (String, String) Syntax_expr_2 |
    Function_syntax_2 String Syntax_expr_2 |
    Name_syntax_2 String |
    Op_syntax_2 Syntax_expr_2 [(String, Syntax_expr_2)] |
    Syntax_syntax_2 Name
      deriving Show
  data Syntax_type' = Arrow_syntax' Syntax_type' Syntax_type' | Expr_syntax' | List_syntax' Syntax_type' | Var_syntax' String
    deriving Show
  data Tree_2 = Tree_2 [Data_6] [Class_7] [Opdecl_1] [Def_1] deriving Show
  data Tree_3 = Tree_3 [Name] Tree_2 deriving Show
  data Type_5 = Application_type_5 Type_5 Type_5 | Name_type_5 Name deriving Show
  data Type_8 = Type_8 Location_0 Type_5 deriving Show
  add :: Ord t => Map t u -> t -> u -> Either u (Map t u)
  add x y z =
    let
      (w, x') = insertLookupWithKey (return return) y z x
    in
      case w of
        Just z' -> Left z'
        Nothing -> Right x'
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
  location_err :: String -> Location' -> Location_1 -> String
  location_err a c d =
    (
      "Conflicting " ++
      a ++
      (case c of
        Language -> " in the language"
        Library e -> location e) ++
      " and" ++
      location' d)
  naming_name :: String -> Name -> Locations -> Err (Locations, String)
  naming_name f (Name a c) d =
    bimap (flip (location_err ("definitions of " ++ c)) (Location_1 f a)) (flip (,) c) (add d c (Library (Location_1 f a)))
  naming_syntax_0 :: String -> Syntax_1 -> Locations -> Err Locations
  naming_syntax_0 a (Syntax_1 c d _ _) b = fst <$> naming_name a (Name c d) b
  naming_syntax_1 :: String -> Locations -> Syntax_1 -> Err Syntax_2
  naming_syntax_1 a b (Syntax_1 c d f g) = Syntax_2 c d f <$> naming_syntax_expr a b g
  naming_syntax_expr :: String -> Locations -> Syntax_expr_1 -> Err Syntax_expr_2
  naming_syntax_expr a b c =
    case c of
      Application_syntax_1 d e -> Application_syntax_2 <$> naming_syntax_expr a b d <*> naming_syntax_expr a b e
      Case_syntax_1 d e (f, g) h ->
        (
          (\i -> \(k, l) -> Case_syntax_2 d i k l) <$>
          (naming_name a d b >>= \(i, _) -> naming_syntax_expr a i e) <*>
          (naming_name a f b >>= \(i, j) -> naming_name a g i >>= \(k, l) -> (,) (j, l) <$> naming_syntax_expr a k h))
      Function_syntax_1 d e -> naming_name a d b >>= \(f, g) -> Function_syntax_2 g <$> naming_syntax_expr a f e
      Name_syntax_1 d -> Right (Name_syntax_2 d)
      Op_syntax_1 d e -> Op_syntax_2 <$> naming_syntax_expr a b d <*> traverse (\(f, g) -> (,) f <$> naming_syntax_expr a b g) e
      Syntax_syntax_1 d -> Right (Syntax_syntax_2 d)
  naming_syntaxes :: String -> (Locations, [Syntax_1]) -> Err (Locations, [Syntax_2])
  naming_syntaxes a (b, c) = naming_syntaxes_0 a c b >>= \d -> (,) d <$> naming_syntaxes_1 a d c
  naming_syntaxes_0 :: String -> [Syntax_1] -> Locations -> Err Locations
  naming_syntaxes_0 a c b =
    case c of
      [] -> Right b
      d : e -> naming_syntax_0 a d b >>= \f -> naming_syntaxes_0 a e f
  naming_syntaxes_1 :: String -> Locations -> [Syntax_1] -> Err [Syntax_2]
  naming_syntaxes_1 a b c =
    case c of
      [] -> Right []
      d : e -> (:) <$> naming_syntax_1 a b d <*> naming_syntaxes_1 a b e
  occ_syn :: String -> Syntax_type' -> Bool
  occ_syn a b =
    case b of
      Arrow_syntax' c d -> occ_syn a c && occ_syn a d
      Expr_syntax' -> False
      List_syntax' c -> occ_syn a c
      Var_syntax' c -> c == a
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
  process_syntax ::
    (
      String ->
      [Syntax] ->
      (Locations, Map' Syntax_type, Map' Expression_6) ->
      Err (Locations, Map' (Syntax_type, Status), Map' Expression_6))
  process_syntax a b (c, d, e) =
    naming_syntaxes a (c, std_syntax <$> b) >>= \(f, g) -> (\(h, i) -> (f, h, i)) <$> type_syntaxes (Location_1 a) g (d, e)
  rem_old :: Map' (t, Status) -> Map' t
  rem_old a = fst <$> Data.Map.filter (\(_, b) -> b == New) a
  repl_syn :: String -> Syntax_type' -> Syntax_type' -> Syntax_type'
  repl_syn a b c =
    let
      f = repl_syn a b
    in
      case c of
        Arrow_syntax' d e -> Arrow_syntax' (f d) (f e)
        Expr_syntax' -> Expr_syntax'
        List_syntax' d -> List_syntax' (f d)
        Var_syntax' d ->
          case d == a of
            False -> c
            True -> b
  show_syn :: Syntax_type' -> String
  show_syn a =
    case a of
      Arrow_syntax' b c -> show_syn' b ++ " -> " ++ show_syn c
      Expr_syntax' -> "Expr"
      List_syntax' b -> "[" ++ show_syn b ++ "]"
      Var_syntax' b -> b
  show_syn' :: Syntax_type' -> String
  show_syn' a =
    let
      b = show_syn a
    in
      case a of
        Arrow_syntax' _ _ -> "(" ++ b ++ ")"
        _ -> b
  show_syn_0 :: Syntax_type -> String
  show_syn_0 a =
    case a of
      Arrow_syntax b c -> show_syn_1 b ++ " -> " ++ show_syn_0 c
      Expr_syntax -> "Expr"
      List_syntax b -> "[" ++ show_syn_0 b ++ "]"
  show_syn_1 :: Syntax_type -> String
  show_syn_1 a =
    let
      b = show_syn_0 a
    in
      case a of
        Arrow_syntax _ _ -> "(" ++ b ++ ")"
        _ -> b
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
  solve_syn :: (String, Location_1) -> [(Syntax_type', Syntax_type')] -> Err ()
  solve_syn m a =
    case a of
      [] -> Right ()
      b : c ->
        case b of
          (Arrow_syntax' x y, Arrow_syntax' z w) -> solve_syn m ((x, z) : (y, w) : c)
          (Expr_syntax', Expr_syntax') -> solve_syn m c
          (List_syntax' x, List_syntax' y) -> solve_syn m ((x, y) : c)
          (Var_syntax' x, Var_syntax' y) -> solve_syn2 m x (Var_syntax' y) c
          (Var_syntax' x, y) -> solve_syn' m x y c
          (x, Var_syntax' y) -> solve_syn' m y x c
          (x, y) -> type_mism_syn m x y
  solve_syn' :: (String, Location_1) -> String -> Syntax_type' -> [(Syntax_type', Syntax_type')] -> Err ()
  solve_syn' a b c d =
    case occ_syn b c of
      False -> solve_syn2 a b c d
      True -> type_mism_syn a (Var_syntax' b) c
  solve_syn2 :: (String, Location_1) -> String -> Syntax_type' -> [(Syntax_type', Syntax_type')] -> Err ()
  solve_syn2 a b c d =
    let
      f = repl_syn b c
    in
      solve_syn a (bimap f f <$> d)
  standard_1 ::
    (
      String ->
      (Locations, Map' Syntax_type, Map' Expression_6, Map' Op) ->
      Tree_0 ->
      Err ((Locations, Map' Syntax_type, Map' Expression_6, Map' Op), Tree_2))
  standard_1 d (w, n, m, f) (Tree_0 y a b e c) =
    (
      process_syntax d y (w, n, m) >>=
      \(o, p, q) ->
        let
          (i, j) = gather_ops (Location_1 d) (old f) e
        in
          (
            (\g -> \h -> \k -> ((o, rem_old p, q, rem_old i), Tree_2 g h j k)) <$>
            traverse (std_dat (Location_1 d) (fst <$> p, q, f)) a <*>
            traverse (std_cls (Location_1 d)) b <*>
            standard_defs (Location_1 d) (fst <$> p, q, fst <$> i) c))
  standard_arguments ::
    (
      (Location_0 -> Location_1) ->
      String ->
      (Map' Syntax_type, Map' Expression_6, Map' Op) ->
      [(Pat, Type_7)] ->
      Type_7 ->
      Expression_0 ->
      Err (Type_8, Expression_9))
  standard_arguments a y m b c d =
    case b of
      [] -> (,) <$> std_type a c <*> std_expr a y m d
      (e, Type_7 l f) : g ->
        (
          (\h -> \(Type_8 i j, k) ->
            (
              Type_8 i (Application_type_5 (Application_type_5 (Name_type_5 (Name l "Function")) h) j),
              Function_expression_9 e k)) <$>
          std_type' a f <*>
          standard_arguments a y m g c d)
  standard_def :: (Location_0 -> Location_1) -> (Map' Syntax_type, Map' Expression_6, Map' Op) -> Def_0 -> Err Def_1
  standard_def i j a =
    case a of
      Basic_def_0 (Name b y) c g d e f ->
        uncurry (Basic_def_1 (Name b y) c g) <$> standard_arguments i ("definition " ++ y ++ location' (i b)) j d e f
      Instance_def_0 b c (Name d y) f g e -> Instance_1 b c (Name d y) f g <$> traverse (std_inst i y j) e
  standard_defs :: (Location_0 -> Location_1) -> (Map' Syntax_type, Map' Expression_6, Map' Op) -> [Def_0] -> Err [Def_1]
  standard_defs a b = traverse (standard_def a b)
  std_br :: (Location_0 -> Location_1) -> Data_br_0 -> Err Data_br_6
  std_br a (Data_br_0 b c) = Data_br_6 b <$> traverse (\(g, h) -> (,) g <$> std_type a h) c
  std_cls :: (Location_0 -> Location_1) -> Class_0 -> Err Class_7
  std_cls e (Class_0 a b c d) = Class_7 a b c <$> traverse (std_mthd e) d
  std_dat :: (Location_0 -> Location_1) -> (Map' Syntax_type, Map' Expression_6, Map' Op) -> Data_0 -> Err Data_6
  std_dat x (t, m, z) y =
    case y of
      Algebraic_data_0 a b c d ->
        (\e -> Data_6 a b c (Algebraic_data_6 e)) <$> traverse (\(Form_0 g h) -> Form_6 g <$> traverse (std_type x) h) d
      Branching_data_0 a b c d e f -> (\g -> \h -> Data_6 a b c (Branching_data_6 g e h)) <$> std_br x d <*> std_br x f
      Struct_data_0 a f b c d j ->
        (
          (\i -> \e -> Data_6 a b c (Struct_data_6 e i)) <$>
          std_stat x b (t, m, z) a f j <*>
          traverse (\(g, h) -> (,) g <$> std_type x h) d)
  std_exp :: (Location_0 -> Location_1) -> Map' Op -> Expression_6 -> Err Expression_9
  std_exp a b c =
    let
      d = std_exp a b
    in
      case c of
        Application_expression_6 e f -> Application_expression_9 <$> d e <*> d f
        Branch_expression_6 e f g h -> (\i -> Branch_expression_9 e i g) <$> d f <*> d h
        Char_expression_6 e -> Right (Char_expression_9 e)
        Function_expression_6 e f -> Function_expression_9 e <$> d f
        Int_expression_6 e -> Right (Int_expression_9 e)
        Let_expression_6 e f g h -> Let_expression_9 e f <$> d g <*> d h
        Match_expression_6 e f g -> Match_expression_9 e <$> d f <*> traverse (\(Case_6 h i) -> Case_1 h <$> d i) g
        Modular_expression_6 e -> Right (Modular_expression_9 e)
        Name_expression_6 (Maybename e i) f g ->
          case e of
            Nothing -> undefined
            Just h -> Name_expression_9 (Name h i) <$> traverse (std_type a) f <*> traverse (std_type a) g
        Op_expression_6 e f ->
          shunting_yard
            a
            (d, Application_expression_9, \g -> Name_expression_9 g Nothing [])
            b
            []
            e
            (
              first
                (\(Maybename g h) ->
                  case g of
                    Nothing -> undefined
                    Just i -> Name i h) <$>
              f)
        _ -> undefined
  std_expr ::
    (Location_0 -> Location_1) -> String -> (Map' Syntax_type, Map' Expression_6, Map' Op) -> Expression_0 -> Err Expression_9
  std_expr a f (b, c, d) e = type_syn' a f b e >>= \g -> std_exp a d (std_expr' c g)
  std_expr' :: Map' Expression_6 -> Expression_6 -> Expression_6
  std_expr' b d =
    let
      e = std_expr' b
    in
      case d of
        Application_expression_6 f g -> Application_expression_6 (e f) (e g)
        Branch_expression_6 c f g h -> Branch_expression_6 c (e f) g (e h)
        Case_expression_6 c f (g, h) i ->
          e
            (case e c of
              List_expression_6 j ->
                case j of
                  [] -> f
                  k : l -> synrepl h (List_expression_6 l) (synrepl g k i)
              _ -> undefined)
        Function_expression_6 f g -> Function_expression_6 f (e g)
        Let_expression_6 f g h i -> Let_expression_6 f g (e h) (e i)
        List_expression_6 f -> List_expression_6 (e <$> f)
        Match_expression_6 f g h -> Match_expression_6 f (e g) ((\(Case_6 c i) -> Case_6 c (e i)) <$> h)
        Op_expression_6 f g -> Op_expression_6 (e f) (second e <$> g)
        Syn_app_expression_6 f g ->
          case e f of
            Function_syn_expression_6 c j -> e (synrepl c (e g) j)
            _ -> undefined
        Syntax_expression_6 (Maybename f g) ->
          case f of
            Nothing -> undefined
            Just a -> std_expr'' b a (b ! g)
        _ -> d
  std_expr'' :: Map' Expression_6 -> Location_0 -> Expression_6 -> Expression_6
  std_expr'' b a d =
    let
      e = std_expr'' b a
    in
      case d of
        Application_expression_6 f g -> Application_expression_6 (e f) (e g)
        Branch_expression_6 c f g h -> Branch_expression_6 c (e f) g (e h)
        Case_expression_6 c f (g, h) i ->
          e
            (case e c of
              List_expression_6 j ->
                case j of
                  [] -> f
                  k : l -> synrepl h (List_expression_6 l) (synrepl g k i)
              _ -> undefined)
        Function_expression_6 f g -> Function_expression_6 f (e g)
        Let_expression_6 f g h i -> Let_expression_6 f g (e h) (e i)
        List_expression_6 f -> List_expression_6 (e <$> f)
        Match_expression_6 f g h -> Match_expression_6 f (e g) ((\(Case_6 c i) -> Case_6 c (e i)) <$> h)
        Name_expression_6 (Maybename _ f) g h -> Name_expression_6 (Maybename (Just a) f) g h
        Op_expression_6 f g -> Op_expression_6 (e f) (bimap (\(Maybename _ h) -> Maybename (Just a) h) e <$> g)
        Syn_app_expression_6 f g ->
          case e f of
            Function_syn_expression_6 c j -> e (synrepl c (e g) j)
            _ -> undefined
        Syntax_expression_6 (Maybename _ g) -> e (b ! g)
        _ -> d
  std_inst ::
    (
      (Location_0 -> Location_1) ->
      String ->
      (Map' Syntax_type, Map' Expression_6, Map' Op) ->
      (Name, ([Pat], Expression_0)) ->
      Err (Name, Expression_9))
  std_inst a y f (Name b z, (c, d)) =
    (
      (\e -> (Name b z, Prelude.foldr Function_expression_9 e c)) <$>
      std_expr a ("definition " ++ z ++ "{" ++ y ++ "}" ++ location' (a b)) f d)
  std_mthd :: (Location_0 -> Location_1) -> Method -> Err Method_9
  std_mthd a (Method b c d e) = Method_9 b c d <$> std_type a e
  std_stat ::
    (
      (Location_0 -> Location_1) ->
      String ->
      (Map' Syntax_type, Map' Expression_6, Map' Op) ->
      Location_0 ->
      Stat ->
      Maybe (Location_0, Expression_0) ->
      Err Struct_status)
  std_stat a x f b c d =
    case (c, d) of
      (Hidden, Nothing) -> Right Hidden_str
      (Restricted, Nothing) -> Left ("Restricted struct" ++ location (a b) ++ " should have a checker.")
      (Restricted, Just (g, e)) -> (\h -> Restricted_str (g, h)) <$> std_expr a ("checker for " ++ x ++ location' (a g)) f e
      (Standard, Nothing) -> Right Standard_str
      (_, Just _) -> Left ("Non-restricted struct" ++ location (a b) ++ " should not have a checker.")
  std_synexpr :: Syntax_expr -> Syntax_expr_1
  std_synexpr a =
    case a of
      Application_syntax b c -> Prelude.foldl Application_syntax_1 (std_synexpr b) (std_synexpr <$> c)
      Case_syntax b c d e -> Case_syntax_1 b (std_synexpr c) d (std_synexpr e)
      Name_syntax b -> Name_syntax_1 b
      Op_syntax b c -> Op_syntax_1 (std_synexpr b) (second std_synexpr <$> c)
      Syntax_syntax b -> Syntax_syntax_1 b
  std_syntax :: Syntax -> Syntax_1
  std_syntax (Syntax a b c d e) =
    Syntax_1 a b (Prelude.foldr Arrow_syntax d (snd <$> c)) (Prelude.foldr Function_syntax_1 (std_synexpr e) (fst <$> c))
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
  syn_type_transf :: Syntax_type -> Syntax_type'
  syn_type_transf a =
    case a of
      Arrow_syntax b c -> Arrow_syntax' (syn_type_transf b) (syn_type_transf c)
      Expr_syntax -> Expr_syntax'
      List_syntax b -> List_syntax' (syn_type_transf b)
  synrepl :: String -> Expression_6 -> Expression_6 -> Expression_6
  synrepl a b c =
    let
      f = synrepl a b
    in
      case c of
        Application_expression_6 d e -> Application_expression_6 (f d) (f e)
        Branch_expression_6 d e g h -> Branch_expression_6 d (f e) g (f h)
        Case_expression_6 d e (g, h) i ->
          Case_expression_6
            (f d)
            (f e)
            (g, h)
            (case elem a [g, h] of
              False -> f i
              True -> i)
        Function_expression_6 d e -> Function_expression_6 d (f e)
        Function_syn_expression_6 d e ->
          case d == a of
            False -> Function_syn_expression_6 d (f e)
            True -> c
        Let_expression_6 d e g h -> Let_expression_6 d e (f g) (f h)
        List_expression_6 d -> List_expression_6 (f <$> d)
        Match_expression_6 d e g -> Match_expression_6 d (f e) ((\(Case_6 h i) -> Case_6 h (f i)) <$> g)
        Op_expression_6 d e -> Op_expression_6 (f d) (second f <$> e)
        Syn_app_expression_6 d e -> Syn_app_expression_6 (f d) (f e)
        Syntax_expression_6 (Maybename _ d) ->
          case d == a of
            False -> c
            True -> b
        _ -> c
  type_err_syn :: String -> Syntax_type -> Syntax_type -> Err t
  type_err_syn a c d = Left ("Syntactic type mismatch between " ++ show_syn_0 c ++ " and " ++ show_syn_0 d ++ " in " ++ a)
  type_mism_syn :: (String, Location_1) -> Syntax_type' -> Syntax_type' -> Err ()
  type_mism_syn (a, b) c d =
    Left ("Syntactic type mismatch bewteen " ++ show_syn c ++ " and " ++ show_syn d ++ " in syntax " ++ a ++ location' b)
  type_syn :: (Location_0 -> Location_1) -> String -> Map' Syntax_type -> Expression_0 -> Err (Expression_6, Syntax_type)
  type_syn a m b c =
    case c of
      Application_expression_0 d e ->
        (
          (,) <$> type_syn a m b d <*> type_syn a m b e >>=
          \((f, g), (h, i)) ->
            case (g, i) of
              (Expr_syntax, Expr_syntax) -> Right (Application_expression_6 f h, Expr_syntax)
              (Arrow_syntax j k, l) ->
                case j == l of
                  False -> type_err_syn m j l
                  True -> Right (Syn_app_expression_6 f h, k)
              _ -> Left ("Syntactic type mismatch in " ++ m))
      Branch_expression_0 d e f g ->
        (\h -> \j -> (Branch_expression_6 d h f j, Expr_syntax)) <$> type_syn' a m b e <*> type_syn' a m b g
      Char_expression_0 d -> Right (Char_expression_6 d, Expr_syntax)
      Function_expression_0 d e -> (\f -> (Function_expression_6 d f, Expr_syntax)) <$> type_syn' a m b e
      Int_expression_0 d -> Right (Int_expression_6 d, Expr_syntax)
      Let_expression_0 d e f g ->
        (\h -> \j -> (Let_expression_6 d e h j, Expr_syntax)) <$> type_syn' a m b f <*> type_syn' a m b g
      List_expression_0 d ->
        (
          traverse (type_syn a m b) d >>=
          \e ->
            case e of
              [] -> undefined
              (f, g) : h -> (List_expression_6 (f : (fst <$> h)), List_syntax g) <$ type_syn_list m g (snd <$> h))
      Match_expression_0 d e f ->
        (\g -> \i -> (Match_expression_6 d g i, Expr_syntax)) <$> type_syn' a m b e <*> traverse (type_syn_case a m b) f
      Modular_expression_0 d -> Right (Modular_expression_6 d, Expr_syntax)
      Name_expression_0 (Name d g) e f -> Right (Name_expression_6 (Maybename (Just d) g) e f, Expr_syntax)
      Op_expression_0 d e ->
        (
          (\f -> \g -> (Op_expression_6 f g, Expr_syntax)) <$>
          type_syn' a m b d <*>
          traverse (\(Name f h, g) -> (,) (Maybename (Just f) h) <$> type_syn' a m b g) e)
      Syntax_expression_0 (Name d e) -> und_err e b "syntax" (a d) (\f -> Right (Syntax_expression_6 (Maybename (Just d) e), f))
  type_syn' :: (Location_0 -> Location_1) -> String -> Map' Syntax_type -> Expression_0 -> Err Expression_6
  type_syn' a b c d =
    (
      type_syn a b c d >>=
      \(e, f) ->
        case f of
          Expr_syntax -> Right e
          _ -> type_err_syn b Expr_syntax f)
  type_syn_case :: (Location_0 -> Location_1) -> String -> Map' Syntax_type -> Case_0 -> Err Case_6
  type_syn_case a b c (Case_0 d e) = (\f -> Case_6 d f) <$> type_syn' a b c e
  type_syn_list :: String -> Syntax_type -> [Syntax_type] -> Err ()
  type_syn_list a b c =
    case c of
      [] -> Right ()
      d : e ->
        case b == d of
          False -> type_err_syn a b d
          True -> type_syn_list a b e
  type_synexpr ::
    (
      (Location_0 -> Location_1) ->
      Map' Syntax_type' ->
      Syntax_type' ->
      Syntax_expr_2 ->
      Integer ->
      Err (Expression_6, [(Syntax_type', Syntax_type')], Integer))
  type_synexpr a b c d e =
    case d of
      Application_syntax_2 f g ->
        (
          type_synexpr a b (Arrow_syntax' (Var_syntax' (show e)) c) f (e + 1) >>=
          \(h, i, j) -> (\(k, l, m) -> (Syn_app_expression_6 h k, i ++ l, m)) <$> type_synexpr a b (Var_syntax' (show e)) g j)
      Case_syntax_2 (Name f g) h (i, j) k ->
        und_err
          g
          b
          "syntax"
          (a f)
          (\l ->
            (
              type_synexpr a b c h (e + 1) >>=
              \(m, n, o) ->
                (
                  (\(p, q, r) ->
                    (
                      Case_expression_6 (Syntax_expression_6 (Maybename Nothing g)) m (i, j) p,
                      (l, List_syntax' (Var_syntax' (show e))) : n ++ q,
                      r)) <$>
                  type_synexpr a (insert j (List_syntax' (Var_syntax' (show e))) (insert i (Var_syntax' (show e)) b)) c k o)))
      Function_syntax_2 f g ->
        (
          (\(h, i, j) ->
            (Function_syn_expression_6 f h, (c, Arrow_syntax' (Var_syntax' (show e)) (Var_syntax' (show (e + 1)))) : i, j)) <$>
          type_synexpr a (Data.Map.insert f (Var_syntax' (show e)) b) (Var_syntax' (show (e + 1))) g (e + 2))
      Name_syntax_2 f -> Right (Name_expression_6 (Maybename Nothing f) Nothing [], [(c, Expr_syntax')], e)
      Op_syntax_2 f g ->
        (
          type_synexpr a b Expr_syntax' f e >>=
          \(h, i, j) -> (\(k, l, m) -> (Op_expression_6 h k, i ++ l, m)) <$> type_synexpr_ops a b g j)
      Syntax_syntax_2 (Name f g) ->
        und_err g b "syntax" (a f) (\h -> Right (Syntax_expression_6 (Maybename Nothing g), [(c, h)], e))
  type_synexpr_ops ::
    (
      (Location_0 -> Location_1) ->
      Map' Syntax_type' ->
      [(String, Syntax_expr_2)] ->
      Integer ->
      Err ([(Maybename, Expression_6)], [(Syntax_type', Syntax_type')], Integer))
  type_synexpr_ops a b d e =
    case d of
      [] -> Right ([], [], e)
      (f, g) : h ->
        (
          type_synexpr a b Expr_syntax' g e >>=
          \(c, i, j) -> (\(k, l, m) -> ((Maybename Nothing f, c) : k, i ++ l, m)) <$> type_synexpr_ops a b h j)
  type_syntax_0 :: Map' (Syntax_type, Status) -> Syntax_2 -> Map' (Syntax_type, Status)
  type_syntax_0 a (Syntax_2 _ c d _) = ins_new c d a
  type_syntax_1 :: (Location_0 -> Location_1) -> Map' Syntax_type -> Syntax_2 -> Map' Expression_6 -> Err (Map' Expression_6)
  type_syntax_1 a b (Syntax_2 c d e f) g =
    (
      type_synexpr a (syn_type_transf <$> b) (syn_type_transf e) f 0 >>=
      \(h, i, _) -> Data.Map.insert d h g <$ solve_syn (d, a c) i)
  type_syntaxes ::
    (
      (Location_0 -> Location_1) ->
      [Syntax_2] ->
      (Map' Syntax_type, Map' Expression_6) ->
      Err (Map' (Syntax_type, Status), Map' Expression_6))
  type_syntaxes a b (c, d) =
    let
      e = type_syntaxes_0 (old c) b
    in
      (,) e <$> type_syntaxes_1 a (fst <$> e) b d
  type_syntaxes_0 :: Map' (Syntax_type, Status) -> [Syntax_2] -> Map' (Syntax_type, Status)
  type_syntaxes_0 a b = Prelude.foldl type_syntax_0 a b
  type_syntaxes_1 ::
    (Location_0 -> Location_1) -> Map' Syntax_type -> [Syntax_2] -> Map' Expression_6 -> Err (Map' Expression_6)
  type_syntaxes_1 a b c d =
    case c of
      [] -> Right d
      e : f -> type_syntax_1 a b e d >>= type_syntaxes_1 a b f
  und_err :: String -> Map' t -> String -> Location_1 -> (t -> Err u) -> Err u
  und_err a b c d f =
    case Data.Map.lookup a b of
      Just e -> f e
      Nothing -> Left ("Undefined " ++ c ++ " " ++ a ++ location' d)
--------------------------------------------------------------------------------------------------------------------------------