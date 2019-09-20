--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Standard where
  import Data.Bifunctor
  import Data.Map
  import Data.Set
  import Errors
  import Tokenise
  import Tree
  data Alg_pat_7 =
    Application_alg_pat_7 Location_0 String [Alg_pat_7] |
    Blank_alg_pat_7 |
    Char_alg_pat_7 Char |
    Int_alg_pat_7 Integer |
    Modular_alg_pat_7 Modular |
    Name_alg_pat_7 Name
      deriving Show
  data Case_1 = Case_1 Alg_pat_7 Expression_9 deriving Show
  data Case_6 = Case_6 Alg_pat_7 Expression_6 deriving Show
  data Class_7 = Class_7 Name (Name, Kind_0) [Constraint_0] [Method_9] deriving Show
  data Def_1 =
    Basic_def_1 Name Kinds_constraints Type_8 Expression_9 |
    Instance_1 Location_0 Name (Name, [Pattern_1]) [Constraint_0] [(Name, Expression_9)]
      deriving Show
  data Data_6 = Data_6 Location_0 String [(Name, Kind_0)] Data_br_6 deriving Show
  data Data_br_6 =
    Algebraic_data_6 [Form_6] |
    Branching_data_6 Name (Data_br_6, Name, Data_br_6) |
    Struct_data_6 Location_0 Status String [(Name, Type_8)]
      deriving Show
  data Expression_6 =
    Application_expression_6 Expression_6 Expression_6 |
    Branch_expression_6 Name Expression_6 Pattern_1 Expression_6 |
    Case_expression_6 Expression_6 Expression_6 (String, String) Expression_6 |
    Cat_expression_6 Expression_6 Expression_6 |
    Char_expression_6 Char |
    Function_expression_6 Pat_2 Expression_6 |
    Function_syn_expression_6 String Expression_6 |
    Int_expression_6 Integer |
    List_expression_6 [Expression_6] |
    Match_expression_6 Expression_6 [Case_6] |
    Modular_expression_6 Modular |
    Name_expression_6 Name |
    Op_expression_6 Expression_6 [(Name, Expression_6)] |
    Syn_app_expression_6 Expression_6 Expression_6 |
    Syntax_expression_6 Name
      deriving Show
  data Expression_9 =
    Application_expression_9 Expression_9 Expression_9 |
    Branch_expression_9 Name Expression_9 Pattern_1 Expression_9 |
    Char_expression_9 Char |
    Function_expression_9 Pat_2 Expression_9 |
    Int_expression_9 Integer |
    Match_expression_9 Expression_9 [Case_1] |
    Modular_expression_9 Modular |
    Name_expression_9 Name
      deriving Show
  data Form_6 = Form_6 Location_0 Status String [Type_8] deriving Show
  type Locations = Map' Location_2
  type Map' t = Map String t
  data Method_9 = Method_9 Name Kinds_constraints Type_8 deriving Show
  data Op' = Op' Location_0 Op deriving Show
  data Pat_2 = Application_pat_2 Name [Pat_2] | Blank_pat_2 | Name_pat_2 Name deriving Show
  data Syneq = Syneq Syntax_type' Syntax_type' deriving Show
  data Syntax_2 = Syntax_2 Location_0 String [String] [(String, Syntax_type)] Syntax_type Syntax_expr_2 deriving Show
  data Syntax_3 =
    Application_syntax_3 Syntax_3 Syntax_3 |
    Case_syntax_3 Syntax_3 Syntax_3 (String, String) Syntax_3 |
    Cat_syntax_3 Syntax_3 Syntax_3 |
    Function_syntax_3 String Syntax_3 |
    Emp_syntax_3 |
    Name_syntax_3 String |
    Synapp_syntax_3 Syntax_3 Syntax_3 |
    Syntax_syntax_3 String
      deriving Show
  data Syntax_4 = Syntax_4 Location_0 String [String] [(String, Syntax_type')] Syntax_type' Syntax_expr_2 deriving Show
  data Syntax_expr_2 =
    Application_syntax_2 Syntax_expr_2 Syntax_expr_2 |
    Case_syntax_2 Location_0 Name Syntax_expr_2 (String, String) Syntax_expr_2 |
    Cat_syntax_2 Syntax_expr_2 Syntax_expr_2 |
    Emp_syntax_2 Syntax_type |
    Name_syntax_2 String |
    Syntax_syntax_2 Name
      deriving Show
  data Tree_2 = Tree_2 [Name] [Name] [Data_6] [Class_7] [Def_1] deriving Show
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
  eith :: Either t u -> Either t u -> Either t u
  eith x y =
    case x of
      Left _ -> y
      Right _ -> x
  gather_ops :: Map' (Op, Status) -> [Opdecl_0] -> (Map' (Op, Status), [Name])
  gather_ops b c =
    case c of
      [] -> (b, [])
      Opdecl_0 d e g h i : j -> second ((:) (Name d e)) (gather_ops (ins_new e (Op h i g) b) j)
  ins_new :: Ord t => t -> u -> Map t (u, Status) -> Map t (u, Status)
  ins_new a b = Data.Map.insert a (b, New)
  int_to_nat :: Integer -> Type_5
  int_to_nat a =
    case a of
      0 -> Name_type_5 (Name (Location_0 0 0) "Zero")
      _ -> Application_type_5 (Name_type_5 (Name (Location_0 0 0) "Next")) (int_to_nat (a - 1))
  location_err :: String -> Location_2 -> Location_0 -> Error
  location_err a c d =
    Error
      (
        "Conflicting " ++
        a ++
        case c of
          File (Location_1 e f) -> " at " ++ e ++ location f ++ " and "
          Language -> " in the language and at ")
      (location' d)
{-
  type_synexpr ::
    Set String -> String -> Location_0 -> Map' ([String], Syntax_type') -> Syntax_expr_2 -> Err (Syntax_3, Syntax_type')
  type_synexpr r b0 b1 c d =
    case d of
      Application_syntax_2 e f ->
        (
          (,) <$> type_synexpr r b0 b1 c e <*> type_synexpr r b0 b1 c f >>=
          \((g, h), (i, j)) ->
            case (h, j) of
              (Expr_syntax', Expr_syntax') -> Right (Application_syntax_3 g i, Expr_syntax')
              (Arrow_syntax' k l, m) ->
                case k == m of
                  False -> Left (Error ("Syntactic type mismatch in " ++ b0 ++ " at ") (location' b1))
                  True -> Right (Synapp_syntax_3 g i, l)
              _ -> Left (Error ("Syntactic type mismatch in " ++ b0 ++ " at ") (location' b1)))
      Case_syntax_2 q (Name e f) g (h, i) j ->
        und_err
          f
          c
          "syntax"
          e
          (\(s, k) ->
            case s of
              [] ->
                case k of
                  List_syntax' l ->
                    (
                      (
                        (,) <$>
                        type_synexpr r b0 b1 c g <*>
                        type_synexpr
                          r
                          b0
                          b1
                          (Data.Map.union
                            c
                            (Data.Map.fromList
                              (
                                (case h of
                                  Blank_pattern -> []
                                  Name_pattern h' -> [(h', ([], l))]) ++
                                case i of
                                  Blank_pattern -> []
                                  Name_pattern i' -> [(i', ([], k))])))
                          j) >>=
                      \((m, n), (o, p)) ->
                        case n == p of
                          False -> Left (Error ("Syntactic type mismatch in " ++ b0 ++ " at ") (location' b1))
                          True -> Right (Case_syntax_3 (Syntax_syntax_3 f) m (h, i) o, n))
                  _ -> Left (Error ("Syntactic type mismatch in " ++ b0 ++ " at ") (location' b1))
              _ -> Left (Error ("Case over a polymorphic expression in " ++ b0 ++ " at ") (location' q)))
      Cat_syntax_2 e f ->
        (
          type_synexpr r b0 b1 c e >>=
          \(g, h) ->
            (
              type_synexpr r b0 b1 c f >>=
              \(i, j) ->
                case j of
                  List_syntax' k ->
                    case h == k of
                      False -> Left (Error ("Syntactic type mismatch in " ++ b0 ++ " at ") (location' b1))
                      True -> Right (Cat_syntax_3 g i, j)
                  _ -> Left (Error ("Syntactic type mismatch in " ++ b0 ++ " at ") (location' b1))))
      Emp_syntax_2 e -> (\f -> (Emp_syntax_3, List_syntax' f)) <$> syntype_check r e
      Name_syntax_2 e -> Right (Name_syntax_3 e, Expr_syntax')
      Syntax_syntax_2 e h -> type_syn_name e c h b1 (return Syntax_syntax_3) r
-}
  make_syn_eqs :: Map' ([String], Syntax_type') -> Syntax_expr_2 -> Syntax_type' -> Err (Syntax_3, [Syneq])
  make_syn_eqs a b c = _
{-
  type_syn ::
    (
      String ->
      Location_0 ->
      (Set String, Map' Op, Map' ([String], Syntax_type')) ->
      Expression_0 ->
      Err (Expression_6, Syntax_type'))
  type_syn m0 m1 (u, k, b) c =
    case c of
      Application_expression_0 d e ->
        (
          (,) <$> type_syn m0 m1 (u, k, b) d <*> type_syn m0 m1 (u, k, b) e >>=
          \((f, g), (h, i)) ->
            case (g, i) of
              (Expr_syntax', Expr_syntax') -> Right (Application_expression_6 f h, Expr_syntax')
              (Arrow_syntax' j u2, l) ->
                case j == l of
                  False -> Left (Error ("Syntactic type mismatch in " ++ m0 ++ " at ") (location' m1))
                  True -> Right (Syn_app_expression_6 f h, u2)
              _ -> Left (Error ("Syntactic type mismatch in " ++ m0 ++ " at ") (location' m1)))
      Branch_expression_0 d e f g ->
        (\h -> \j -> (Branch_expression_6 d h f j, Expr_syntax')) <$> type_syn' m0 m1 (u, k, b) e <*> type_syn' m0 m1 (u, k, b) g
      Char_expression_0 d -> Right (Char_expression_6 d, Expr_syntax')
      Function_expression_0 d e ->
        (\g -> \f -> (Function_expression_6 g f, Expr_syntax')) <$> std_pat (k, u) d <*> type_syn' m0 m1 (u, k, b) e
      Int_expression_0 d -> Right (Int_expression_6 d, Expr_syntax')
      Let_expression_0 d e -> (\x -> (x, Expr_syntax')) <$> std_let m0 m1 (u, k, b) d e
      List_expression_0 d ->
        (
          traverse (type_syn m0 m1 (u, k, b)) d >>=
          \e ->
            case e of
              [] -> undefined
              (f, g) : h -> (List_expression_6 (f : (fst <$> h)), List_syntax' g) <$ type_syn_list m0 m1 g (snd <$> h))
      Match_expression_0 e f ->
        (
          (\g -> \i -> (Match_expression_6 g i, Expr_syntax')) <$>
          type_syn' m0 m1 (u, k, b) e <*> traverse (type_syn_case m0 m1 (u, k, b)) f)
      Modular_expression_0 d -> Right (Modular_expression_6 d, Expr_syntax')
      Name_expression_0 d -> Right (Name_expression_6 d, Expr_syntax')
      Op_expression_0 d e ->
        (
          (\f -> \g -> (Op_expression_6 f g, Expr_syntax')) <$>
          type_syn' m0 m1 (u, k, b) d <*>
          traverse (\(f, g) -> (,) f <$> type_syn' m0 m1 (u, k, b) g) e)
      Syntax_expression_0 d g -> type_syn_name d b g m1 (\j -> \k' -> Syntax_expression_6 (Name j k')) Data.Set.empty
-}
  make_syn_eqs' :: (Map' Op, Map' ([String], Syntax_type')) -> Expression_0 -> Syntax_type' -> Err (Expression_6, [Syneq])
  make_syn_eqs' (a, b, c) d e =
    case d of
      _ -> _
  naming_list :: (String -> t -> u -> Err (u, v)) -> String -> [t] -> u -> Err (u, [v])
  naming_list a h b c =
    case b of
      [] -> Right (c, [])
      d : e -> a h d c >>= \(f, g) -> second ((:) g) <$> naming_list a h e f
  naming_name :: String -> Name -> Locations -> Err (Locations, String)
  naming_name f (Name a c) d =
    bimap (\location_0 -> location_err ("definitions of " ++ c) location_0 a) (flip (,) c) (add d c (Library (Location_1 f a)))
  naming_names :: String -> [(Name, t)] -> Locations -> Err (Locations, [(String, t)])
  naming_names a b c =
    case b of
      [] -> Right (c, [])
      (d, e) : f -> naming_name a d c >>= \(g, h) -> second ((:) (h, e)) <$> naming_names a f g
  naming_names' :: String -> [Name] -> Locations -> Err (Locations, [String])
  naming_names' a b c = second (fmap fst) (nameing_names a ((\d -> (d, ())) <$> b) c)
  naming_pattern :: String -> Pattern_1 -> Locations -> Err (Locations, Pattern_0)
  naming_pattern f (Pattern_1 a c) d =
    case c of
      Blank_pattern -> Right (d, Blank_pattern)
      Name_pattern e -> second Name_pattern <$> naming_name f (Name a e) d
  naming_patterns :: String -> [Pattern_1] -> Locations -> Err (Locations, [Pattern_0])
  naming_patterns = naming_list naming_pattern
  naming_patterns_2 :: String -> [(Pattern_1, t)] -> Locations -> Err (Locations, [(Pattern_0, t)])
  naming_patterns_2 a b c =
    case b of
      [] -> Right (c, [])
      (d, e) : f -> naming_pattern a d c >>= \(g, h) -> second ((:) (h, e)) <$> naming_patterns_2 a f g
  naming_synt ::  String -> Locations -> [Name] -> Err (Locations, [String])
  naming_synt a b c =
    case c of
      [] -> Right (b, [])
      d : e -> naming_name a d b >>= \(f, g) -> second ((:) g) <$> naming_synt a f e
  naming_syntax_0 :: String -> Syntax -> Locations -> Err Locations
  naming_syntax_0 a (Syntax c d _ _ _ _) b = fst <$> naming_name a (Name c d) b
  naming_syntax_1 :: String -> Locations -> Syntax -> Err Syntax_2
  naming_syntax_1 a b (Syntax c d j e f g) =
    naming_tvars a b j >>= \(k, l) -> naming_patterns_2 a e k >>= \(i, h) -> Syntax_2 c d l h f <$> naming_syntax_expr a i g
  naming_syntax_expr :: String -> Locations -> Syntax_expr -> Err Syntax_expr_2
  naming_syntax_expr a b c =
    case c of
      Application_syntax d e -> Application_syntax_2 <$> naming_syntax_expr a b d <*> naming_syntax_expr a b e
      Case_syntax q d e (f, g) h ->
        (
          (\i -> \(k, l) -> Case_syntax_2 q d i k l) <$>
          naming_syntax_expr a b e <*>
          (naming_names' a [f, g] b >>= \(i, [j, k]) -> (,) (j, k) <$> naming_syntax_expr a i h))
      Cat_syntax d e -> Cat_syntax_2 <$> naming_syntax_expr a b d <*> naming_syntax_expr a b e
      Emp_syntax d -> Right (Emp_syntax_2 d)
      Name_syntax d -> Right (Name_syntax_2 d)
      Syntax_syntax d -> Right (Syntax_syntax_2 d)
  naming_syntaxes :: String -> (Locations, [Syntax]) -> Err (Locations, [Syntax_2])
  naming_syntaxes a (b, c) = naming_syntaxes_0 a c b >>= \d -> (,) d <$> naming_syntaxes_1 a d c
  naming_syntaxes_0 :: String -> [Syntax] -> Locations -> Err Locations
  naming_syntaxes_0 a c b =
    case c of
      [] -> Right b
      d : e -> naming_syntax_0 a d b >>= \f -> naming_syntaxes_0 a e f
  naming_syntaxes_1 :: String -> Locations -> [Syntax] -> Err [Syntax_2]
  naming_syntaxes_1 a b c =
    case c of
      [] -> Right []
      d : e -> (:) <$> naming_syntax_1 a b d <*> naming_syntaxes_1 a b e
  naming_tvars :: String -> Locations -> [Name] -> Err (Locations, [String])
  naming_tvars a b c =
    case c of
      [] -> Right (b, [])
      d : f -> naming_name a d b >>= \(g, h) -> second ((:) h) <$> naming_tvars a g f
  old :: Map' t -> Map' (t, Status)
  old = (<$>) (flip (,) Old)
  pop :: (Name -> t -> t -> t) -> [(Op', t)] -> t -> Op' -> [(Op', t)]
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
      ((Name -> t -> t -> t) -> [(Op', t)] -> t -> u) ->
      (Name -> t -> t -> t) ->
      [(Op', t)] ->
      Location_0 ->
      String ->
      t ->
      t ->
      u)
  pop' h f x' l name expr' expr = h f x' (f (Name l name) expr' expr)
  pop_all :: (Name -> t -> t -> t) -> [(Op', t)] -> t -> t
  pop_all f x expr =
    case x of
      [] -> expr
      (Op' l (Op _ _ name), expr') : x' -> pop' pop_all f x' l name expr' expr
  process_syntax ::
    (
      String ->
      [Syntax] ->
      (Locations, Map' ([String], Syntax_type'), Map' Syntax_3) ->
      Err (Locations, Map' (([String], Syntax_type'), Status), Map' Syntax_3))
  process_syntax a b (c, d, e) = naming_syntaxes a (c, b) >>= \(f, g) -> (\(h, i) -> (f, h, i)) <$> type_syntaxes g (d, e)
  rem_old :: Map' (t, Status) -> Map' t
  rem_old a = fst <$> Data.Map.filter (\(_, b) -> b == New) a
  show_syn_0 :: Syntax_type' -> String
  show_syn_0 a =
    case a of
      Arrow_syntax' b c -> show_syn_1 b ++ " -> " ++ show_syn_0 c
      Expr_syntax' -> "Expr"
      List_syntax' b -> "[" ++ show_syn_0 b ++ "]"
      Name_st' b -> b
  show_syn_1 :: Syntax_type' -> String
  show_syn_1 a =
    let
      b = show_syn_0 a
    in
      case a of
        Arrow_syntax' _ _ -> "(" ++ b ++ ")"
        _ -> b
  shunting_yard :: String -> (t -> Err u, Name -> u -> u -> u) -> Map' Op -> [(Op', u)] -> t -> [(Name, t)] -> Err u
  shunting_yard k (f, g) ops x expr y =
    (
      f expr >>=
      \expr'' ->
        case y of
          [] -> Right (pop_all g x expr'')
          (Name l op, expr') : y' ->
            und_err op ops k l (\op' -> shunting_yard k (f, g) ops (pop g x expr'' (Op' l op')) expr' y'))
  solve_syn_eqs :: Set String -> [Syneq] -> Maybe (Syntax_type', Syntax_type')
  solve_syn_eqs d a =
    case a of
      [] -> Nothing
      b : c ->
        case b of
          Syneq (Arrow_syntax' e f) (Arrow_syntax' g h) -> solve_syn_eqs d (Syneq e g : Syneq f h : c)
          Syneq Expr_syntax' Expr_syntax' -> solve_syn_eqs d c
          Syneq (List_syntax' e) (List_syntax' f) -> solve_syn_eqs d (Syneq e f : c)
          Syneq (Name_st' e) (Name_st' f) ->
            case e == f of
              False ->
                case (Data.Set.member e d, Data.Set.member f d) of
                  (False, _) -> solve_syn_eqs_repl d e (Name_st' f) c
                  (_, False) -> solve_syn_eqs_repl d f (Name_st' e) c
                  (True, True) -> Just (Name_st' e, Name_st' f)
              True -> solve_syn_eqs d c
          Syneq (Name_st' e) f ->
            case Data.Set.member e d of
              False -> solve_syn_eqs_repl d e f c
              True -> Just (Name_st' e, f)
          Syneq e (Name_st' f) ->
            case Data.Set.member f d of
              False -> solve_syn_eqs_repl d f e c
              True -> Just (e, Name_st' f)
          Syneq e f -> Just (e, f)
  solve_syn_eqs_repl :: Set String -> String -> Syntax_type' -> [Syneq] -> Maybe (Syntax_type', Syntax_type')
  solve_syn_eqs_repl a b c d = solve_syn_eqs a ((\(Syneq e f) -> Syneq ( e) ( f)) <$> d)
  standard_1 ::
    (
      String ->
      (Locations, Map' ([String], Syntax_type'), Map' Syntax_3, (Map' Op, Map' Op), Set String) ->
      Tree_0 ->
      Err ((Locations, Map' ([String], Syntax_type'), Map' Syntax_3, (Map' Op, Map' Op), Set String), Tree_2))
  standard_1 x3 (w, n, m, (f0, f), z) (Tree_0 t' e y a b c) =
    let
      (i0, j0) = gather_ops (old f0) t'
      (i, j) = gather_ops (old f) e
    in
      (
        (,) <$> process_syntax x3 y (w, n, m) <*> std_dats f0 (z, a) >>=
        \((o, p, q), (z', a')) ->
          (
            (\y2 -> \k -> ((o, rem_old p, q, (rem_old i0, rem_old i), z'), Tree_2 j0 j a' y2 k)) <$>
            traverse (std_cls (fst <$> i0)) b <*> 
            standard_defs (z', fst <$> p, q, (fst <$> i0, fst <$> i)) c))
  standard_arguments ::
    (
      String ->
      Location_0 ->
      (Set String, Map' ([String], Syntax_type'), Map' Syntax_3, (Map' Op, Map' Op)) ->
      [(Pat, Type_7)] ->
      Type_7 ->
      Expression_0 ->
      Err (Type_8, Expression_9))
  standard_arguments y0 y1 (w, t, u, (m0, m)) b c d =
    case b of
      [] -> (,) <$> std_type m0 c <*> std_expr (Just (Name y1 y0)) (w, t, u, m) d
      (e, Type_7 l f) : g ->
        (
          (\n -> \u' -> \(Type_8 i j, k) ->
            (
              Type_8 i (Application_type_5 (Application_type_5 (Name_type_5 (Name l "Arrow")) u') j),
              Function_expression_9 n k)) <$>
          std_pat (m, w) e <*>
          std_type' m0 f <*>
          standard_arguments y0 y1 (w, t, u, (m0, m)) g c d)
  standard_def :: (Set String, Map' ([String], Syntax_type'), Map' Syntax_3, (Map' Op, Map' Op)) -> Def_0 -> Err Def_1
  standard_def (j, k, l, (m, n)) a =
    case a of
      Basic_def_0 (Name b y) c d e f -> uncurry (Basic_def_1 (Name b y) c) <$> standard_arguments y b (j, k, l, (m, n)) d e f
      Instance_def_0 b c d g e ->
        (
          (case d of
            Left m' -> Right m'
            Right (m0, Name u t, m1) -> und_err t m "type operator" u (\(Op _ _ g0) -> Right (Name u g0, [m0, m1]))) >>=
          \(Name u y, w) -> Instance_1 b c (Name u y, w) g <$> traverse (std_inst y (j, k, l, n)) e)
  standard_defs :: (Set String, Map' ([String], Syntax_type'), Map' Syntax_3, (Map' Op, Map' Op)) -> [Def_0] -> Err [Def_1]
  standard_defs b = traverse (standard_def b)
  std_apat :: (Set String, Map' Op) -> Alg_pat -> Err Alg_pat_7
  std_apat (i, b) c =
    case c of
      Application_alg_pat d e f -> Application_alg_pat_7 d e <$> traverse (std_apat (i, b)) f
      Blank_alg_pat -> Right (Blank_alg_pat_7)
      Char_alg_pat d -> Right (Char_alg_pat_7 d)
      Int_alg_pat d -> Right (Int_alg_pat_7 d)
      Modular_alg_pat d -> Right (Modular_alg_pat_7 d)
      Name_alg_pat (Name d e) ->
        Right
          (case Data.Set.member e i of
            False -> Name_alg_pat_7 (Name d e)
            True -> Application_alg_pat_7 d e [])
      Op_alg_pat d e ->
        shunting_yard "operator" (std_apat (i, b), \(Name f t) -> \g -> \h -> Application_alg_pat_7 f t [g, h]) b [] d e
  std_br :: Map' Op -> (Set String, Data_br_0) -> Err (Set String, Data_br_6)
  std_br b (c, d) =
    case d of
      Algebraic_data_0 e ->
        (
          (\f -> (Prelude.foldl (\g -> \(Form_0 _ _ h _) -> Data.Set.insert h g) c e, Algebraic_data_6 f)) <$>
          traverse (\(Form_0 f g h i) -> Form_6 f g h <$> traverse (std_type b) i) e)
      Branching_data_0 e (f, g, h) ->
        std_br b (c, f) >>= \(i, j) -> second (\k -> Branching_data_6 e (j, g, k)) <$> std_br b (i, h)
      Struct_data_0 e f g h -> (\i -> (Data.Set.insert g c, Struct_data_6 e f g i)) <$> traverse (traverse (std_type b)) h
  std_cls :: Map' Op -> Class_0 -> Err Class_7
  std_cls f (Class_0 a b c d) = Class_7 a b c <$> traverse (std_mthd f) d
  std_dat :: Map' Op -> (Set String, Data_0) -> Err (Set String, Data_6)
  std_dat b (c, Data_0 d e f g) = second (Data_6 d e f) <$> std_br b (c, g)
  std_dats :: Map' Op -> (Set String, [Data_0]) -> Err (Set String, [Data_6])
  std_dats w (b, c) =
    case c of
      [] -> Right (b, [])
      d : e -> std_dat w (b, d) >>= \(f, g) -> second ((:) g) <$> std_dats w (f, e)
  std_exp :: Map' Op -> Expression_6 -> Err Expression_9
  std_exp b c =
    let
      d = std_exp b
    in
      case c of
        Application_expression_6 e f -> Application_expression_9 <$> d e <*> d f
        Branch_expression_6 e f g h -> (\i -> Branch_expression_9 e i g) <$> d f <*> d h
        Char_expression_6 e -> Right (Char_expression_9 e)
        Function_expression_6 e f -> Function_expression_9 e <$> d f
        Int_expression_6 e -> Right (Int_expression_9 e)
        Match_expression_6 f g -> Match_expression_9 <$> d f <*> traverse (\(Case_6 h i) -> Case_1 h <$> d i) g
        Modular_expression_6 e -> Right (Modular_expression_9 e)
        Name_expression_6 h -> Right (Name_expression_9 h)
        Op_expression_6 e f ->
          shunting_yard
            "operator"
            (d, \g -> \h -> Application_expression_9 (Application_expression_9 (Name_expression_9 g) h))
            b
            []
            e
            f
        _ -> undefined
  std_expr ::
    Maybe Name -> (Set String, Map' ([String], Syntax_type'), Map' Syntax_3, Map' Op) -> Expression_0 -> Err Expression_9
  std_expr f0 (t, b, c, d) e = type_syn' f0 (t, d, b) e >>= \g -> std_expr' c g >>= std_exp d
  std_expr' :: Map' Syntax_3 -> Expression_6 -> Err Expression_6
  std_expr' b d =
    let
      e = std_expr' b
    in
      case d of
        Application_expression_6 f g -> Application_expression_6 <$> e f <*> e g
        Branch_expression_6 c f g h -> (\a -> Branch_expression_6 c a g) <$> e f <*> e h
        Case_expression_6 c f (g, h) i ->
          (
            e c >>=
            \(List_expression_6 j) ->
              e
                (case j of
                  [] -> f
                  k : l -> synrepl h (List_expression_6 l) (synrepl g k i)))
        Cat_expression_6 a c -> (\f -> \(List_expression_6 g) -> List_expression_6 (f : g)) <$> e a <*> e c
        Function_expression_6 a c -> Function_expression_6 a <$> e c
        List_expression_6 f -> List_expression_6 <$> traverse e f
        Match_expression_6 g h -> Match_expression_6 <$> e g <*> traverse (\(Case_6 c i) -> Case_6 c <$> e i) h
        Op_expression_6 f g -> Op_expression_6 <$> e f <*> traverse (traverse e) g
        Syn_app_expression_6 f g ->
          (
            e f >>=
            \(Function_syn_expression_6 c j) -> e g >>= \a -> e (synrepl c a j))
        Syntax_expression_6 (Name a g) -> e (std_expr'' a (b ! g))
        _ -> Right d
  std_expr'' :: Location_0 -> Syntax_3 -> Expression_6
  std_expr'' a d =
    let
      e = std_expr'' a
    in
      case d of
        Application_syntax_3 f g -> Application_expression_6 (e f) (e g)
        Case_syntax_3 c f (g, h) i -> Case_expression_6 (e c) (e f) (g, h) (e i)
        Cat_syntax_3 b c -> Cat_expression_6 (e b) (e c)
        Emp_syntax_3 -> List_expression_6 []
        Function_syntax_3 f g -> Function_syn_expression_6 f (e g)
        Name_syntax_3 f -> Name_expression_6 (Name a f)
        Synapp_syntax_3 f g -> Syn_app_expression_6 (e f) (e g)
        Syntax_syntax_3 g -> Syntax_expression_6 (Name a g)
  std_inst ::
    (
      String ->
      (Set String, Map' ([String], Syntax_type'), Map' Syntax_3, Map' Op) ->
      (Either (Name, [Pat]) ((Pat, [(Name, Pat)]), [Pat]), Expression_0) ->
      Err (Name, Expression_9))
  std_inst b (c, d, e, f) (g, h) =
    (
      (case g of
        Left (i, j) -> (,) i <$> traverse (std_pat (f, c)) j
        Right ((i, j), x) ->
          (
            (\(Application_pat_2 l m) -> \w -> (l, m ++ w)) <$>
            shunting_yard "operator" (std_pat (f, c), \k -> \l -> \m -> Application_pat_2 k [l, m]) f [] i j <*>
            traverse (std_pat (f, c)) x)) >>=
      \(Name i j, k) ->
        (\l -> (Name i j, Prelude.foldr Function_expression_9 l k)) <$> std_expr (Just (Name i (j ++ " " ++ b))) (c, d, e, f) h)
{-
  std_let ::
    (
      String ->
      Location_0 ->
      (Set String, Map' Op, Map' ([String], Syntax_type')) ->
      [(Either (Name, [Pat]) (Pat, [(Name, Pat)]), Expression_0)] ->
      Expression_0 ->
      Err Expression_6)
  std_let m0 m1 (u, k, b) n g =
    case n of
      [] -> type_syn' m0 m1 (u, k, b) g
      (w', f) : n' ->
        case w' of
          Left (Name l d, e) ->
            (
              (\i -> \h -> \j ->
                let
                  w t = Application_expression_6 (Function_expression_6 t j)
                in
                  case Data.Set.member d u of
                    False -> w (Name_pat_2 (Name l d)) (Prelude.foldr Function_expression_6 h i)
                    True -> w (Application_pat_2 (Name l d) i) h) <$>
              traverse (std_pat (k, u)) e <*>
              type_syn' m0 m1 (u, k, b) f <*>
              std_let m0 m1 (u, k, b) n' g)
          Right (l, d) ->
            (
              (\e -> \h -> \j -> Application_expression_6 (Function_expression_6 e j) h) <$>
              shunting_yard "operator" (std_pat (k, u), \k1 -> \k2 -> \k3 -> Application_pat_2 k1 [k2, k3]) k [] l d <*>
              type_syn' m0 m1 (u, k, b) f <*>
              std_let m0 m1 (u, k, b) n' g)
-}
  std_mthd :: Map' Op -> Method -> Err Method_9
  std_mthd d (Method b c e) = Method_9 b c <$> std_type d e
  std_pat :: (Map' Op, Set String) -> Pat -> Err Pat_2
  std_pat (b, i) c =
    case c of
      Application_pat d e -> Application_pat_2 d <$> traverse (std_pat (b, i)) e
      Blank_pat -> Right (Blank_pat_2)
      Name_pat (Name d e) ->
        Right
          (case Data.Set.member e i of
            False -> Name_pat_2 (Name d e)
            True -> Application_pat_2 (Name d e) [])
      Op_pat d e -> shunting_yard "operator" (std_pat (b, i), \f -> \g -> \h -> Application_pat_2 f [g, h]) b [] d e
  std_type :: Map' Op -> Type_7 -> Err Type_8
  std_type d (Type_7 a b) = Type_8 a <$> std_type' d b
  std_type' :: Map' Op -> Type_0 -> Err Type_5
  std_type' g b =
    case b of
      Application_type_0 c d -> Prelude.foldl Application_type_5 <$> std_type' g c <*> traverse (std_type' g) d
      Name_type_0 c -> Right (Name_type_5 c)
      Nat_type_0 a -> Right (int_to_nat a)
      Op_type_0 a c ->
        shunting_yard
          "type operator"
          (std_type' g, \d -> \f -> Application_type_5 (Application_type_5 (Name_type_5 d) f))
          g
          []
          a
          c
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
          case a == d of
            False -> Function_syn_expression_6 d (f e)
            True -> c
        List_expression_6 d -> List_expression_6 (f <$> d)
        Match_expression_6 e g -> Match_expression_6 (f e) ((\(Case_6 h i) -> Case_6 h (f i)) <$> g)
        Op_expression_6 d e -> Op_expression_6 (f d) (second f <$> e)
        Syn_app_expression_6 d e -> Syn_app_expression_6 (f d) (f e)
        Syntax_expression_6 (Name _ d) ->
          case d == a of
            False -> c
            True -> b
        _ -> c
  syntype_check :: Set String -> Syntax_type -> Err Syntax_type'
  syntype_check a b =
    case b of
      Arrow_syntax c d -> Arrow_syntax' <$> syntype_check a c <*> syntype_check a d
      Expr_syntax -> Right Expr_syntax'
      List_syntax c -> List_syntax' <$> syntype_check a c
      Name_st (Name c d) ->
        case Data.Set.member d a of
          False -> Left (Undefined "syntactic type variable" d c)
          True -> Right (Name_st' d)
{-
  syntyperep :: Map' Syntax_type' -> Syntax_type' -> Syntax_type'
  syntyperep a b =
    case b of
      Arrow_syntax' c d -> Arrow_syntax' (syntyperep a c) (syntyperep a d)
      Expr_syntax' -> Expr_syntax'
      List_syntax' c -> List_syntax' (syntyperep a c)
      Name_st' c ->
        case Data.Map.lookup c a of
          Nothing -> b
          Just d -> d
-}
  syntyperep' :: String -> Syntax_type' -> Syntax_type' -> Syntax_type'
  syntyperep' x y b =
    case b of
      Arrow_syntax' c d -> Arrow_syntax' (syntyperep' x y c) (syntyperep' x y d)
      Expr_syntax' -> Expr_syntax'
      List_syntax' c -> List_syntax' (syntyperep' x y c)
      Name_st' c ->
        case x == c of
          False -> b
          True -> y
{-
  type_err_syn :: String -> Syntax_type' -> Syntax_type' -> Err t
  type_err_syn a c d = Left ("Syntactic type mismatch between " ++ show_syn_0 c ++ " and " ++ show_syn_0 d ++ " in " ++ a)
-}
{-
  type_syn ::
    (
      String ->
      Location_0 ->
      (Set String, Map' Op, Map' ([String], Syntax_type')) ->
      Expression_0 ->
      Err (Expression_6, Syntax_type'))
  type_syn m0 m1 (u, k, b) c =
    case c of
      Application_expression_0 d e ->
        (
          (,) <$> type_syn m0 m1 (u, k, b) d <*> type_syn m0 m1 (u, k, b) e >>=
          \((f, g), (h, i)) ->
            case (g, i) of
              (Expr_syntax', Expr_syntax') -> Right (Application_expression_6 f h, Expr_syntax')
              (Arrow_syntax' j u2, l) ->
                case j == l of
                  False -> Left (Error ("Syntactic type mismatch in " ++ m0 ++ " at ") (location' m1))
                  True -> Right (Syn_app_expression_6 f h, u2)
              _ -> Left (Error ("Syntactic type mismatch in " ++ m0 ++ " at ") (location' m1)))
      Branch_expression_0 d e f g ->
        (\h -> \j -> (Branch_expression_6 d h f j, Expr_syntax')) <$> type_syn' m0 m1 (u, k, b) e <*> type_syn' m0 m1 (u, k, b) g
      Char_expression_0 d -> Right (Char_expression_6 d, Expr_syntax')
      Function_expression_0 d e ->
        (\g -> \f -> (Function_expression_6 g f, Expr_syntax')) <$> std_pat (k, u) d <*> type_syn' m0 m1 (u, k, b) e
      Int_expression_0 d -> Right (Int_expression_6 d, Expr_syntax')
      Let_expression_0 d e -> (\x -> (x, Expr_syntax')) <$> std_let m0 m1 (u, k, b) d e
      List_expression_0 d ->
        (
          traverse (type_syn m0 m1 (u, k, b)) d >>=
          \e ->
            case e of
              [] -> undefined
              (f, g) : h -> (List_expression_6 (f : (fst <$> h)), List_syntax' g) <$ type_syn_list m0 m1 g (snd <$> h))
      Match_expression_0 e f ->
        (
          (\g -> \i -> (Match_expression_6 g i, Expr_syntax')) <$>
          type_syn' m0 m1 (u, k, b) e <*> traverse (type_syn_case m0 m1 (u, k, b)) f)
      Modular_expression_0 d -> Right (Modular_expression_6 d, Expr_syntax')
      Name_expression_0 d -> Right (Name_expression_6 d, Expr_syntax')
      Op_expression_0 d e ->
        (
          (\f -> \g -> (Op_expression_6 f g, Expr_syntax')) <$>
          type_syn' m0 m1 (u, k, b) d <*>
          traverse (\(f, g) -> (,) f <$> type_syn' m0 m1 (u, k, b) g) e)
      Syntax_expression_0 d g -> type_syn_name d b g m1 (\j -> \k' -> Syntax_expression_6 (Name j k')) Data.Set.empty
-}
  type_syn' :: Maybe Name -> (Set String, Map' Op, Map' ([String], Syntax_type')) -> Expression_0 -> Err Expression_6
{-
    (
      type_syn b c d >>=
        \(e, f) ->
          case Expr_syntax' == f of
            False ->
              Left
                (case b of
                  Nothing -> Error ("Syntactic type mismatch in " ++ b0 ++ " at ") (location' b1))
            True -> Right e)
-}
  type_syn' a (b, g, h) c =
    (
      make_syn_eqs' (g, h) c Expr_syntax' >>=
      \(e, f) ->
        case solve_syn_eqs b f of
          Nothing -> Right e
          Just (i, j) -> Left (Syntactic_type_mismatch a i j))
{-
  type_syn_case :: String -> Location_0 -> (Set String, Map' Op, Map' ([String], Syntax_type')) -> Case_0 -> Err Case_6
  type_syn_case b0 b1 (u, c, f) (Case_0 d e) = Case_6 <$> std_apat (u, c) d <*> type_syn' b0 b1 (u, c, f) e
-}
{-
  type_syn_list :: String -> Location_0 -> Syntax_type' -> [Syntax_type'] -> Err ()
  type_syn_list a0 a1 b c =
    case c of
      [] -> Right ()
      d : e ->
        case b == d of
          False -> Left (Error ("Syntactic type mismatch in " ++ a0 ++ " at ") (location' a1))
          True -> type_syn_list a0 a1 b e
-}
{-
  type_syn_name ::
    (
      Name ->
      Map' ([String], Syntax_type') ->
      [Syntax_type] ->
      Location_0 ->
      (Location_0 -> String -> t) ->
      Set String ->
      Err (t, Syntax_type'))
  type_syn_name (Name m c) h k j a g =
    und_err
      c
      h
      "syntax"
      m
      (\(i, e) ->
        case length i == length k of
          False -> Left (Error ("Wrong number of type arguments to " ++ c ++ " at ") (location' j))
          True -> (\f -> (a m c, syntyperep (Data.Map.fromList (zip i f)) e)) <$> traverse (syntype_check g) k)
-}
{-
  type_synexpr ::
    Set String -> String -> Location_0 -> Map' ([String], Syntax_type') -> Syntax_expr_2 -> Err (Syntax_3, Syntax_type')
  type_synexpr r b0 b1 c d =
    case d of
      Application_syntax_2 e f ->
        (
          (,) <$> type_synexpr r b0 b1 c e <*> type_synexpr r b0 b1 c f >>=
          \((g, h), (i, j)) ->
            case (h, j) of
              (Expr_syntax', Expr_syntax') -> Right (Application_syntax_3 g i, Expr_syntax')
              (Arrow_syntax' k l, m) ->
                case k == m of
                  False -> Left (Error ("Syntactic type mismatch in " ++ b0 ++ " at ") (location' b1))
                  True -> Right (Synapp_syntax_3 g i, l)
              _ -> Left (Error ("Syntactic type mismatch in " ++ b0 ++ " at ") (location' b1)))
      Case_syntax_2 q (Name e f) g (h, i) j ->
        und_err
          f
          c
          "syntax"
          e
          (\(s, k) ->
            case s of
              [] ->
                case k of
                  List_syntax' l ->
                    (
                      (
                        (,) <$>
                        type_synexpr r b0 b1 c g <*>
                        type_synexpr
                          r
                          b0
                          b1
                          (Data.Map.union
                            c
                            (Data.Map.fromList
                              (
                                (case h of
                                  Blank_pattern -> []
                                  Name_pattern h' -> [(h', ([], l))]) ++
                                case i of
                                  Blank_pattern -> []
                                  Name_pattern i' -> [(i', ([], k))])))
                          j) >>=
                      \((m, n), (o, p)) ->
                        case n == p of
                          False -> Left (Error ("Syntactic type mismatch in " ++ b0 ++ " at ") (location' b1))
                          True -> Right (Case_syntax_3 (Syntax_syntax_3 f) m (h, i) o, n))
                  _ -> Left (Error ("Syntactic type mismatch in " ++ b0 ++ " at ") (location' b1))
              _ -> Left (Error ("Case over a polymorphic expression in " ++ b0 ++ " at ") (location' q)))
      Cat_syntax_2 e f ->
        (
          type_synexpr r b0 b1 c e >>=
          \(g, h) ->
            (
              type_synexpr r b0 b1 c f >>=
              \(i, j) ->
                case j of
                  List_syntax' k ->
                    case h == k of
                      False -> Left (Error ("Syntactic type mismatch in " ++ b0 ++ " at ") (location' b1))
                      True -> Right (Cat_syntax_3 g i, j)
                  _ -> Left (Error ("Syntactic type mismatch in " ++ b0 ++ " at ") (location' b1))))
      Emp_syntax_2 e -> (\f -> (Emp_syntax_3, List_syntax' f)) <$> syntype_check r e
      Name_syntax_2 e -> Right (Name_syntax_3 e, Expr_syntax')
      Syntax_syntax_2 e h -> type_syn_name e c h b1 (return Syntax_syntax_3) r
-}
  type_syntax_0 ::
    Map' (([String], Syntax_type'), Status) -> Syntax_2 -> Err (Map' (([String], Syntax_type'), Status), Syntax_4)
  type_syntax_0 a (Syntax_2 g c e b d f) =
    let
      i = syntype_check (Data.Set.fromList e)
    in
      (
        (\j -> \h -> (ins_new c (e, Prelude.foldr (\(_, t) -> Arrow_syntax' t) h j) a, Syntax_4 g c e j h f)) <$>
        traverse (traverse i) b <*>
        i d)
  type_syntax_1 :: Map' ([String], Syntax_type') -> Syntax_4 -> Map' Syntax_3 -> Err (Map' Syntax_3)
  type_syntax_1 b (Syntax_4 c d m j e f) g =
{-
    (
      type_synexpr
        (Data.Set.fromList m)
        d
        c
        (Prelude.foldl
          (\h -> \(i, k) ->
            case i of
              Blank_pattern -> h
              Name_pattern i' -> Data.Map.insert i' ([], k) h)
          b
          j)
        f >>=
      \(h, i) ->
        case e == i of
          False -> Left (Error ("Syntactic type mismatch in " ++ d ++ " at ") (location' c))
          True -> Right (Data.Map.insert d (Prelude.foldr Function_syntax_3 h (fst <$> j)) g))
-}
    (
      make_syn_eqs (Prelude.foldl (\h -> \(i, k) -> Data.Map.insert i ([], k) h) b j) f e >>=
      \(a, h) ->
        case solve_syn_eqs (Data.Set.fromList m) h of
          Nothing -> Right (Data.Map.insert d (Prelude.foldr Function_syntax_3 a (fst <$> j)) g)
          Just (i, k) -> Left (Syntactic_type_mismatch (Just (Name c d)) i k))
  type_syntaxes ::
    [Syntax_2] -> (Map' ([String], Syntax_type'), Map' Syntax_3) -> Err (Map' (([String], Syntax_type'), Status), Map' Syntax_3)
  type_syntaxes b (c, d) = type_syntaxes_0 (old c) b >>= \(e, f) -> (,) e <$> type_syntaxes_1 (fst <$> e) f d
  type_syntaxes_0 ::
    Map' (([String], Syntax_type'), Status) -> [Syntax_2] -> Err (Map' (([String], Syntax_type'), Status), [Syntax_4])
  type_syntaxes_0 a b =
    case b of
      [] -> Right (a, [])
      c : d -> type_syntax_0 a c >>= \(e, f) -> second ((:) f) <$> type_syntaxes_0 e d
  type_syntaxes_1 :: Map' ([String], Syntax_type') -> [Syntax_4] -> Map' Syntax_3 -> Err (Map' Syntax_3)
  type_syntaxes_1 b c d =
    case c of
      [] -> Right d
      e : f -> type_syntax_1 b e d >>= type_syntaxes_1 b f
  und_err :: String -> Map' t -> String -> Location_0 -> (t -> Err u) -> Err u
  und_err a b c g f =
    case Data.Map.lookup a b of
      Just e -> f e
      Nothing -> Left (Undefined c a g)
  unwrap :: Either t u -> u
  unwrap x =
    case x of
      Left _ -> undefined
      Right y -> y
--------------------------------------------------------------------------------------------------------------------------------