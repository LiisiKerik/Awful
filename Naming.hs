-----------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Naming where
  import Data.Bifunctor
  import Data.Map
  import Data.Set
  import Standard
  import Tokenise
  import Tree
  data Alg_pat_1 =
    Application_alg_pat_1 String [Alg_pat_1] |
    Blank_alg_pat_1 |
    Char_alg_pat_1 Char |
    Int_alg_pat_1 Integer |
    Modular_alg_pat_1 Modular |
    Name_alg_pat_1 String
      deriving Show
  data Case_2 = Case_2 Alg_pat_1 Expression_1 deriving Show
  data Class_1 = Class_1 String (Name, Kind_0) (Maybe Name) [Method_1] deriving Show
  data Class_2 = Class_2 String (String, Kind_0) (Maybe Name) [Method_2] deriving Show
  data Data_1 = Data_1 String [(Name, Kind_0)] Data_branch_1 deriving Show
  data Data_2 = Data_2 String [(String, Kind_0)] Data_branch_2 deriving Show
  data Data_br_1 = Data_br_1 String [(String, Type_8)] deriving Show
  data Data_branch_1 =
    Algebraic_data_1 [Form_1] | Branching_data_1 Data_br_1 Name Data_br_1 | Struct_data_1 [(String, Type_8)]
      deriving Show
  data Data_branch_2 =
    Algebraic_data_2 [Form_1] | Branching_data_2 Data_br_1 String Data_br_1 | Struct_data_2 [(String, Type_8)]
      deriving Show
  data Def_2 =
    Basic_def_2 Location_0 String [(Name, Kind_0)] [Constraint_0] Type_8 Expression_9 |
    Instance_2 Location_0 Name Name [Pattern_1] [Constraint_0] [(Name, Expression_9)]
      deriving Show
  data Def_3 =
    Basic_def_3 Location_0 String [(String, Kind_0)] [Constraint_0] Type_8 Expression_1 |
    Instance_3 Location_0 Name Name [Pattern_0] [Constraint_0] [(Name, Expression_1)]
      deriving Show
  data Expression_1 =
    Application_expression_1 Expression_1 Expression_1 |
    Branch_expression_1 Name Expression_1 String Expression_1 |
    Char_expression_1 Char |
    Function_expression_1 Pat Expression_1 |
    Int_expression_1 Integer |
    Match_expression_1 Location_0 Expression_1 [Case_2] |
    Modular_expression_1 Modular |
    Name_expression_1 Name (Maybe Type_8) [Type_8]
      deriving Show
  data Form_1 = Form_1 String [Type_8] deriving Show
  type Locations = Map' Location'
  data Method_1 = Method_1 String [(Name, Kind_0)] [Constraint_0] Type_8 deriving Show
  data Method_2 = Method_2 String [(String, Kind_0)] [Constraint_0] Type_8 deriving Show
  data Tree_4 = Tree_4 [Data_1] [Class_1] [Name] [Def_2] deriving Show
  data Tree_5 = Tree_5 [Data_2] [Class_2] [Name] [Def_3] deriving Show
  add :: Ord t => Map t u -> t -> u -> Either u (Map t u)
  add x y z =
    let
      (w, x') = insertLookupWithKey (return return) y z x
    in
      case w of
        Just z' -> Left z'
        Nothing -> Right x'
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
  naming :: String -> Tree_2 -> (Set String, Locations, Locations) -> Err ((Set String, Locations, Locations), Tree_5)
  naming f a b = naming_1 f a b >>= \((c, e, g), d) -> (,) (c, e, g) <$> naming_2 f d (c, e)
  naming_1 :: String -> Tree_2 -> (Set String, Locations, Locations) -> Err ((Set String, Locations, Locations), Tree_4)
  naming_1 f (Tree_2 a g j b) (c, k, l) =
    (
      naming_datas_1 f a (c, k) >>=
      \((t, d), e) ->
        (
          naming_classes_0 f g d >>=
          \(d', e') -> (\(m, n) -> \(h, i) -> ((t, i, m), Tree_4 e e' n h)) <$> naming_ops f l j <*> naming_defs_1 f b d'))
  naming_2 :: String -> Tree_4 -> (Set String, Locations) -> Err Tree_5
  naming_2 e (Tree_4 a f c b) (h, i) =
    naming_datas_2 e a i >>= \d -> naming_classes_1 e f i >>= \g -> Tree_5 d g c <$> naming_defs_2 e b (h, i)
{-
  naming_pat :: String -> Pat -> (Set String, Locations) -> Err (Locations, Pat)
  naming_pat a (Pat b c) (f, d) =
    case c of
      Application_pat g e -> second (\h -> Pat b (Application_pat g h)) <$> naming_pats a e (f, d)
      Blank_pat -> Right (d, Pat b Blank_pat)
      Name_pat e ->
        case Data.Set.member e f of
          False -> (\(g, _) -> (g, Pat b c)) <$> naming_name a (Name b e) d
          True -> Right (d, Pat b (Application_pat e []))
-}
  naming_alg_pats :: String -> (Set String, Locations) -> [Alg_pat] -> Err (Locations, [Alg_pat_1])
  naming_alg_pats a (b, c) d =
    case d of
      [] -> Right (c, [])
      e : f -> naming_alg_pattern a (b, c) e >>= \(g, h) -> second ((:) h) <$> naming_alg_pats a (b, g) f
  naming_alg_pattern :: String -> (Set String, Locations) -> Alg_pat -> Err (Locations, Alg_pat_1)
  naming_alg_pattern a (b, c) d =
    case d of
      Application_alg_pat e f -> second (Application_alg_pat_1 e) <$> naming_alg_pats a (b, c) f
      Blank_alg_pat -> Right (c, Blank_alg_pat_1)
      Char_alg_pat e -> Right (c, Char_alg_pat_1 e)
      Int_alg_pat e -> Right (c, Int_alg_pat_1 e)
      Modular_alg_pat e -> Right (c, Modular_alg_pat_1 e)
      Name_alg_pat (Name e f) ->
        case Data.Set.member f b of
          False -> (\(g, _) -> (g, Name_alg_pat_1 f)) <$> naming_name a (Name e f) c
          True -> Right (c, Application_alg_pat_1 f [])
  naming_application :: String -> (Set String, Locations) -> Err Expression_1 -> Expression_9 -> Err Expression_1
  naming_application a b c d = c >>= \e -> Application_expression_1 e <$> naming_expression a d b
  naming_args :: String -> [(Name, t)] -> Locations -> Err [(String, t)]
  naming_args a b c =
    case b of
      [] -> Right []
      (d, e) : f -> naming_name a d c >>= \(g, h) -> (:) (h, e) <$> naming_args a f g
  naming_argument ::
    (String -> t -> Locations -> Err (Locations, u)) -> String -> (t, v) -> Locations -> Err (Locations, (u, v))
  naming_argument a e (b, c) d = second (flip (,) c) <$> a e b d
  naming_arguments ::
    (String -> t -> Locations -> Err (Locations, u)) -> String -> [(t, v)] -> Locations -> Err (Locations, [(u, v)])
  naming_arguments = naming_list <$> naming_argument
  naming_arguments' :: String -> (String -> t -> Locations -> Err (Locations, u)) -> [(t, v)] -> Locations -> Err [(u, v)]
  naming_arguments' c a b = (<$>) snd <$> naming_arguments a c b
  naming_case :: String -> (Set String, Locations) -> Case_1 -> Err Case_2
  naming_case a (b, g) (Case_1 c d) = naming_alg_pattern a (b, g) c >>= \(e, f) -> Case_2 f <$> naming_expression a d (b, e)
  naming_cases :: String -> [Case_1] -> (Set String, Locations) -> Err [Case_2]
  naming_cases a b c = traverse (naming_case a c) b
  naming_class_0 :: String -> Class_7 -> Locations -> Err (Locations, Class_1)
  naming_class_0 a (Class_7 b c h d) e = naming_name a b e >>= \(f, g) -> second (Class_1 g c h) <$> naming_methods_0 a d f
  naming_class_1 :: String -> Class_1 -> Locations -> Err Class_2
  naming_class_1 a (Class_1 b (c, d) h e) f = naming_name a c f >>= \(i, g) -> Class_2 b (g, d) h <$> naming_methods_1 a e i
  naming_classes_0 :: String -> [Class_7] -> Locations -> Err (Locations, [Class_1])
  naming_classes_0 a b c =
    case b of
      [] -> Right (c, [])
      d : e -> naming_class_0 a d c >>= \(f, g) -> second ((:) g) <$> naming_classes_0 a e f
  naming_classes_1 :: String -> [Class_1] -> Locations -> Err [Class_2]
  naming_classes_1 a b c =
    case b of
      [] -> Right []
      d : e -> naming_class_1 a d c >>= \g -> (:) g <$> naming_classes_1 a e c
  naming_data_1 :: String -> Data_6 -> (Set String, Locations) -> Err ((Set String, Locations), Data_1)
  naming_data_1 a (Data_6 b c d e) (f, g) =
    naming_name a (Name b c) g >>= \(h, _) -> second (Data_1 c d) <$> naming_data_branch_1 a e (f, h) c
  naming_data_2 :: String -> Data_1 -> Locations -> Err Data_2
  naming_data_2 a (Data_1 b c d) e =
    naming_arguments naming_name a c e >>= \(f, g) -> Data_2 b g <$> naming_data_branch_2 a d f
  naming_data_br :: String -> Data_br_6 -> (Set String, Locations) -> Err ((Set String, Locations), Data_br_1)
  naming_data_br a (Data_br_6 b c) (d, e) =
    naming_name a b e >>= \(f, g) -> bimap ((,) (Data.Set.insert g d)) (Data_br_1 g) <$> naming_fields a c f
  naming_data_branch_1 ::
    String -> Data_branch_6 -> (Set String, Locations) -> String -> Err ((Set String, Locations), Data_branch_1)
  naming_data_branch_1 a b (c, d) e =
    case b of
      Algebraic_data_6 f -> bimap ((,) c) Algebraic_data_1 <$> naming_forms a f d
      Branching_data_6 f g h ->
        naming_data_br a f (c, d) >>= \(i, j) -> second (Branching_data_1 j g) <$> naming_data_br a h i
      Struct_data_6 f -> bimap ((,) (Data.Set.insert e c)) Struct_data_1 <$> naming_fields a f d
  naming_data_branch_2 :: String -> Data_branch_1 -> Locations -> Err Data_branch_2
  naming_data_branch_2 a b c =
    case b of
      Algebraic_data_1 d -> Right (Algebraic_data_2 d)
      Branching_data_1 d e f -> (\(_, g) -> Branching_data_2 d g f) <$> naming_name a e c
      Struct_data_1 d -> Right (Struct_data_2 d)
  naming_datas_1 :: String -> [Data_6] -> (Set String, Locations) -> Err ((Set String, Locations), [Data_1])
  naming_datas_1 = naming_list naming_data_1
  naming_datas_2 :: String -> [Data_1] -> Locations -> Err [Data_2]
  naming_datas_2 f a b =
    case a of
      [] -> Right []
      c : d -> naming_data_2 f c b >>= \e -> (:) e <$> naming_datas_2 f d b
  naming_def_1 :: String -> Def_1 -> Locations -> Err (Def_2, Locations)
  naming_def_1 i a g =
    case a of
      Basic_def_1 c @ (Name h j) b x d e -> (\(f, _) -> (Basic_def_2 h j b x d e, f)) <$> naming_name i c g
      Instance_1 b c d f h e -> Right (Instance_2 b c d f h e, g)
  naming_def_2 :: String -> Def_2 -> (Set String, Locations) -> Err Def_3
  naming_def_2 j a (m, b) =
    case a of
      Basic_def_2 k c d t f g ->
        naming_arguments naming_name j d b >>= \(h, i) -> Basic_def_3 k c i t f <$> naming_expression j g (m, h)
      Instance_2 f c d g k e -> naming_patterns j g b >>= \(h, i) -> Instance_3 f c d i k <$> naming_nameexprs j (m, h) e
  naming_defs_1 :: String -> [Def_1] -> Locations -> Err ([Def_2], Locations)
  naming_defs_1 a b c =
    case b of
      [] -> Right ([], c)
      d : e -> naming_def_1 a d c >>= \(f, g) -> first ((:) f) <$> naming_defs_1 a e g
  naming_defs_2 :: String -> [Def_2] -> (Set String, Locations) -> Err [Def_3]
  naming_defs_2 = naming_list' naming_def_2
  naming_expression :: String -> Expression_9 -> (Set String, Locations) -> Err Expression_1
  naming_expression g a (f, b) =
    case a of
      Application_expression_9 c d -> naming_application g (f, b) (naming_expression g c (f, b)) d
      Branch_expression_9 c d e h ->
        (
          (\i -> \(j, k) -> Branch_expression_1 c i j k) <$>
          naming_expression g d (f, b) <*>
          (naming_name g e b >>= \(i, j) -> (,) j <$> naming_expression g h (f, i)))
      Char_expression_9 c -> Right (Char_expression_1 c)
      Function_expression_9 c d -> naming_fun g (f, b) c d
      Int_expression_9 c -> Right (Int_expression_1 c)
      Let_expression_9 (Eqq' (Name l c) d e) h ->
        let
          i j = naming_application g (f, b) (naming_fun g (f, b) (Pat l j) h)
        in
          case Data.Set.member c f of
            False -> i (Name_pat c) (Prelude.foldr Function_expression_9 e d)
            True -> i (Application_pat c d) e
      Match_expression_9 h c d -> Match_expression_1 h <$> naming_expression g c (f, b) <*> naming_cases g d (f, b)
      Modular_expression_9 c -> Right (Modular_expression_1 c)
      Name_expression_9 c d e -> Right (Name_expression_1 c d e)
  naming_fields :: String -> [(Name, Type_8)] -> Locations -> Err (Locations, [(String, Type_8)])
  naming_fields = naming_arguments naming_name
  naming_form :: String -> Form_6 -> Locations -> Err (Locations, Form_1)
  naming_form d (Form_6 a b) c = second (flip Form_1 b) <$> naming_name d a c
  naming_forms :: String -> [Form_6] -> Locations -> Err (Locations, [Form_1])
  naming_forms = naming_list naming_form
  naming_fun :: String -> (Set String, Locations) -> Pat -> Expression_9 -> Err Expression_1
  naming_fun x (y, b) z w = naming_pat x z (y, b) >>= \(a, c) -> Function_expression_1 c <$> naming_expression x w (y, a)
  naming_list :: (String -> t -> u -> Err (u, v)) -> String -> [t] -> u -> Err (u, [v])
  naming_list a h b c =
    case b of
      [] -> Right (c, [])
      d : e -> a h d c >>= \(f, g) -> second ((:) g) <$> naming_list a h e f
  naming_list' :: (String -> t -> u -> Err v) -> String -> [t] -> u -> Err [v]
  naming_list' a g b c =
    case b of
      [] -> Right []
      d : e -> a g d c >>= \f -> (:) f <$> naming_list' a g e c
{-
  naming_match_algebraic :: String -> Match_Algebraic_9 -> (Set String, Locations) -> Err Match_Algebraic_1
  naming_match_algebraic a (Match_Algebraic_9 b c d) (g, e) =
    naming_pats a c (g, e) >>= \(f, h) -> Match_Algebraic_1 b h <$> naming_expression a d (g, f)
  naming_match_char :: String -> Match_char_9 -> (Set String, Locations) -> Err Match_char_1
  naming_match_char a (Match_char_9 e b c) d = Match_char_1 e b <$> naming_expression a c d
  naming_match_int :: String -> Match_Int_9 -> (Set String, Locations) -> Err Match_Int_1
  naming_match_int a (Match_Int_9 e b c) d = Match_Int_1 e b <$> naming_expression a c d
  naming_match_modular :: String -> Match_Modular_9 -> (Set String, Locations) -> Err Match_Modular_1
  naming_match_modular a (Match_Modular_9 e b c) d = Match_Modular_1 e b <$> naming_expression a c d
  naming_matches :: String -> Matches_9 -> (Set String, Locations) -> Err Matches_1
  naming_matches a b c =
    let
      j k = naming_expression a k c
    in
      case b of
        Matches_Algebraic_9 d e -> naming_default c naming_matches_algebraic d a Matches_Algebraic_1 e
        Matches_char_9 d e -> naming_matches_char a d c >>= \f -> Matches_char_1 f <$> j e
        Matches_Int_9 d e -> naming_matches_int a d c >>= \f -> Matches_Int_1 f <$> j e
        Matches_Modular_9 d e -> naming_default c naming_matches_modular d a Matches_Modular_1 e
  naming_matches_algebraic :: String -> [Match_Algebraic_9] -> (Set String, Locations) -> Err [Match_Algebraic_1]
  naming_matches_algebraic a b c =
    case b of
      [] -> Right []
      d : e -> naming_match_algebraic a d c >>= \f -> (:) f <$> naming_matches_algebraic a e c
  naming_matches_char :: String -> [Match_char_9] -> (Set String, Locations) -> Err [Match_char_1]
  naming_matches_char a b c =
    case b of
      [] -> Right []
      d : e -> naming_match_char a d c >>= \f -> (:) f <$> naming_matches_char a e c
  naming_matches_int :: String -> [Match_Int_9] -> (Set String, Locations) -> Err [Match_Int_1]
  naming_matches_int a b c =
    case b of
      [] -> Right []
      d : e -> naming_match_int a d c >>= \f -> (:) f <$> naming_matches_int a e c
  naming_matches_modular :: String -> [Match_Modular_9] -> (Set String, Locations) -> Err [Match_Modular_1]
  naming_matches_modular a b c =
    case b of
      [] -> Right []
      d : e -> naming_match_modular a d c >>= \f -> (:) f <$> naming_matches_modular a e c
-}
  naming_method_0 :: String -> Method_9 -> Locations -> Err (Locations, Method_1)
  naming_method_0 a (Method_9 b c g d) e = second (\f -> Method_1 f c g d) <$> naming_name a b e
  naming_method_1 :: String -> Method_1 -> Locations -> Err Method_2
  naming_method_1 a (Method_1 b c g d) e = (\f -> Method_2 b f g d) <$> naming_args a c e
  naming_methods_0 :: String -> [Method_9] -> Locations -> Err (Locations, [Method_1])
  naming_methods_0 a b c =
    case b of
      [] -> Right (c, [])
      d : e -> naming_method_0 a d c >>= \(f, g) -> second ((:) g) <$> naming_methods_0 a e f
  naming_methods_1 :: String -> [Method_1] -> Locations -> Err [Method_2]
  naming_methods_1 a b c =
    case b of
      [] -> Right []
      d : e -> naming_method_1 a d c >>= \g -> (:) g <$> naming_methods_1 a e c
  naming_name :: String -> Name -> Locations -> Err (Locations, String)
  naming_name f (Name a c) d =
    bimap (flip (location_err ("definitions of " ++ c)) (Location_1 f a)) (flip (,) c) (add d c (Library (Location_1 f a)))
  naming_nameexprs :: String -> (Set String, Locations) -> [(Name, Expression_9)] -> Err [(Name, Expression_1)]
  naming_nameexprs a b c =
    case c of
      [] -> Right []
      (d, e) : f -> naming_expression a e b >>= \g -> (:) (d, g) <$> naming_nameexprs a b f
  naming_names :: String -> [(Name, t)] -> Locations -> Err (Locations, [(String, t)])
  naming_names a b c =
    case b of
      [] -> Right (c, [])
      (d, e) : f -> naming_name a d c >>= \(g, h) -> second ((:) (h, e)) <$> naming_names a f g
  naming_names' :: String -> Locations -> [Name] -> Err [String]
  naming_names' a b c =
    case c of
      [] -> Right []
      d : e -> naming_name a d b >>= \(f, g) -> (:) g <$> naming_names' a f e
  naming_ops :: String -> Locations -> [Opdecl_1] -> Err (Locations, [Name])
  naming_ops a b c =
    case c of
      [] -> Right (b, [])
      Opdecl_1 d e f : i ->
        let
          h = Location_1 a d
        in
          case Data.Map.lookup e b of
            Just g -> Left (location_err ("definitions of " ++ e) g h)
            Nothing -> second ((:) f) <$> naming_ops a (Data.Map.insert e (Library h) b) i
  naming_pat :: String -> Pat -> (Set String, Locations) -> Err (Locations, Pat)
  naming_pat a (Pat b c) (f, d) =
    case c of
      Application_pat g e -> second (\h -> Pat b (Application_pat g h)) <$> naming_pats a e (f, d)
      Blank_pat -> Right (d, Pat b Blank_pat)
      Name_pat e ->
        case Data.Set.member e f of
          False -> (\(g, _) -> (g, Pat b c)) <$> naming_name a (Name b e) d
          True -> Right (d, Pat b (Application_pat e []))
  naming_pats :: String -> [Pat] -> (Set String, Locations) -> Err (Locations, [Pat])
  naming_pats a b (f, c) =
    case b of
      [] -> Right (c, [])
      d : e -> naming_pat a d (f, c) >>= \(g, h) -> second ((:) h) <$> naming_pats a e (f, g)
  naming_pattern :: String -> Pattern_1 -> Locations -> Err (Locations, Pattern_0)
  naming_pattern f (Pattern_1 a c) d =
    case c of
      Blank_pattern -> Right (d, Blank_pattern)
      Name_pattern e -> second Name_pattern <$> naming_name f (Name a e) d
  naming_patterns :: String -> [Pattern_1] -> Locations -> Err (Locations, [Pattern_0])
  naming_patterns = naming_list naming_pattern
-----------------------------------------------------------------------------------------------------------------------------