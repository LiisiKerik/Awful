--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Naming where
  import Data.Bifunctor
  import Data.Map
  import Standard
  import Tokenise
  import Tree
  data Alg_pat_1 =
    Application_alg_pat_1 Location_0 String [Alg_pat_1] |
    Blank_alg_pat_1 |
    Char_alg_pat_1 Char |
    Int_alg_pat_1 Integer |
    Modular_alg_pat_1 Modular |
    Name_alg_pat_1 String
      deriving Show
  data Case_2 = Case_2 Alg_pat_1 Expression_1 deriving Show
  data Class_1 = Class_1 String (Name, Kind_0) [Constraint_0] [Method_1] deriving Show
  data Class_2 = Class_2 String (String, Kind_0) [Constraint_0] [Method_2] deriving Show
  data Data_1 = Data_1 String [(Name, Kind_0)] Data_br_1 deriving Show
  data Data_2 = Data_2 String [(String, Kind_0)] Data_br_2 deriving Show
  data Data_br_1 =
    Algebraic_data_1 [Form_1] |
    Branching_data_1 Name (Data_br_1, Name, Data_br_1) |
    Struct_data_1 Status String [(String, Type_8)]
      deriving Show
  data Data_br_2 =
    Algebraic_data_2 [Form_1] |
    Branching_data_2 Name (Data_br_2, String, Data_br_2) |
    Struct_data_2 Status String [(String, Type_8)]
      deriving Show
  data Def_2 =
    Basic_def_2 Location_0 String Kinds_constraints Type_8 Expression_9 |
    Instance_2 Location_0 Name (Name, [Pattern_1]) [Constraint_0] [(Name, Expression_9)]
      deriving Show
  data Def_3 =
    Basic_def_3 Location_0 String Kinds_constraints' Type_8 Expression_1 |
    Instance_3 Location_0 Name (Name, [Pattern_0]) [Constraint_0] [(Name, Expression_1)]
      deriving Show
  data Expression_1 =
    Application_expression_1 Expression_1 Expression_1 |
    Branch_expression_1 Name Expression_1 Pattern_0 Expression_1 |
    Char_expression_1 Char |
    Function_expression_1 Pat' Expression_1 |
    Int_expression_1 Integer |
    Match_expression_1 Expression_1 [Case_2] |
    Modular_expression_1 Modular |
    Name_expression_1 Name |
    Write_expression_1
      deriving Show
  data Form_1 = Form_1 Status String [Type_8] deriving Show
  data Kinds_constraints' = Kinds_constraints' [(String, Kind_0)] [Constraint_0] deriving Show
  data Method_1 = Method_1 String Kinds_constraints Type_8 deriving Show
  data Method_2 = Method_2 String Kinds_constraints' Type_8 deriving Show
  data Pat' = Application_pat' Name [Pat'] | Blank_pat' | Name_pat' String deriving Show
  data Tree_4 = Tree_4 [Data_1] [Class_1] [Def_2] deriving Show
  data Tree_5 = Tree_5 [Data_2] [Class_2] [Def_3] deriving Show
  naming ::
    (
      String ->
      Tree_2 ->
      ((Locations, Locations, Locations), (Locations, Locations), Map' (Map' Location')) ->
      Err (((Locations, Locations, Locations), (Locations, Locations), Map' (Map' Location')), Tree_5))
  naming f a b = naming_1 f a b >>= \(((c, e, i), g, h), d) -> (,) ((c, e, i), g, h) <$> naming_2 f d (c, i)
  naming_1 ::
    (
      String ->
      Tree_2 ->
      ((Locations, Locations, Locations), (Locations, Locations), Map' (Map' Location')) ->
      Err (((Locations, Locations, Locations), (Locations, Locations), Map' (Map' Location')), Tree_4))
  naming_1 f (Tree_2 j' j a g b) ((k0, k1, k2), (l0, l), x) =
    (
      (,) <$> ((,) <$> naming_ops f l0 j' <*> naming_ops f l j) <*> naming_datas_1 f a (k0, k2) >>=
      \(m, ((d0, d1), e)) ->
        (
          naming_classes_0 f g (k1, d1) >>=
          \((t0, t1), e') -> (\(h, (i, y)) -> (((d0, t0, i), m, y), Tree_4 e e' h)) <$> naming_defs_1 f b (t1, x)))
  naming_2 :: String -> Tree_4 -> (Locations, Locations) -> Err Tree_5
  naming_2 e (Tree_4 a f b) (i0, i1) =
    Tree_5 <$> naming_datas_2 e a i0 <*> naming_classes_1 e f i0 <*> naming_defs_2 e b (i0, i1)
  naming_alg_pats :: String -> Locations -> [Alg_pat_7] -> Err (Locations, [Alg_pat_1])
  naming_alg_pats a c d =
    case d of
      [] -> Right (c, [])
      e : f -> naming_alg_pattern a c e >>= \(g, h) -> second ((:) h) <$> naming_alg_pats a g f
  naming_alg_pattern :: String -> Locations -> Alg_pat_7 -> Err (Locations, Alg_pat_1)
  naming_alg_pattern a c d =
    case d of
      Application_alg_pat_7 g e f -> second (Application_alg_pat_1 g e) <$> naming_alg_pats a c f
      Blank_alg_pat_7 -> Right (c, Blank_alg_pat_1)
      Char_alg_pat_7 e -> Right (c, Char_alg_pat_1 e)
      Int_alg_pat_7 e -> Right (c, Int_alg_pat_1 e)
      Modular_alg_pat_7 e -> Right (c, Modular_alg_pat_1 e)
      Name_alg_pat_7 e -> second Name_alg_pat_1 <$> naming_name a e c
  naming_application :: String -> Locations -> Err Expression_1 -> Expression_9 -> Err Expression_1
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
  naming_case :: String -> Locations -> Case_1 -> Err Case_2
  naming_case a g (Case_1 c d) = naming_alg_pattern a g c >>= \(e, f) -> Case_2 f <$> naming_expression a d e
  naming_cases :: String -> [Case_1] -> Locations -> Err [Case_2]
  naming_cases a b c = traverse (naming_case a c) b
  naming_class_0 :: String -> Class_7 -> (Locations, Locations) -> Err ((Locations, Locations), Class_1)
  naming_class_0 a (Class_7 b c h d) (e0, e1) =
    (\(f, g) -> bimap ((,) f) (Class_1 g c h)) <$> naming_name a b e0 <*> naming_methods_0 a d e1
  naming_class_1 :: String -> Class_1 -> Locations -> Err Class_2
  naming_class_1 a (Class_1 b (c, d) h e) f = naming_name a c f >>= \(i, g) -> Class_2 b (g, d) h <$> naming_methods_1 a e i
  naming_classes_0 :: String -> [Class_7] -> (Locations, Locations) -> Err ((Locations, Locations), [Class_1])
  naming_classes_0 a b c =
    case b of
      [] -> Right (c, [])
      d : e -> naming_class_0 a d c >>= \(f, g) -> second ((:) g) <$> naming_classes_0 a e f
  naming_classes_1 :: String -> [Class_1] -> Locations -> Err [Class_2]
  naming_classes_1 a b c =
    case b of
      [] -> Right []
      d : e -> naming_class_1 a d c >>= \g -> (:) g <$> naming_classes_1 a e c
  naming_data_1 :: String -> Data_6 -> (Locations, Locations) -> Err ((Locations, Locations), Data_1)
  naming_data_1 a (Data_6 b c d e) (f0, f1) =
    (\(g, _) -> bimap ((,) g) (Data_1 c d)) <$> naming_name a (Name b c) f0 <*> naming_data_br_0 a (f1, e)
  naming_data_2 :: String -> Data_1 -> Locations -> Err Data_2
  naming_data_2 a (Data_1 b c d) e = naming_kinds a (e, c) >>= \(f, g) -> Data_2 b g <$> naming_data_br_1 (a, f) d
  naming_data_br_0 :: String -> (Locations, Data_br_6) -> Err (Locations, Data_br_1)
  naming_data_br_0 a (b, c) =
    case c of
      Algebraic_data_6 d -> second Algebraic_data_1 <$> naming_forms a d b
      Branching_data_6 d (e, f, g) ->
        naming_data_br_0 a (b, e) >>= \(h, i) -> second (\j -> Branching_data_1 d (i, f, j)) <$> naming_data_br_0 a (h, g)
      Struct_data_6 e f g h -> naming_name a (Name e g) b >>= \(i, _) -> second (Struct_data_1 f g) <$> naming_fields a h i
  naming_data_br_1 :: (String, Locations) -> Data_br_1 -> Err Data_br_2
  naming_data_br_1 (a, b) c =
    case c of
      Algebraic_data_1 d -> Right (Algebraic_data_2 d)
      Branching_data_1 d (e, f, g) ->
        (
          (\h -> \(i, j) -> Branching_data_2 d (h, i, j)) <$>
          naming_data_br_1 (a, b) e <*> (naming_name a f b >>= \(h, i) -> (,) i <$> naming_data_br_1 (a, h) g))
      Struct_data_1 d e f -> Right (Struct_data_2 d e f)
  naming_datas_1 :: String -> [Data_6] -> (Locations, Locations) -> Err ((Locations, Locations), [Data_1])
  naming_datas_1 = naming_list naming_data_1
  naming_datas_2 :: String -> [Data_1] -> Locations -> Err [Data_2]
  naming_datas_2 f a b =
    case a of
      [] -> Right []
      c : d -> naming_data_2 f c b >>= \e -> (:) e <$> naming_datas_2 f d b
  naming_def_1 :: String -> Def_1 -> (Locations, Map' (Map' Location')) -> Err (Def_2, (Locations, Map' (Map' Location')))
  naming_def_1 i a (g, z) =
    case a of
      Basic_def_1 (Name h j) b x e -> (\(f, _) -> (Basic_def_2 h j b x e, (f, z))) <$> naming_name i (Name h j) g
      Instance_1 b (Name r3 k) (Name d l, f) h e ->
        let
          u = Library (Location_1 i b)
          m n = Right (Instance_2 b (Name r3 k) (Name d l, f) h e, (g, n z))
        in
          case Data.Map.lookup k z of
            Nothing -> m (insert k (singleton l u))
            Just w ->
              case Data.Map.lookup l w of
                Nothing -> m (insert k (insert l u w))
                Just t -> Left (location_err ("instances of " ++ k ++ " " ++ l) t b)
  naming_def_2 :: String -> Def_2 -> (Locations, Locations) -> Err Def_3
  naming_def_2 j a (b0, b1) =
    case a of
      Basic_def_2 k c d f g -> (\(_, i) -> Basic_def_3 k c i f) <$> naming_kinds' j (b0, d) <*> naming_expression j g b1
      Instance_2 f c (d, g) k e -> (\(_, i) -> Instance_3 f c (d, i) k) <$> naming_patterns j g b0 <*> naming_nameexprs j b1 e
  naming_defs_1 :: String -> [Def_1] -> (Locations, Map' (Map' Location')) -> Err ([Def_2], (Locations, Map' (Map' Location')))
  naming_defs_1 a b c =
    case b of
      [] -> Right ([], c)
      d : e -> naming_def_1 a d c >>= \(f, g) -> first ((:) f) <$> naming_defs_1 a e g
  naming_defs_2 :: String -> [Def_2] -> (Locations, Locations) -> Err [Def_3]
  naming_defs_2 = naming_list' naming_def_2
  naming_expression :: String -> Expression_9 -> Locations -> Err Expression_1
  naming_expression g a b =
    case a of
      Application_expression_9 c d -> naming_application g b (naming_expression g c b) d
      Branch_expression_9 c d e h ->
        (
          (\i -> \(j, k) -> Branch_expression_1 c i j k) <$>
          naming_expression g d b <*>
          (naming_pattern g e b >>= \(i, j) -> (,) j <$> naming_expression g h i))
      Char_expression_9 c -> Right (Char_expression_1 c)
      Function_expression_9 c d -> naming_fun g b c d
      Int_expression_9 c -> Right (Int_expression_1 c)
      Match_expression_9 c d -> Match_expression_1 <$> naming_expression g c b <*> naming_cases g d b
      Modular_expression_9 c -> Right (Modular_expression_1 c)
      Name_expression_9 c -> Right (Name_expression_1 c)
  naming_fields :: String -> [(Name, Type_8)] -> Locations -> Err (Locations, [(String, Type_8)])
  naming_fields = naming_arguments naming_name
  naming_form :: String -> Form_6 -> Locations -> Err (Locations, Form_1)
  naming_form a (Form_6 b c d e) f = second (\g -> Form_1 c g e) <$> naming_name a (Name b d) f
  naming_forms :: String -> [Form_6] -> Locations -> Err (Locations, [Form_1])
  naming_forms = naming_list naming_form
  naming_fun :: String -> Locations -> Pat_2 -> Expression_9 -> Err Expression_1
  naming_fun x b z w = naming_pat x z b >>= \(a, c) -> Function_expression_1 c <$> naming_expression x w a
  naming_kinds :: String -> (Locations, [(Name, Kind_0)]) -> Err (Locations, [(String, Kind_0)])
  naming_kinds a (b, c) = naming_arguments naming_name a c b
  naming_kinds' :: String -> (Locations, Kinds_constraints) -> Err (Locations, Kinds_constraints')
  naming_kinds' a (b, Kinds_constraints c d) = second (\e -> Kinds_constraints' e d) <$> naming_kinds a (b, c)
  naming_list' :: (String -> t -> u -> Err v) -> String -> [t] -> u -> Err [v]
  naming_list' a g b c =
    case b of
      [] -> Right []
      d : e -> a g d c >>= \f -> (:) f <$> naming_list' a g e c
  naming_method_0 :: String -> Method_9 -> Locations -> Err (Locations, Method_1)
  naming_method_0 a (Method_9 b c d) e = second (\f -> Method_1 f c d) <$> naming_name a b e
  naming_method_1 :: String -> Method_1 -> Locations -> Err Method_2
  naming_method_1 a (Method_1 b c d) e = (\(_, f) -> Method_2 b f d) <$> naming_kinds' a (e, c)
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
  naming_nameexprs :: String -> Locations -> [(Name, Expression_9)] -> Err [(Name, Expression_1)]
  naming_nameexprs a b c =
    case c of
      [] -> Right []
      (d, e) : f -> naming_expression a e b >>= \g -> (:) (d, g) <$> naming_nameexprs a b f
  naming_names' :: String -> Locations -> [Name] -> Err [String]
  naming_names' a b c =
    case c of
      [] -> Right []
      d : e -> naming_name a d b >>= \(f, g) -> (:) g <$> naming_names' a f e
  naming_ops :: String -> Locations -> [Name] -> Err Locations
  naming_ops a b c =
    case c of
      [] -> Right b
      Name d e : i ->
        case Data.Map.lookup e b of
          Nothing -> naming_ops a (insert e (Library (Location_1 a d)) b) i
          Just g -> Left (location_err ("definitions of " ++ e) g d)
  naming_pat :: String -> Pat_2 -> Locations -> Err (Locations, Pat')
  naming_pat a c d =
    case c of
      Application_pat_2 b e -> second (\h -> Application_pat' b h) <$> naming_pats a e d
      Blank_pat_2 -> Right (d, Blank_pat')
      Name_pat_2 b -> second Name_pat' <$> naming_name a b d
  naming_pats :: String -> [Pat_2] -> Locations -> Err (Locations, [Pat'])
  naming_pats a b c =
    case b of
      [] -> Right (c, [])
      d : e -> naming_pat a d c >>= \(g, h) -> second ((:) h) <$> naming_pats a e g
--------------------------------------------------------------------------------------------------------------------------------