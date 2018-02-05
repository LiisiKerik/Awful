-----------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Naming where
  import Data.Bifunctor
  import Data.Map
  import Standard
  import Tokenise
  import Tree
  data Class_1 = Class_1 String (Name, Kind_0) [Method_1] deriving Show
  data Class_2 = Class_2 String (String, Kind_0) [Method_2] deriving Show
  data Data_branch_1 = Algebraic_data_1 [Form_1] | Struct_data_1 [(String, Type_0)] deriving Show
  data Data_1 = Data_1 String [(Name, Kind_0)] Data_branch_1 deriving Show
  data Data_2 = Data_2 String [(String, Kind_0)] Data_branch_1 deriving Show
{-
  data Def_branch_2 =
    Basic_def_2 String [Argument_tree Name_tree Kind] [Constraint_0] Type_0 Expression_tree |
    Instance_def_2 Name_tree Name_tree [Pattern_tree] [Constraint_0] Expression_tree
      deriving Show
  data Def_branch_3 =
    Basic_def_3 String [Argument_tree String Kind] [Constraint_0] Type_0 Expression_tree_1 |
    Instance_def_3 Name_tree Name_tree [Pattern_branch] [Constraint_0] Expression_tree_1
      deriving Show
-}
  data Def_2 =
    Basic_def_2 Location_0 String [(Name, Kind_0)] Type_0 Expression_0 |
    Instance_2 Location_0 Name Name [Pattern_1] [Constraint_0] [(Name, Expression_0)]
      deriving Show
  data Def_3 =
    Basic_def_3 Location_0 String [(String, Kind_0)] Type_0 Expression_1 |
    Instance_3 Location_0 Name Name [Pattern_0] [Constraint_0] [(Name, Expression_1)]
      deriving Show
  data Expression_branch_1 =
    Application_expression_1 Expression_1 Expression_1 |
    Char_expression_1 Char |
    Function_expression_1 Pattern_0 Expression_1 |
    Int_expression_1 Integer |
    Match_expression_1 Expression_1 Matches_1 |
    Name_expression_1 String
      deriving Show
  data Expression_1 = Expression_1 Location_0 Expression_branch_1 deriving Show
  data Form_1 = Form_1 String [Type_0] deriving Show
  data Location' = Language | Library Location_1 deriving Show
  type Locations = Map' Location'
  type Map' t = Map String t
  data Match_Algebraic_1 = Match_Algebraic_1 Name [Pattern_0] Expression_1 deriving Show
  data Match_char_1 = Match_char_1 Char Expression_1 deriving Show
  data Match_Int_1 = Match_Int_1 Integer Expression_1 deriving Show
  data Matches_1 =
    Matches_Algebraic_1 [Match_Algebraic_1] (Maybe (Location_0, Expression_1)) |
    Matches_char_1 [Match_char_1] Expression_1 |
    Matches_Int_1 [Match_Int_1] Expression_1
      deriving Show
  data Method_1 = Method_1 String [(Name, Kind_0)] Type_0 deriving Show
  data Method_2 = Method_2 String [(String, Kind_0)] Type_0 deriving Show
{-
  data Tree_3 = Tree_3 [Data_tree_1] [Abstract_tree_1] [Def_branch_2] deriving Show
  data Tree_4 = Tree_4 [Data_tree_2] [Abstract_tree_2] [Def_branch_3] deriving Show
-}
  data Tree_4 = Tree_4 [Data_1] [Class_1] [Def_2] deriving Show
  data Tree_5 = Tree_5 [Data_2] [Class_2] [Def_3] deriving Show
  add :: Ord t => Map t u -> t -> u -> Either u (Map t u)
  add x y z = (case w of
    Just z' -> Left z'
    Nothing -> Right x') where
      (w, x') = insertLookupWithKey (return return) y z x
  location_err :: String -> Location' -> Location_1 -> String
  location_err a c d = "Conflicting " ++ a ++ (case c of
    Language -> " in the language"
    Library e -> location e) ++ " and" ++ location' d
  naming :: String -> Tree_2 -> Locations -> Err (Locations, Tree_5)
  naming f a b = naming_1 f a b >>= \(c, d) -> ((,) c) <$> naming_2 f d c
{-
  naming_abstract_0 :: Abstract_tree_0 -> Locations -> Err (Abstract_tree_1, Locations)
  naming_abstract_0 (Abstract_tree_0 a b u c d) e = (\(t, f) -> (Abstract_tree_1 f b u c d, t)) <$> naming_name a e
  naming_abstracts_0 :: [Abstract_tree_0] -> Locations -> Err ([Abstract_tree_1], Locations)
  naming_abstracts_0 a b = case a of
    [] -> Right ([], b)
    c : d -> naming_abstract_0 c b >>= \(e, f) -> first ((:) e) <$> naming_abstracts_0 d f
  naming_abstract_1 :: Abstract_tree_1 -> Locations -> Err (Abstract_tree_2)
  naming_abstract_1 (Abstract_tree_1 a b t c d) e =
    naming_name b e >>= \(f, g) -> flip (Abstract_tree_2 a g t) d <$> naming_arguments' naming_name c f
  naming_abstracts_1 :: [Abstract_tree_1] -> Locations -> Err [Abstract_tree_2]
  naming_abstracts_1 a b = case a of
    [] -> Right []
    c : d -> naming_abstract_1 c b >>= \e -> ((:) e) <$> naming_abstracts_1 d b
-}
  naming_args :: String -> [(Name, t)] -> Locations -> Err [(String, t)]
  naming_args a b c = case b of
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
  naming_1 :: String -> Tree_2 -> Locations -> Err (Locations, Tree_4)
  naming_1 f (Tree_2 a g b) c =
    (
      naming_datas_1 f a c >>=
      \(d, e) -> naming_classes_0 f g d >>= \(d', e') -> (\(h, i) -> (i, Tree_4 e e' h)) <$> naming_defs_1 f b d')
  naming_2 :: String -> Tree_4 -> Locations -> Err Tree_5
  naming_2 e (Tree_4 a f b) c =
    naming_datas_2 e a c >>= \d -> naming_classes_1 e f c >>= \g -> Tree_5 d g <$> naming_defs_2 e b c
  naming_class_0 :: String -> Class_0 -> Locations -> Err (Locations, Class_1)
  naming_class_0 a (Class_0 b c d) e = naming_name a b e >>= \(f, g) -> second (Class_1 g c) <$> naming_methods_0 a d f
  naming_class_1 :: String -> Class_1 -> Locations -> Err Class_2
  naming_class_1 a (Class_1 b (c, d) e) f =
    naming_name a c f >>= \(i, g) -> Class_2 b (g, d) <$> naming_methods_1 a e i
  naming_classes_0 :: String -> [Class_0] -> Locations -> Err (Locations, [Class_1])
  naming_classes_0 a b c = case b of
    [] -> Right (c, [])
    d : e -> naming_class_0 a d c >>= \(f, g) -> second ((:) g) <$> naming_classes_0 a e f
  naming_classes_1 :: String -> [Class_1] -> Locations -> Err [Class_2]
  naming_classes_1 a b c = case b of
    [] -> Right []
    d : e -> naming_class_1 a d c >>= \g -> (:) g <$> naming_classes_1 a e c
  naming_data_1 :: String -> Data_0 -> Locations -> Err (Locations, Data_1)
  naming_data_1 g (Data_0 a b c) d =
    naming_name g a d >>= \(e, f) -> second (Data_1 f b) <$> naming_data_branches g c e
  naming_data_2 :: String -> Data_1 -> Locations -> Err Data_2
  naming_data_2 f (Data_1 a b c) d = (\e -> Data_2 a e c) <$> naming_arguments' f naming_name b d
  naming_data_branches :: String -> Data_branch_0 -> Locations -> Err (Locations, Data_branch_1)
  naming_data_branches c a = case a of
    Algebraic_data_0 b -> naming_forms c b
    Struct_data_0 b -> naming_fields c b
  naming_datas_1 :: String -> [Data_0] -> Locations -> Err (Locations, [Data_1])
  naming_datas_1 = naming_list naming_data_1
  naming_datas_2 :: String -> [Data_1] -> Locations -> Err [Data_2]
  naming_datas_2 f a b = case a of
    [] -> Right []
    c : d -> naming_data_2 f c b >>= \e -> (:) e <$> naming_datas_2 f d b
  naming_def_1 :: String -> Def_1 -> Locations -> Err (Def_2, Locations)
  naming_def_1 i a g = case a of
    Basic_def_1 c @ (Name h j) b d e -> (\(f, _) -> (Basic_def_2 h j b d e, f)) <$> naming_name i c g
    Instance_1 b c d f h e -> Right (Instance_2 b c d f h e, g)
  naming_def_2 :: String -> Def_2 -> Locations -> Err Def_3
  naming_def_2 j a b = case a of
    Basic_def_2 k c d f g ->
      naming_arguments naming_name j d b >>= \(h, i) -> Basic_def_3 k c i f <$> naming_expression j g h
    Instance_2 f c d g k e -> naming_patterns j g b >>= \(h, i) -> Instance_3 f c d i k <$> naming_nameexprs j h e
  naming_defs_1 :: String -> [Def_1] -> Locations -> Err ([Def_2], Locations)
  naming_defs_1 a b c = case b of
    [] -> Right ([], c)
    d : e -> naming_def_1 a d c >>= \(f, g) -> first ((:) f) <$> naming_defs_1 a e g
  naming_defs_2 :: String -> [Def_2] -> Locations -> Err [Def_3]
  naming_defs_2 = naming_list' naming_def_2
  naming_expression :: String -> Expression_0 -> Locations -> Err Expression_1
  naming_expression e (Expression_0 a c) d = Expression_1 a <$> naming_expression_branch e c d
  naming_expression_branch :: String -> Expression_branch_0 -> Locations -> Err Expression_branch_1
  naming_expression_branch g a b = case a of
    Application_expression_0 c d -> naming_expression g c b >>= \e -> Application_expression_1 e <$> naming_expression g d b
    Char_expression_0 c -> Right (Char_expression_1 c)
    Function_expression_0 c d -> naming_pattern g c b >>= \(e, f) -> Function_expression_1 f <$> naming_expression g d e
    Int_expression_0 c -> Right (Int_expression_1 c)
    Match_expression_0 c d -> naming_expression g c b >>= \e -> Match_expression_1 e <$> naming_matches g d b
    Name_expression_0 c -> Right (Name_expression_1 c)
  naming_fields :: String -> [(Name, Type_0)] -> Locations -> Err (Locations, Data_branch_1)
  naming_fields b a c = second Struct_data_1 <$> naming_arguments naming_name b a c
  naming_form :: String -> Form_0 -> Locations -> Err (Locations, Form_1)
  naming_form d (Form_0 a b) c = second (flip Form_1 b) <$> naming_name d a c
  naming_forms :: String -> [Form_0] -> Locations -> Err (Locations, Data_branch_1)
  naming_forms c a b = second Algebraic_data_1 <$> naming_list naming_form c a b
  naming_list :: (String -> t -> u -> Err (u, v)) -> String -> [t] -> u -> Err (u, [v])
  naming_list a h b c = case b of
    [] -> Right (c, [])
    d : e -> a h d c >>= \(f, g) -> second ((:) g) <$> naming_list a h e f
  naming_list' :: (String -> t -> u -> Err v) -> String -> [t] -> u -> Err [v]
  naming_list' a g b c = case b of
    [] -> Right []
    d : e -> a g d c >>= \f -> (:) f <$> naming_list' a g e c
  naming_match_algebraic :: String -> Match_Algebraic_0 -> Locations -> Err Match_Algebraic_1
  naming_match_algebraic a (Match_Algebraic_0 b c d) e =
    naming_patterns a c e >>= \(f, g) -> Match_Algebraic_1 b g <$> naming_expression a d f
  naming_match_char :: String -> Match_char_0 -> Locations -> Err Match_char_1
  naming_match_char a (Match_char_0 b c) d = Match_char_1 b <$> naming_expression a c d
  naming_match_int :: String -> Match_Int_0 -> Locations -> Err Match_Int_1
  naming_match_int a (Match_Int_0 b c) d = Match_Int_1 b <$> naming_expression a c d
  naming_matches :: String -> Matches_0 -> Locations -> Err Matches_1
  naming_matches a b c =
    let
      j k = naming_expression a k c
    in case b of
      Matches_Algebraic_0 d e -> naming_matches_algebraic a d c >>= \f ->
        let
          g = Matches_Algebraic_1 f
        in case e of
          Just (x, h) -> (\i -> g (Just (x, i))) <$> j h
          Nothing -> Right (g Nothing)
      Matches_char_0 d e -> naming_matches_char a d c >>= \f -> Matches_char_1 f <$> j e
      Matches_Int_0 d e -> naming_matches_int a d c >>= \f -> Matches_Int_1 f <$> j e
  naming_matches_algebraic :: String -> [Match_Algebraic_0] -> Locations -> Err [Match_Algebraic_1]
  naming_matches_algebraic a b c = case b of
    [] -> Right []
    d : e -> naming_match_algebraic a d c >>= \f -> (:) f <$> naming_matches_algebraic a e c
  naming_matches_char :: String -> [Match_char_0] -> Locations -> Err [Match_char_1]
  naming_matches_char a b c = case b of
    [] -> Right []
    d : e -> naming_match_char a d c >>= \f -> (:) f <$> naming_matches_char a e c
  naming_matches_int :: String -> [Match_Int_0] -> Locations -> Err [Match_Int_1]
  naming_matches_int a b c = case b of
    [] -> Right []
    d : e -> naming_match_int a d c >>= \f -> (:) f <$> naming_matches_int a e c
  naming_method_0 :: String -> Method -> Locations -> Err (Locations, Method_1)
  naming_method_0 a (Method b c d) e = second (\f -> Method_1 f c d) <$> naming_name a b e
  naming_method_1 :: String -> Method_1 -> Locations -> Err Method_2
  naming_method_1 a (Method_1 b c d) e = (\f -> Method_2 b f d) <$> naming_args a c e
  naming_methods_0 :: String -> [Method] -> Locations -> Err (Locations, [Method_1])
  naming_methods_0 a b c = case b of
    [] -> Right (c, [])
    d : e -> naming_method_0 a d c >>= \(f, g) -> second ((:) g) <$> naming_methods_0 a e f
  naming_methods_1 :: String -> [Method_1] -> Locations -> Err [Method_2]
  naming_methods_1 a b c = case b of
    [] -> Right []
    d : e -> naming_method_1 a d c >>= \g -> (:) g <$> naming_methods_1 a e c
  naming_name :: String -> Name -> Locations -> Err (Locations, String)
  naming_name f (Name a c) d =
    bimap (flip (location_err ("definitions of " ++ c)) (Location_1 f a)) (flip (,) c) (add d c (Library (Location_1 f a)))
  naming_nameexprs :: String -> Locations -> [(Name, Expression_0)] -> Err [(Name, Expression_1)]
  naming_nameexprs a b c = case c of
    [] -> Right []
    (d, e) : f -> naming_expression a e b >>= \g -> (:) (d, g) <$> naming_nameexprs a b f
  naming_names :: String -> [(Name, t)] -> Locations -> Err (Locations, [(String, t)])
  naming_names a b c = case b of
    [] -> Right (c, [])
    (d, e) : f -> naming_name a d c >>= \(g, h) -> second ((:) (h, e)) <$> naming_names a f g
  naming_pattern :: String -> Pattern_1 -> Locations -> Err (Locations, Pattern_0)
  naming_pattern f (Pattern_1 a c) d = case c of
    Blank_pattern -> Right (d, Blank_pattern)
    Name_pattern e -> second Name_pattern <$> naming_name f (Name a e) d
  naming_patterns :: String -> [Pattern_1] -> Locations -> Err (Locations, [Pattern_0])
  naming_patterns = naming_list naming_pattern
-----------------------------------------------------------------------------------------------------------------------------