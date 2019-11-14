--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Standard (
  Arrow_1 (..),
  Binding_1 (..),
  Class_1 (..),
  Constructor_1 (..),
  Def_or_instance_1 (..),
  Data_branch_1 (..),
  Data_1 (..),
  Field_1 (..),
  File_2 (..),
  Method_1 (..),
  Term_1 (..),
  Term_pattern_3 (..),
  Type_2 (..),
  Type_3 (..),
  Type_pattern_2 (..),
  standard_file,
  standard_term) where
  import Data.Bifunctor
  import Data.Map
  import Data.Set
  import Dictionary
  import Errors
  import Modular
  import Tree
  data Arrow_1 = Arrow_1 Term_pattern_3 Term_1 deriving Show
  data Binding_1 = Binding_1 Name Term_1 deriving Show
  data Class_1 = Class_1 Line_and_char String Type_variable_0 [Constraint_0] [Method_1] deriving Show
  data Constructor_1 = Constructor_1 Line_and_char Status String [Type_3] deriving Show
  data Data_1 = Data_1 Line_and_char Status String [Type_variable_0] Data_branch_1 deriving Show
  data Data_branch_1 =
    Algebraic_data_1 [Constructor_1] |
    Branch_data_1 Name (Data_branch_1, (Name, Data_branch_1)) |
    Struct_data_1 Line_and_char Status String [Field_1]
      deriving Show
  data Def_or_instance_1 =
    Def_1 Line_and_char Status String [Type_variable_0] [Constraint_0] Type_2 Term_1 |
    Instance_1 Line_and_char Name Type_pattern_2 [Constraint_0] [Binding_1]
      deriving Show
  data Field_1 = Field_1 Name Type_3 deriving Show
  data Method_1 = Method_1 Name [Type_variable_0] [Constraint_0] Type_3 deriving Show
  data Operator_2 = Operator_2 Line_and_char Operator_0 deriving Show
  data Term_1 =
    Application_term_1 Term_1 Term_1 |
    Arrow_term_1 Arrow_1 |
    Branch_term_1 Name (Term_1, (Type_pattern_0, Term_1)) |
    Int_term_1 Integer |
    Let_term_1 Term_pattern_3 Term_1 Term_1 |
    Match_term_1 Term_1 [Arrow_1] |
    Modular_term_1 Modular_0 |
    Name_term_1 Name
      deriving Show
  data Term_pattern_2 =
    Application_term_pattern_2 Term_pattern_2 Term_pattern_2 |
    Blank_term_pattern_2 |
    Int_term_pattern_2 Integer |
    Modular_term_pattern_2 Modular_0 |
    Name_term_pattern_2 Name
      deriving Show
  data Term_pattern_3 =
    Blank_term_pattern_3 |
    Int_term_pattern_3 Integer |
    Modular_term_pattern_3 Modular_0 |
    Name_term_pattern_3 Name |
    Struct_term_pattern_3 Name [Term_pattern_3]
      deriving Show
  data Term_pattern_4 =
    Arrow_term_pattern_4 Name [Term_pattern_3] | Name_term_pattern_4 Name | Simple_term_pattern_4 Term_pattern_3
      deriving Show
  data File_2 = File_2 [Name] [Name] [Data_1] [Class_1] [Def_or_instance_1] deriving Show
  data Type_2 = Application_type_2 Type_2 Type_2 | Name_type_2 Name deriving Show
  data Type_3 = Type_3 Line_and_char Type_2 deriving Show
  data Type_pattern_2 = Type_pattern_2 Name [Type_pattern_0] deriving Show
  class Operator t where
    get_precedence :: t -> Integer
  instance Operator Operator_0 where
    get_precedence (Operator_0 _ precedence _) = precedence
  instance Operator Operator_2 where
    get_precedence (Operator_2 _ operator) = get_precedence operator
  arrow_term :: Term_pattern_3 -> Term_1 -> Term_1
  arrow_term term_pattern term = Arrow_term_1 (Arrow_1 term_pattern term)
  arrow_type :: Type_2 -> Type_2 -> Type_2
  arrow_type typ = Application_type_2 (Application_type_2 (name_type "Arrow") typ)
  check_modular :: String -> Modular_1 -> Err Modular_0
  check_modular file_name (Modular_1 line_and_char x) =
    case valid_modular x of
      False -> Left (Invalid_modular x file_name line_and_char)
      True -> Right x
  gather_operators :: [Operator_1] -> Dictionary Operator_0 -> (Dictionary Operator_0, (Dictionary Operator_0, [Name]))
  gather_operators operator_list operator_dictionary_0 =
    let
      operator_dictionary_1 = Data.Map.fromList (operator_entry <$> operator_list)
    in
      (Data.Map.union operator_dictionary_1 operator_dictionary_0, (operator_dictionary_1, operator_name <$> operator_list))
  name_type :: String -> Type_2
  name_type x = Name_type_2 (Name (Line_and_char 0 0) x)
  nat_type :: Integer -> Type_2
  nat_type n =
    case n of
      0 -> name_type "Zero"
      _ -> Application_type_2 (name_type "Next") (nat_type (n - 1))
  operator_entry :: Operator_1 -> (String, Operator_0)
  operator_entry (Operator_1 _ x operator) = (x, operator)
  operator_name :: Operator_1 -> Name
  operator_name (Operator_1 line_and_char x _) = Name line_and_char x
  split_by :: Integer -> (t, [(Operator_2, t)]) -> ((t, [(Operator_2, t)]), [(Operator_2, (t, [(Operator_2, t)]))])
  split_by precedence (term_0, sequence_0) =
    case sequence_0 of
      [] -> ((term_0, []), [])
      (operator, term_1) : sequence_1 ->
        let
          ((term_2, sequence_2), sequence') = split_by precedence (term_1, sequence_1)
        in
          case precedence == get_precedence operator of
            False -> ((term_0, (operator, term_2) : sequence_2), sequence')
            True -> ((term_0, []), (operator, (term_2, sequence_2)) : sequence')
  standard_file ::
    (
      String ->
      Dictionary Operator_0 ->
      Dictionary Operator_0 ->
      File_0 ->
      Set String ->
      Err (Set String, (Dictionary Operator_0, Dictionary Operator_0, File_2)))
  standard_file
    file_name
    old_type_operators
    old_term_operators
    (File_0 type_operators term_operators datas classes defs_and_instances)
    old_constructors =
      let
        (all_type_operators, (new_type_operators, type_operators')) = gather_operators type_operators old_type_operators
        (all_term_operators, (new_term_operators, term_operators')) = gather_operators term_operators old_term_operators
      in
        (
          standard_datas file_name all_type_operators datas >>=
          \(new_constructors, datas') ->
            let
              all_constructors = Data.Set.union old_constructors new_constructors
            in
              (
                (\classes' -> \defs_and_instances' ->
                  (
                    all_constructors,
                    (
                      new_type_operators,
                      new_term_operators,
                      File_2 type_operators' term_operators' datas' classes' defs_and_instances'))) <$>
                traverse (std_cls file_name all_type_operators) classes <*>
                traverse
                  (standard_def_or_instance file_name all_type_operators all_term_operators all_constructors)
                  defs_and_instances))
  standard_argument ::
    (
      String ->
      Dictionary Operator_0 ->
      Dictionary Operator_0 ->
      Set String ->
      (Term_pattern_1, Type_0) ->
      Err (Term_pattern_3, Type_2))
  standard_argument file_name type_operators term_operators constructors (term_pattern, typ) =
    (
      (,) <$>
      standard_simple_term_pattern file_name term_operators constructors term_pattern <*>
      standard_type' file_name type_operators typ)
  standard_constructor :: String -> Dictionary Operator_0 -> Constructor_0 -> Err (String, Constructor_1)
  standard_constructor file_name type_operators (Constructor_0 line_and_char status x types) =
    (\types' -> (x, Constructor_1 line_and_char status x types')) <$> traverse (standard_type file_name type_operators) types
  standard_data :: String -> Dictionary Operator_0 -> Data_0 -> Err (Set String, Data_1)
  standard_data file_name type_operators (Data_0 line_and_char status x type_variables dat) =
    second (Data_1 line_and_char status x type_variables) <$> standard_data' file_name type_operators dat
  standard_data' :: String -> Dictionary Operator_0 -> Data_branch_0 -> Err (Set String, Data_branch_1)
  standard_data' file_name type_operators data_branch_0 =
    case data_branch_0 of
      Algebraic_data_0 constructors ->
        (
          (\constructors' -> (Data.Set.fromList (fst <$> constructors'), Algebraic_data_1 (snd <$> constructors'))) <$>
          traverse (standard_constructor file_name type_operators) constructors)
      Branch_data_0 n (data_1, (n', data_2)) ->
        (
          (\(constructors_0, data'_1) -> \(constructors_1, data'_2) ->
            (Data.Set.union constructors_0 constructors_1, Branch_data_1 n (data'_1, (n', data'_2)))) <$>
          standard_data' file_name type_operators data_1 <*>
          standard_data' file_name type_operators data_2)
      Struct_data_0 line_and_char status x fields ->
        (
          (\fields' -> (Data.Set.singleton x, Struct_data_1 line_and_char status x fields')) <$>
          traverse (standard_field file_name type_operators) fields)
  standard_datas :: String -> Dictionary Operator_0 -> [Data_0] -> Err (Set String, [Data_1])
  standard_datas file_name b c = (\d -> (Data.Set.unions (fst <$> d), snd <$> d)) <$> traverse (standard_data file_name b) c
  standard_def_or_instance ::
    String -> Dictionary Operator_0 -> Dictionary Operator_0 -> Set String -> Def_or_instance_0 -> Err Def_or_instance_1
  standard_def_or_instance file_name type_operators term_operators constructors def_or_instance =
    case def_or_instance of
      Def_0 line_and_char status name type_variables constraints arguments typ term ->
        (
          (\arguments' -> \typ' -> \term' ->
            Def_1
              line_and_char
              status
              name
              type_variables
              constraints
              (Prelude.foldr arrow_type typ' (snd <$> arguments'))
              (Prelude.foldr arrow_term term' (fst <$> arguments'))) <$>
          traverse (standard_argument file_name type_operators term_operators constructors) arguments <*>
          standard_type' file_name type_operators typ <*>
          standard_term file_name term_operators constructors term)
      Instance_0 line_and_char cls type_pattern constraints defs ->
        (
          (\type_pattern' -> Instance_1 line_and_char cls type_pattern' constraints) <$>
          standard_type_pattern file_name type_operators type_pattern <*>
          traverse (std_inst file_name (constructors, term_operators)) defs)
  standard_field :: String -> Dictionary Operator_0 -> Field_0 -> Err Field_1
  standard_field file_name type_operators (Field_0 x typ) = Field_1 x <$> standard_type file_name type_operators typ
  standard_operators ::
    String -> (t -> Err u) -> (u -> u -> u) -> (Name -> u) -> String -> Dictionary Operator_0 -> t -> [(Name, t)] -> Err u
  standard_operators a b c d e f g h =
    (,) <$> b g <*> traverse (standard_sequence e a b f) h >>= standard_operators' e a (\k -> \l -> c (c (d k) l))
  standard_operators' :: String -> String -> (Name -> t -> t -> t) -> (t, [(Operator_2, t)]) -> Err t
  standard_operators' l k a (c, d) =
    case d of
      [] -> Right c
      _ ->
        let
          (e, g) = split_by (maximum (get_precedence <$> fst <$> d)) (c, d)
          h i = all (== i) ((\(Operator_2 _ (Operator_0 _ _ f), _) -> f) <$> g)
          (Operator_2 m _, _) = head g
          (Operator_2 n _, _) = last g
        in
          (
            (,) <$> standard_operators' l k a e <*> traverse (traverse (standard_operators' l k a)) g >>=
            \(b, j) ->
              case (h Left_associativity, h Right_associativity) of
                (True, False) -> Right (Prelude.foldl (\o -> \(Operator_2 p (Operator_0 r _ _), q) -> a (Name p r) o q) b j)
                (False, True) ->
                  Right
                    (Prelude.foldr
                      (\(o, Operator_2 p (Operator_0 r _ _)) -> a (Name p r) o)
                      (snd (last j))
                      (zip (b : init (snd <$> j)) (fst <$> j)))
                _ -> Left (Mixed_operator_associativities k l m n))
  standard_sequence :: String -> String -> (t -> Err u) -> Dictionary Operator_0 -> (Name, t) -> Err (Operator_2, u)
  standard_sequence e m a f (Name b d, c) = (,) <$> (Operator_2 b <$> search (m ++ " operator") e f (Name b d)) <*> a c
  standard_simple_term_pattern :: String -> Dictionary Operator_0 -> Set String -> Term_pattern_1 -> Err Term_pattern_3
  standard_simple_term_pattern file_name term_operators constructors (Term_pattern_1 line_and_char term_pattern) =
    (
      standard_term_pattern' file_name term_operators constructors (Term_pattern_1 line_and_char term_pattern) >>=
      \term_pattern' ->
        case term_pattern' of
          Arrow_term_pattern_4 _ _ -> Left (Pattern_error file_name line_and_char)
          Name_term_pattern_4 x -> Right (Name_term_pattern_3 x)
          Simple_term_pattern_4 term_pattern'' -> Right term_pattern'')
  standard_term :: String -> Dictionary Operator_0 -> Set String -> Term_0 -> Err Term_1
  standard_term x c b d =
    case d of
      Application_term_0 e f ->
        Prelude.foldl Application_term_1 <$> standard_term x c b e <*> traverse (standard_term x c b) f
      Arrow_term_0 e -> Arrow_term_1 <$> std_case x (b, c) e
      Branch_term_0 e (f, (g, h)) ->
        (\i -> \k' -> Branch_term_1 e (i, (g, k'))) <$> standard_term x c b f <*> standard_term x c b h
      Int_term_0 e -> Right (Int_term_1 e)
      Let_term_0 e f -> std_let x (b, c) e f
      Match_term_0 e f -> Match_term_1 <$> standard_term x c b e <*> traverse (std_case x (b, c)) f
      Modular_term_0 e -> Modular_term_1 <$> check_modular x e
      Name_term_0 e -> Right (Name_term_1 e)
      Operators_term_0 e f -> standard_operators "term" (standard_term x c b) Application_term_1 Name_term_1 x c e f
  standard_term_pattern' :: String -> Dictionary Operator_0 -> Set String -> Term_pattern_1 -> Err Term_pattern_4
  standard_term_pattern' file_name term_operators constructors (Term_pattern_1 line_and_char term_pattern) =
    (
      standard_term_pattern_0 file_name term_operators term_pattern >>=
      standard_term_pattern_1 file_name line_and_char constructors)
  standard_term_pattern_0 :: String -> Dictionary Operator_0 -> Term_pattern_0 -> Err Term_pattern_2
  standard_term_pattern_0 file_name term_operators term_pattern_0 =
    case term_pattern_0 of
      Application_term_pattern_0 term_pattern_1 term_patterns ->
        (
          Prelude.foldl Application_term_pattern_2 <$>
          standard_term_pattern_0 file_name term_operators term_pattern_1 <*>
          traverse (standard_term_pattern_0 file_name term_operators) term_patterns)
      Blank_term_pattern_0 -> Right Blank_term_pattern_2
      Int_term_pattern_0 x -> Right (Int_term_pattern_2 x)
      Modular_term_pattern_0 x -> Modular_term_pattern_2 <$> check_modular file_name x
      Name_term_pattern_0 x -> Right (Name_term_pattern_2 x)
      Operators_term_pattern_0 term_pattern_1 operator_sequence ->
        (
          standard_operators
          "term"
          (standard_term_pattern_0 file_name term_operators)
          Application_term_pattern_2
          Name_term_pattern_2
          file_name
          term_operators
          term_pattern_1
          operator_sequence)
  standard_term_pattern_1 :: String -> Line_and_char -> Set String -> Term_pattern_2 -> Err Term_pattern_4
  standard_term_pattern_1 x j a b =
    case b of
      Application_term_pattern_2 c d ->
        (
          (
            (,) <$>
            standard_term_pattern_1 x j a c <*>
            (
              standard_term_pattern_1 x j a d >>=
              \e ->
                case e of
                  Arrow_term_pattern_4 _ _ -> Left (Pattern_error x j)
                  Name_term_pattern_4 f -> Right (Name_term_pattern_3 f)
                  Simple_term_pattern_4 f -> Right f)) >>=
          \(e, f) ->
            case e of
              Arrow_term_pattern_4 g h -> Right (Arrow_term_pattern_4 g (h ++ [f]))
              Name_term_pattern_4 g -> Right (Arrow_term_pattern_4 g [f])
              Simple_term_pattern_4 (Struct_term_pattern_3 g h) ->
                Right (Simple_term_pattern_4 (Struct_term_pattern_3 g (h ++ [f])))
              _ -> Left (Pattern_error x j))
      Blank_term_pattern_2 -> Right (Simple_term_pattern_4 Blank_term_pattern_3)
      Int_term_pattern_2 c -> Right (Simple_term_pattern_4 (Int_term_pattern_3 c))
      Modular_term_pattern_2 c -> Right (Simple_term_pattern_4 (Modular_term_pattern_3 c))
      Name_term_pattern_2 (Name c d) ->
        Right
          (case Data.Set.member d a of
            False -> Name_term_pattern_4 (Name c d)
            True -> Simple_term_pattern_4 (Struct_term_pattern_3 (Name c d) []))
  standard_type :: String -> Dictionary Operator_0 -> Type_1 -> Err Type_3
  standard_type file_name type_operators (Type_1 line_and_char typ) =
    Type_3 line_and_char <$> standard_type' file_name type_operators typ
  standard_type' :: String -> Dictionary Operator_0 -> Type_0 -> Err Type_2
  standard_type' m g b =
    case b of
      Application_type_0 c d -> Prelude.foldl Application_type_2 <$> standard_type' m g c <*> traverse (standard_type' m g) d
      Name_type_0 c -> Right (Name_type_2 c)
      Nat_type_0 n -> Right (nat_type n)
      Operators_type_0 a c -> standard_operators "type" (standard_type' m g) Application_type_2 Name_type_2 m g a c
  standard_type_pattern :: String -> Dictionary Operator_0 -> Type_pattern_1 -> Err Type_pattern_2
  standard_type_pattern file_name type_operators type_pattern =
    case type_pattern of
      Application_type_pattern_1 x type_pattern' -> Right (Type_pattern_2 x type_pattern')
      Operator_type_pattern_1 type_pattern'_0 (Name line_and_char x) type_pattern'_1 ->
        (
          (\(Operator_0 y _ _) -> Type_pattern_2 (Name line_and_char y) [type_pattern'_0, type_pattern'_1]) <$>
          search "type operator" file_name type_operators (Name line_and_char x))
  std_case :: String -> (Set String, Dictionary Operator_0) -> Arrow_0 -> Err Arrow_1
  std_case x (b, f) (Arrow_0 c e) = Arrow_1 <$> standard_simple_term_pattern x f b c <*> standard_term x f b e
  std_cls :: String -> Dictionary Operator_0 -> Class_0 -> Err Class_1
  std_cls m f (Class_0 a b e c d) = Class_1 a b e c <$> traverse (std_mthd m f) d
  std_inst :: String -> (Set String, Dictionary Operator_0) -> Binding_0 -> Err Binding_1
  std_inst file_name (b, c) (Binding_0 (Term_pattern_1 l d) e) =
    (
      (\(h, j) -> \k -> Binding_1 h (Prelude.foldr (\t0 -> \t1 -> Arrow_term_1 (Arrow_1 t0 t1)) k j)) <$>
      (
        standard_term_pattern' file_name c b (Term_pattern_1 l d) >>=
        \g ->
          case g of
            Arrow_term_pattern_4 i j -> Right (i, j)
            Name_term_pattern_4 i -> Right (i, [])
            Simple_term_pattern_4 _ -> Left (Pattern_error file_name l)) <*>
      standard_term file_name c b e)
  std_let :: String -> (Set String, Dictionary Operator_0) -> [Binding_0] -> Term_0 -> Err Term_1
  std_let z (b, c) d e =
    case d of
      [] -> standard_term z c b e
      Binding_0 f h : i ->
        (
          (\(j, m) -> \k -> \l ->
            Application_term_1
              (Arrow_term_1 (Arrow_1 j l))
              (Prelude.foldr (\t0 -> \t1 -> Arrow_term_1 (Arrow_1 t0 t1)) k m)) <$>
          (
            (\k ->
              case k of
                Arrow_term_pattern_4 l m -> (Name_term_pattern_3 l, m)
                Name_term_pattern_4 l -> (Name_term_pattern_3 l, [])
                Simple_term_pattern_4 l -> (l, [])) <$>
            standard_term_pattern' z c b f) <*>
          standard_term z c b h <*>
          std_let z (b, c) i e)
  std_mthd :: String -> Dictionary Operator_0 -> Method_0 -> Err Method_1
  std_mthd m d (Method_0 b a c e) = Method_1 b a c <$> standard_type m d e
--------------------------------------------------------------------------------------------------------------------------------