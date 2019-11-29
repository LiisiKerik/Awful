{-
tests
"requires instance or constraint" -> "requires instance" / "requires constraint"
internal: make the system of specifying built-in algebraic data types and things better and safer
remove special semantics of missing pattern match arguments?
move modular checking to parser?
allow to make algebraic data types and branching data types hidden?
expand restricted constructor thing to algebraic and branching data types and definitions (and class methods?)
check for duplicate constraints everywhere
take some things out of imported-only contexts and put them into global program context
  (they can result in unfound things and should be included everywhere transitively)
algebraic pattern matching - check in Typing module that all constructors are actually legit
check if things are put correctly into local / global context
add Modular arithmetic methods to Nonzero class; this also prevents the user for extending Nonzero to Zero!
div - give error not only when divisor is zero but also when divisor is negative!
Check that incomplete patterns don't crash but give a nice error during execution of the program
circular inheritance checking should be moved into second pass of class checking?
remove constraints from class method signatures?
move method type checking into the second phase of class checking
-}
--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Typing (
  Arrow_4 (..),
  Class_5 (..),
  Class_6 (..),
  Constructor_3 (..),
  Implementation (..),
  Polymorphic_term (..),
  Term_4 (..),
  Term_pattern_6 (..),
  Type_4 (..),
  Polymorphic_type (..),
  context_union,
  defs,
  init_type_context,
  locations_0,
  locations_1,
  locations_2,
  type_expr',
  typestring,
  typing) where
  import Control.Monad
  import Data.Bifunctor
  import Data.List
  import Data.Map
  import Data.Maybe
  import Data.Set
  import Dictionary
  import Errors
  import Modular
  import Naming
  import Standard
  import Tree
  data Arrow_3 = Arrow_3 Term_pattern_6 Term_3 deriving Show
  data Arrow_4 = Arrow_4 Term_pattern_6 Term_4 deriving Show
  data Branch_equations = Branch_equations String (Equations, (String, Equations)) deriving Show
  data Class_4 = Class_4 String Type_variable_1 [Name] [Method_3] deriving Show
  data Class_5 = Class_5 Kind_0 [String] deriving Show
  data Class_6 = Class_6 Type_variable_1 [String] [Method_4] deriving Show
  data Constraint_1 = Constraint_1 String String deriving Show
  data Constructor_3 = Constructor_3 [Type_variable_1] [Type_4] Type_4 deriving Show
{-
  data Def_or_instance_2 =
    Def_2 Line_and_char Status String [Type_variable_1] [Constraint_0] Type_2 Term_2 |
    Instance_2 Line_and_char Name Type_pattern_4 [Constraint_0] [Binding_2]
      deriving Show
-}
  data Def_or_instance_3 =
    Def_3 Line_and_char String [Type_variable_1] [Constraint_1] Type_4 Term_2 |
    Instance_3
      Line_and_char
      String
      [String]
      String
      String
      [Type_variable_1]
      Integer
      [(Name, Term_2, [Type_variable_1], Type_4)]
      Type_4
      [Constraint_1]
        deriving Show
  data Implementation = Add_Modular_term | Convert_Modular_term | Implementation [String] Term_4 | Times_Modular_term
    deriving Show
  data Method_4 = Method_4 String [Type_variable_1] Type_4 deriving Show
  data Term_pattern_6 =
    Blank_term_pattern_6 |
    Int_term_pattern_6 Integer |
    Modular_term_pattern_6 Integer |
    Name_term_pattern_6 String |
    Struct_term_pattern_6 String [Term_pattern_6]
      deriving Show
  data Equations = Equations [Integer] [(Type_6, Type_6)] [(String, (Name, Type_6))] [Branch_equations] deriving Show
  data Form_2 = Form_2 String [Type_4] deriving Show
  data Kind_1 = Arrow_kind_1 Kind_1 Kind_1 | Nat_kind_1 | Star_kind_1 | Var_kind_1 Integer deriving (Eq, Show)
  data Nat = Nxt Nat | Zr deriving (Eq, Ord, Show)
  data Polymorphic_term = Ad_hoc_term [String] (Dictionary Implementation) | Parametric_term [String] Term_4 deriving Show
  -- data Polymorphic_type = Polymorphic_type [Type_variable_1] (Maybe Constraint_1) [Constraint_1] Type_4 deriving Show
  data Polymorphic_type =
    Ad_hoc_type Type_variable_1 [Type_variable_1] String Type_4 | Parametric_type [Type_variable_1] [Constraint_1] Type_4
      deriving Show
  data Term_3 =
    Application_term_3 Term_3 Term_3 |
    Arrow_term_3 Term_pattern_6 Term_3 |
    Branch_term_3 String Term_3 Type_pattern_3 Term_3 |
    Glob_term_3 String (Maybe Type_6) [Type_6] |
    Int_term_3 Integer |
    Loc_term_3 String |
    Match_term_3 Term_3 [Arrow_3] |
    Modular_term_3 Integer
      deriving Show
  data Term_4 =
    Add_Int_term_4 Term_4 Term_4 |
    Add_Modular_term_4 Integer Term_4 Term_4 |
    Algebraic_term_4 String [Term_4] |
    Application_term_4 Term_4 Term_4 |
    Arrow_term_4 Arrow_4 |
    Branch_term_4 Type_4 Term_4 Type_pattern_3 Term_4 |
    Compare_Int_term_4 Term_4 Term_4 |
    Compare_Modular_term_4 Term_4 Term_4 |
    Convert_Modular_term_4 Integer Term_4 |
    Div_term_4 Term_4 Term_4 |
    Field_term_4 Int |
    Glob_term_4 String (Maybe Type_4) [Type_4] |
    Int_term_4 Integer |
    Inverse_Modular_term_4 Integer Term_4 |
    Loc_term_4 String |
    Match_term_4 Term_4 [Arrow_4] |
    Method_term_4 String Type_4 [Type_4] |
    Mod_term_4 Term_4 Term_4 |
    Modular_term_4 Integer |
    Times_Int_term_4 Term_4 Term_4 |
    Times_Modular_term_4 Integer Term_4 Term_4
      deriving Show
  data Type_4 = Application_type_4 Type_4 Type_4 | Name_type_4 String deriving Show
  data Type_6 = Application_type_6 Type_6 Type_6 | Name_type_6 String | Var_type_6 Integer deriving Show
  chain_constraints :: [String] -> Dictionary Class_5 -> Dictionary (Set String) -> String -> Dictionary (Set String)
  chain_constraints a b c e =
    case a of
      [] -> c
      d : h ->
        let
          Class_5 _ f = b ! d
        in
          chain_constraints
            (f ++ h)
            b
            (Data.Map.insert
              d
              (Data.Set.insert
                e 
                (case Data.Map.lookup d c of
                  Just g -> g
                  Nothing -> Data.Set.empty))
              c)
            e
  check_conditions ::
    (
      Dictionary Class_6 ->
      Dictionary (Dictionary [[String]]) ->
      String ->
      (String -> Err ()) ->
      String ->
      [String] ->
      [[String]] ->
      [String] ->
      Err ())
  check_conditions u0 m e s' e' e0 r' w0 =
    case w0 of
      [] -> Right ()
      q : w' ->
        let
          s = s' (e' ++ " assumes " ++ q)
        in
          case Data.Map.lookup q m of
            Just t ->
              case Data.Map.lookup e t of
                Just u ->
                  case constr_check ((\(Class_6 _ q0 _) -> q0) <$> u0) e0 u r' of
                    Just r0 -> s' ("it requires " ++ r0 ++ " due to constraints on " ++ q ++ " " ++ e)
                    Nothing -> check_conditions u0 m e s' e' e0 r' w'
                Nothing -> s
            Nothing -> s
  classes_0 :: Dictionary Class_6
  classes_0 =
    Data.Map.fromList
      [
        (
          "Field",
          Class_6
            (Type_variable_1 "T" Star_kind_0)
            ["Ring"]
            [Method_4 "Inverse" [] (function_type (Name_type_4 "T") (maybe_type (Name_type_4 "T")))]),
        ("Nonzero", Class_6 (Type_variable_1 "N" Nat_kind_0) [] []),
        (
          "Ord",
          Class_6
            (Type_variable_1 "T" Star_kind_0)
            []
            [Method_4 "Compare" [] (function_type (Name_type_4 "T") (function_type (Name_type_4 "T") comparison_type))]),
        (
          "Ring",
          Class_6
            (Type_variable_1 "T" Star_kind_0)
            []
            [
              Method_4 "Add" [] (function_type (Name_type_4 "T") (function_type (Name_type_4 "T") (Name_type_4 "T"))),
              Method_4 "Convert" [] (function_type int_type (Name_type_4 "T")),
              Method_4 "Times" [] (function_type (Name_type_4 "T") (function_type (Name_type_4 "T") (Name_type_4 "T")))])]
  classes_1 :: Dictionary Class_5
  classes_1 = (\(Class_6 (Type_variable_1 _ a) b _) -> Class_5 a b) <$> classes_0
  classes_2 :: Dictionary Kind_0
  classes_2 = (\(Class_6 (Type_variable_1 _ a) _ _) -> a) <$> classes_0
  comparison_type :: Type_4
  comparison_type = Name_type_4 "Ordering"
  constr_check :: Dictionary [String] -> [String] -> [[String]] -> [[String]] -> Maybe String
  constr_check m t x y =
    case t of
      [] -> Nothing
      s : t' ->
        case x of
          [] -> undefined
          a : x' ->
            case y of
              [] -> undefined
              b : y' ->
                case constr_check' m s a b of
                  Just c -> Just c
                  Nothing -> constr_check m t' x' y'
  constr_check' :: Dictionary [String] -> String -> [String] -> [String] -> Maybe String
  constr_check' n t x y =
    case x of 
      [] -> Nothing
      a : x' ->
        case constr_check'' n t a y of
          Left m -> Just m
          Right y' -> constr_check' n t x' y'
  constr_check'' :: Dictionary [String] -> String -> String -> [String] -> Either String [String]
  constr_check'' m t c x =
    case x of
      [] -> Left (c ++ " " ++ t)
      a : x' -> if constr_check_3 m c [a] then Right x' else (:) a <$> constr_check'' m t c x'
  constr_check_3 :: Dictionary [String] -> String -> [String] -> Bool
  constr_check_3 m x y =
    case y of
      [] -> True
      a : b ->
        case x == a of
          False -> constr_check_3 m x (m ! a ++ b)
          True -> constr_check_3 m x b
  constrs :: [(String, Constructor_3)]
  constrs =
    [
      ("EQ", Constructor_3 [] [] (Name_type_4 "Ordering")),
      ("GT", Constructor_3 [] [] (Name_type_4 "Ordering")),
      (
        "Just",
        Constructor_3
          [Type_variable_1 "T" Star_kind_0]
          [Name_type_4 "T"]
          (Application_type_4 (Name_type_4 "Maybe") (Name_type_4 "T"))),
      ("LT", Constructor_3 [] [] (Name_type_4 "Ordering")),
      (
        "Nothing",
        Constructor_3 [Type_variable_1 "T" Star_kind_0] [] (Application_type_4 (Name_type_4 "Maybe") (Name_type_4 "T")))]
  context_union ::
    (
      (
        (
          Dictionary Kind_0,
          Dictionary Constructor_3,
          Dictionary Polymorphic_type,
          Dictionary Class_6,
          Dictionary Class_5,
          Dictionary (Dictionary [[String]]),
          Dictionary Kind_0),
        (Dictionary Operator_0, Dictionary Operator_0)) ->
      (
        (
          Dictionary Kind_0,
          Dictionary Constructor_3,
          Dictionary Polymorphic_type,
          Dictionary Class_6,
          Dictionary Class_5,
          Dictionary (Dictionary [[String]]),
          Dictionary Kind_0),
        (Dictionary Operator_0, Dictionary Operator_0)) ->
      (
        (
          Dictionary Kind_0,
          Dictionary Constructor_3,
          Dictionary Polymorphic_type,
          Dictionary Class_6,
          Dictionary Class_5,
          Dictionary (Dictionary [[String]]),
          Dictionary Kind_0),
        (Dictionary Operator_0, Dictionary Operator_0)))
  context_union ((b, j, d, e, q, t, g), (a7, t0)) ((f, l, h, m, r, u, n), (a8, t2)) =
    (
      (
        Data.Map.union b f,
        Data.Map.union j l,
        Data.Map.union d h,
        Data.Map.union e m,
        Data.Map.union q r,
        unionWith Data.Map.union t u,
        Data.Map.union g n),
      (Data.Map.union a7 a8, Data.Map.union t0 t2))
  defs :: Dictionary Polymorphic_term
  defs =
    Data.Map.fromList
      [
        (
          "Add",
          Ad_hoc_term
            []
            (Data.Map.fromList
              [
                (
                  "Int",
                  Implementation
                    []
                    (Arrow_term_4
                      (Arrow_4
                        (Name_term_pattern_6 "x")
                        (Arrow_term_4
                          (Arrow_4 (Name_term_pattern_6 "y") (Add_Int_term_4 (Loc_term_4 "x") (Loc_term_4 "y"))))))),
                ("Modular", Add_Modular_term)])),
        (
          "Compare",
          Ad_hoc_term
            []
            (Data.Map.fromList
              [
                (
                  "Int",
                  Implementation
                    []
                    (Arrow_term_4
                      (Arrow_4
                        (Name_term_pattern_6 "x")
                        (Arrow_term_4
                          (Arrow_4 (Name_term_pattern_6 "y") (Compare_Int_term_4 (Loc_term_4 "x") (Loc_term_4 "y"))))))),
                (
                  "Modular",
                  Implementation
                    ["N"]
                    (Arrow_term_4
                      (Arrow_4
                        (Name_term_pattern_6 "x")
                        (Arrow_term_4
                          (Arrow_4 (Name_term_pattern_6 "y") (Compare_Modular_term_4 (Loc_term_4 "x") (Loc_term_4 "y")))))))])),
        (
          "Convert",
          Ad_hoc_term
            []
            (Data.Map.fromList
              [
                ("Int", Implementation [] (Arrow_term_4 (Arrow_4 (Name_term_pattern_6 "x") (Loc_term_4 "x")))),
                ("Modular", Convert_Modular_term)])),
        (
          "Div",
          Parametric_term
            []
            (Arrow_term_4
              (Arrow_4
                (Name_term_pattern_6 "x")
                (Arrow_term_4 (Arrow_4 (Name_term_pattern_6 "y") (Div_term_4 (Loc_term_4 "x") (Loc_term_4 "y"))))))),
        ("EQ", Parametric_term [] (Algebraic_term_4 "EQ" [])),
        ("GT", Parametric_term [] (Algebraic_term_4 "GT" [])),
        ("Just", Parametric_term ["T"] (Algebraic_term_4 "Just" [])),
        ("LT", Parametric_term [] (Algebraic_term_4 "LT" [])),
        (
          "Mod",
          Parametric_term
            []
            (Arrow_term_4
              (Arrow_4
                (Name_term_pattern_6 "x")
                (Arrow_term_4 (Arrow_4 (Name_term_pattern_6 "y") (Mod_term_4 (Loc_term_4 "x") (Loc_term_4 "y"))))))),
        ("Nothing", Parametric_term ["T"] (Algebraic_term_4 "Nothing" [])),
        (
          "Times",
          Ad_hoc_term
            []
            (Data.Map.fromList
              [
                (
                  "Int",
                  Implementation
                    []
                    (Arrow_term_4
                      (Arrow_4
                        (Name_term_pattern_6 "x")
                        (Arrow_term_4
                          (Arrow_4 (Name_term_pattern_6 "y") (Times_Int_term_4 (Loc_term_4 "x") (Loc_term_4 "y"))))))),
                ("Modular", Times_Modular_term)]))]
  exprrepl :: Map Integer Type_4 -> Term_3 -> Term_4
  exprrepl a b =
    case b of
      Application_term_3 c d -> Application_term_4 (exprrepl a c) (exprrepl a d)
      Branch_term_3 c d e f -> Branch_term_4 (Name_type_4 c) (exprrepl a d) e (exprrepl a f)
      Arrow_term_3 c d -> Arrow_term_4 (Arrow_4 c (exprrepl a d))
      Glob_term_3 c d e -> Glob_term_4 c (exprrepl' a <$> d) (exprrepl' a <$> e)
      Int_term_3 c -> Int_term_4 c
      Loc_term_3 c -> Loc_term_4 c
      Match_term_3 c d -> Match_term_4 (exprrepl a c) ((\(Arrow_3 e f) -> Arrow_4 e (exprrepl a f)) <$> d)
      Modular_term_3 c -> Modular_term_4 c
  exprrepl' :: Map Integer Type_4 -> Type_6 -> Type_4
  exprrepl' a b =
    case b of
      Application_type_6 c d -> Application_type_4 (exprrepl' a c) (exprrepl' a d)
      Name_type_6 c -> Name_type_4 c
      Var_type_6 c -> a ! c
{-
  find_and_delete :: Ord t => Map t u -> t -> Maybe (u, Map t u)
  find_and_delete a b = (\c -> (c, Data.Map.delete b a)) <$> Data.Map.lookup b a
-}
  function_type :: Type_4 -> Type_4 -> Type_4
  function_type a = Application_type_4 (Application_type_4 (Name_type_4 "Arrow") a)
  function_type' :: Type_6 -> Type_6 -> Type_6
  function_type' a = Application_type_6 (Application_type_6 (Name_type_6 "Arrow") a)
  get_pattern_type ::
    (
      String ->
      Dictionary Constructor_3 ->
      (Integer, Dictionary (Either Polymorphic_type Type_6)) ->
      Term_pattern_5 ->
      Type_6 ->
      Err ((Integer, [Integer], [(Type_6, Type_6)], Dictionary (Either Polymorphic_type Type_6)), Term_pattern_6))
  get_pattern_type file_name c (d, n) g h =
    case g of
      Blank_term_pattern_5 -> Right ((d, [], [], n), Blank_term_pattern_6)
      Int_term_pattern_5 i -> Right ((d, [], [(h, int_type')], n), Int_term_pattern_6 i)
      Modular_term_pattern_5 (Modular_0 j z) -> Right ((d, [], [(h, mod_type' (int_to_nat' j))], n), Modular_term_pattern_6 z)
      Name_term_pattern_5 i -> Right ((d, [], [], Data.Map.insert i (Right h) n), Name_term_pattern_6 i)
      Struct_term_pattern_5 (Name o i) j ->
        case Data.Map.lookup i c of
          Nothing -> Left (Undefined "constructor" i file_name o)
          Just (Constructor_3 m0 y n0) ->
            let
              ((q, r), (s, q4)) = typevars m0 (d, Data.Map.empty)
            in
              (
                bimap
                  (\(t, x, y', z) ->
                    (t, s ++ x, (h, sysrepl (Data.Map.fromList (zip ((\(Type_variable_1 t7 _) -> t7) <$> m0) q4)) n0) : y', z))
                  (Struct_term_pattern_6 i) <$>
                get_pattern_types file_name c (q, n) j (repl_transf r <$> y) (Name o i))
  get_pattern_types ::
    (
      String ->
      Dictionary Constructor_3 ->
      (Integer, Dictionary (Either Polymorphic_type Type_6)) ->
      [Term_pattern_5] ->
      [Type_6] ->
      Name ->
      Err ((Integer, [Integer], [(Type_6, Type_6)], Dictionary (Either Polymorphic_type Type_6)), [Term_pattern_6]))
  get_pattern_types fn c (d0, d3) e f (Name m n) =
    let
      t = show <$> [0 .. length f - 1]
    in
      case e of
        [] ->
          case t of
            [] -> Right ((d0, [], [], Prelude.foldl (\u -> \(m', v) -> Data.Map.insert m' (Right v) u) d3 (zip t f)), [])
            _ -> Left (Constructor_has_been_given_a_wrong_number_of_arguments fn n m)
        g : h ->
          case f of
            [] -> Left (Constructor_has_been_given_a_wrong_number_of_arguments fn n m)
            i : j ->
              (
                get_pattern_type fn c (d0, d3) g i >>=
                \((k, k0, k1, k2), l) ->
                  (
                    bimap (\(m0, m1, m2, m3) -> (m0, k0 ++ m1, k1 ++ m2, m3)) ((:) l) <$>
                    get_pattern_types fn c (k, k2) h j (Name m n)))
  init_type_context ::
    (
      (
        Dictionary Kind_0,
        Dictionary Constructor_3,
        Dictionary Polymorphic_type,
        Dictionary Class_6,
        Dictionary Class_5,
        Dictionary (Dictionary [[String]]),
        Dictionary Kind_0),
      (Dictionary Operator_0, Dictionary Operator_0))
  init_type_context =
    ((kinds, Data.Map.fromList constrs, typest, classes_0, classes_1, instances, classes_2), (Data.Map.empty, Data.Map.empty))
  ins_new :: String -> t -> Dictionary (t, Status) -> Dictionary (t, Status)
  ins_new a b = Data.Map.insert a (b, Public)
  instances :: Dictionary (Dictionary [[String]])
  instances =
    Data.Map.fromList
      [
        ("Field", Data.Map.fromList [("Modular", [["Nonzero"]])]),
        ("Nonzero", Data.Map.fromList [("Next", [[]])]),
        ("Ord", Data.Map.fromList [("Int", []), ("Modular", [[]])]),
        ("Ring", Data.Map.fromList [("Int", []), ("Modular", [["Nonzero"]])])]
  int_to_nat' :: Integer -> Type_6
  int_to_nat' a =
    case a of
      0 -> Name_type_6 "Zero"
      _ -> Application_type_6 (Name_type_6 "Next") (int_to_nat' (a - 1))
  int_type :: Type_4
  int_type = Name_type_4 "Int"
  int_type' :: Type_6
  int_type' = Name_type_6 "Int"
  kind_0_to_1 :: Kind_0 -> Kind_1
  kind_0_to_1 a =
    case a of
      Arrow_kind_0 b c -> Arrow_kind_1 (kind_0_to_1 b) (kind_0_to_1 c)
      Nat_kind_0 -> Nat_kind_1
      Star_kind_0 -> Star_kind_1
  kinds :: Dictionary Kind_0
  kinds =
    Data.Map.fromList
      [
        ("Arrow", Arrow_kind_0 Star_kind_0 (Arrow_kind_0 Star_kind_0 Star_kind_0)),
        ("Int", Star_kind_0),
        ("Maybe", Arrow_kind_0 Star_kind_0 Star_kind_0),
        ("Modular", Arrow_kind_0 Nat_kind_0 Star_kind_0),
        ("Next", Arrow_kind_0 Nat_kind_0 Nat_kind_0),
        ("Ordering", Star_kind_0),
        ("Zero", Nat_kind_0)]
  locations_0 :: Dictionary Language_or_location
  locations_0 =
    Data.Map.fromList ((\x -> (x, Language)) <$> ["Arrow", "Int", "Maybe", "Modular", "Next", "Ordering", "Zero"])
  locations_1 :: Dictionary Language_or_location
  locations_1 = Data.Map.fromList ((\x -> (x, Language)) <$> ["Field", "Nonzero", "Ord", "Ring"])
  locations_2 :: Dictionary Language_or_location
  locations_2 =
    Data.Map.fromList
      (
        (\x -> (x, Language)) <$>
        ((fst <$> constrs) ++ ["Add", "Compare", "Convert", "Crash", "Div", "Inverse", "Mod", "Times"]))
  maybe_type :: Type_4 -> Type_4
  maybe_type = Application_type_4 (Name_type_4 "Maybe")
  mod_type' :: Type_6 -> Type_6
  mod_type' = Application_type_6 (Name_type_6 "Modular")
  occ_kind :: Integer -> Kind_1 -> Bool
  occ_kind a b =
    case b of
      Arrow_kind_1 c d -> occ_kind a c || occ_kind a d
      Var_kind_1 c -> c == a
      _ -> False
  occtype :: Integer -> Type_6 -> Bool
  occtype a b =
    case b of
      Application_type_6 c d -> occtype a c || occtype a d
      Name_type_6 _ -> False
      Var_type_6 c -> c == a
  old :: Dictionary t -> Dictionary (t, Status)
  old = fmap (\a -> (a, Private))
  old' :: Dictionary (Dictionary t) -> Dictionary (Dictionary (t, Status))
  old' = (<$>) old
  rem_old' :: Dictionary (Dictionary (t, Status)) -> Dictionary (Dictionary t)
  rem_old' a = Data.Map.filter (\b -> not (Data.Map.null b)) (public_entries <$> a)
  repl_kind_eq :: Integer -> Kind_1 -> Kind_1 -> Kind_1
  repl_kind_eq a b c =
    case c of
      Arrow_kind_1 d e -> Arrow_kind_1 (repl_kind_eq a b d) (repl_kind_eq a b e)
      Var_kind_1 d ->
        case d == a of
          False -> c
          True -> b
      _ -> c
  repl_kind_eqs :: Integer -> Kind_1 -> [(Kind_1, Kind_1)] -> [(Kind_1, Kind_1)]
  repl_kind_eqs a b c = bimap (repl_kind_eq a b) (repl_kind_eq a b) <$> c
  repl_transf :: Dictionary Type_6 -> Type_4 -> Type_6
  repl_transf a b =
    case b of
      Application_type_4 c d -> Application_type_6 (repl_transf a c) (repl_transf a d)
      Name_type_4 c ->
        case Data.Map.lookup c a of
          Nothing -> Name_type_6 c
          Just d -> d
  slv :: Dictionary (Dictionary [[String]]) -> [(String, (Name, Type_4))] -> (Name -> String -> String -> Err ()) -> Err ()
  slv a b h =
    case b of
      [] -> Right ()
      (c, (y, d)) : e ->
        let
          (f, g) = typestring d []
          i = h y c f
        in
          case Data.Map.lookup c a of
            Just x ->
              case Data.Map.lookup f x of
                Just j -> slv_constrs a e h g j y
                Nothing -> i
            Nothing -> i
  slv_constrs ::
    (
      Dictionary (Dictionary [[String]]) ->
      [(String, (Name, Type_4))] ->
      (Name -> String -> String -> Err ()) ->
      [Type_4] ->
      [[String]] ->
      Name ->
      Err ())
  slv_constrs a b c d e y =
    case d of
      [] ->
        case e of
          [] -> slv a b c
          _ -> undefined
      f : g ->
        case e of
          [] -> undefined
          h : i -> slv_constrs a (((\j -> (j, (y, f))) <$> h) ++ b) c g i y
  solve_all ::
    (
      Loc ->
      String ->
      Dictionary (Dictionary [[String]]) ->
      Maybe Name ->
      Equations ->
      Map Integer Type_6 ->
      Err (Map Integer Type_6))
  solve_all lc fn b c (Equations d e f g) h =
    (
      solvesys lc e (f, Data.Set.fromList d, g, h) >>=
      \(i, k, l, j) ->
        case Data.Set.null k of
          False -> Left (Unresolved_type_variables lc)
          True ->
            (
              slv
                b
                (second (second typeback) <$> i)
                (\(Name m n) -> \o -> \p -> Left (Method_requires_instance_or_constraint fn n c m o p)) >>
              solve_conds lc fn b c l j))
  solve_cond ::
    (
      Loc ->
      String ->
      Dictionary (Dictionary [[String]]) ->
      Maybe Name ->
      Branch_equations ->
      Map Integer Type_6 ->
      Err (Map Integer Type_6))
  solve_cond lc fn b c (Branch_equations _ (e, (_, g))) h = solve_all lc fn b c e h >>= solve_all lc fn b c g
  solve_conds ::
    (
      Loc ->
      String ->
      Dictionary (Dictionary [[String]]) ->
      Maybe Name ->
      [Branch_equations] ->
      Map Integer Type_6 ->
      Err (Map Integer Type_6))
  solve_conds lc fn b c d e =
    case d of
      [] -> Right e
      f : g -> solve_cond lc fn b c f e >>= solve_conds lc fn b c g
  solve_type_eqs :: String -> Line_and_char -> [(Kind_1, Kind_1)] -> Err ()
  solve_type_eqs x a b =
    case b of
      [] -> Right ()
      d : e ->
        case d of
          (Arrow_kind_1 f g, Arrow_kind_1 h i) -> solve_type_eqs x a ((f, h) : (g, i) : e)
          (Nat_kind_1, Nat_kind_1) -> solve_type_eqs x a e
          (Star_kind_1, Star_kind_1) -> solve_type_eqs x a e
          (Var_kind_1 f, Var_kind_1 g) -> solve_type_eqs x a (repl_kind_eqs f (Var_kind_1 g) e)
          (Var_kind_1 f, g) -> solve_type_eqs' x a f g e
          (f, Var_kind_1 g) -> solve_type_eqs' x a g f e
          _ -> Left (Kind_mismatch x a)
  solve_type_eqs' :: String -> Line_and_char -> Integer -> Kind_1 -> [(Kind_1, Kind_1)] -> Err ()
  solve_type_eqs' x a b c d =
    case occ_kind b c of
      False -> solve_type_eqs x a (repl_kind_eqs b c d)
      True -> Left (Kind_mismatch x a)
  solvesys ::
    Loc ->
    [(Type_6, Type_6)] ->
    ([(String, (Name, Type_6))], Set Integer, [Branch_equations], Map Integer Type_6) ->
    Err ([(String, (Name, Type_6))], Set Integer, [Branch_equations], Map Integer Type_6)
  solvesys m b (a', u, u1, t) =
    case b of
      [] -> Right (a', u, u1, t)
      c' : g ->
        case c' of
          (Application_type_6 e f, Application_type_6 h i) -> solvesys m ((e, h) : (f, i) : g) (a', u, u1, t)
          (Name_type_6 e, Name_type_6 f) ->
            case e == f of
              False -> Left (Type_mismatch m)
              True -> solvesys m g (a', u, u1, t)
          (Var_type_6 e, Var_type_6 f) -> solvesys_rep m e (Var_type_6 f) g (a', u, u1, t)
          (Var_type_6 e, d) -> solvesys' m e d g (a', u, u1, t)
          (c, Var_type_6 h) -> solvesys' m h c g (a', u, u1, t)
          _ -> Left (Type_mismatch m)
  solvesys' ::
    (
      Loc ->
      Integer ->
      Type_6 ->
      [(Type_6, Type_6)] ->
      ([(String, (Name, Type_6))], Set Integer, [Branch_equations], Map Integer Type_6) ->
      Err ([(String, (Name, Type_6))], Set Integer, [Branch_equations], Map Integer Type_6))
  solvesys' h b c d (x, a, w, t) =
    case (Data.Set.member b a, occtype b c) of
      (True, False) -> solvesys_rep h b c d (x, a, w, t)
      _ -> Left (Type_mismatch h)
  solvesys_rep ::
    (
      Loc ->
      Integer ->
      Type_6 ->
      [(Type_6, Type_6)] ->
      ([(String, (Name, Type_6))], Set Integer, [Branch_equations], Map Integer Type_6) ->
      Err ([(String, (Name, Type_6))], Set Integer, [Branch_equations], Map Integer Type_6))
  solvesys_rep a c d e (x, k, w, t) =
    let
      m = sysrep'' c d
    in
      solvesys
        a
        (bimap m m <$> e)
        (second (second m) <$> x, Data.Set.delete c k, sysrep_cond c d <$> w, Data.Map.insert c d (m <$> t))
  sysrep :: String -> Type_4 -> Type_4 -> Type_4
  sysrep a b c =
    case c of
      Application_type_4 d e -> Application_type_4 (sysrep a b d) (sysrep a b e)
      Name_type_4 d -> if d == a then b else c
  sysrep' :: String -> Type_6 -> Type_6 -> Type_6
  sysrep' a b c =
    let
      f = sysrep' a b
    in
      case c of
        Application_type_6 d e -> Application_type_6 (f d) (f e)
        Name_type_6 d -> if d == a then b else c
        _ -> c
  sysrep'' :: Integer -> Type_6 -> Type_6 -> Type_6
  sysrep'' a b c =
    case c of
      Application_type_6 d e -> Application_type_6 (sysrep'' a b d) (sysrep'' a b e)
      Name_type_6 _ -> c
      Var_type_6 d -> if d == a then b else c
  sysrep_cond :: Integer -> Type_6 -> Branch_equations -> Branch_equations
  sysrep_cond a b (Branch_equations c (d, (e, f))) =
    let
      g h i = sysrep_eqs a c h b i
    in
      Branch_equations c (g (Name_type_6 "Zero") d, (e, g (Application_type_6 (Name_type_6 "Next") (Name_type_6 e)) f))
  sysrep_eqs :: Integer -> String -> Type_6 -> Type_6 -> Equations -> Equations
  sysrep_eqs a b h i (Equations c d e f) =
    let
      g = sysrep' b h i
      j = sysrep'' a g
    in
      Equations c (bimap j j <$> d) (second (second j) <$> e) (sysrep_cond a g <$> f)
  sysrepl :: Map String Type_6 -> Type_4 -> Type_6
  sysrepl a b =
    case b of
      Application_type_4 c d -> Application_type_6 (sysrepl a c) (sysrepl a d)
      Name_type_4 c ->
        case Data.Map.lookup c a of
          Nothing -> Name_type_6 c
          Just d -> d
  typeback :: Type_6 -> Type_4
  typeback a =
    case a of
      Application_type_6 b c -> Application_type_4 (typeback b) (typeback c)
      Name_type_6 b -> Name_type_4 b
      Var_type_6 _ -> undefined
  type_algebraic_constructor ::
    (
      String ->
      [Type_variable_1] ->
      Constructor_2 ->
      Dictionary Kind_0 ->
      (Term_4 -> Polymorphic_term) ->
      (Type_4 -> Polymorphic_type) ->
      Type_4 ->
      Status ->
      Err (New_entry Polymorphic_type, New_entry Constructor_3, Entry Polymorphic_term))
  type_algebraic_constructor file_name c (Constructor_2 loc d e f) g m m' n status =
    case (status, d) of
      (Private, Private) -> Left (Unnecessary_access_modifier_before_constructor e file_name loc)
      _ -> (\l -> type_constructor c (min status d) e l m m' n) <$> type_types file_name f g
  type_branch_data :: String -> Name -> [Type_variable_1] -> Err ([Type_variable_1], [Type_variable_1])
  type_branch_data file_name (Name a b) c =
    case c of
      [] -> Left (Undefined "type variable" b file_name a)
      Type_variable_1 d e : f ->
        case b == d of
          False -> bimap ((:) (Type_variable_1 d e)) ((:) (Type_variable_1 d e)) <$> type_branch_data file_name (Name a b) f
          True ->
            case e of
              Nat_kind_0 -> Right (f, Type_variable_1 b Nat_kind_0 : f)
              _ -> Left (Type_variable_is_not_of_kind_Nat b file_name a)
  type_case ::
    (
      String ->
      Dictionary Constructor_3 ->
      Integer ->
      Dictionary (Either Polymorphic_type Type_6) ->
      Arrow_2 ->
      Integer ->
      Type_6 ->
      Dictionary Kind_0 ->
      Err (Arrow_3, Equations, Integer))
  type_case file_name a d f (Arrow_2 g h) i j k =
    (
      get_pattern_type file_name a (d, f) g (Var_type_6 i) >>=
      \((m, n, s, t), o) ->
        (
          (\(u, Equations e p q r, w) -> (Arrow_3 o u, Equations (n ++ e) (s ++ p) q r, w)) <$>
          type_expression file_name a m t h j k))
  type_cases ::
    (
      String ->
      Dictionary Constructor_3 ->
      Integer ->
      Dictionary (Either Polymorphic_type Type_6) ->
      [Arrow_2] ->
      Integer ->
      Type_6 ->
      Dictionary Kind_0 ->
      Err ([Arrow_3], Equations, Integer))
  type_cases file_name b d f g n h i =
    case g of
      [] -> Right ([], Equations [] [] [] [], d)
      l : m ->
        (
          type_case file_name b d f l n h i >>=
          \(o, Equations p0 p1 p2 p3, q) ->
            (
              (\(r, Equations s0 s1 s2 s3, t) -> (o : r, Equations (p0 ++ s0) (p1 ++ s1) (p2 ++ s2) (p3 ++ s3), t)) <$>
              type_cases file_name b q f m n h i))
  type_class_0 ::
    (
      String ->
      Class_3 ->
      (New_dictionary Kind_0, New_dictionary Class_5, Dictionary String) ->
      Err (Class_4, (New_dictionary Kind_0, New_dictionary Class_5, Dictionary String)))
  type_class_0 file_name (Class_3 b (Type_variable_1 c d) g' e) (i', j0, x2) =
    let
      g3 = (\(Constraint_0 (Name _ t4) _) -> t4) <$> g'
    in
      (
        (\u5 ->
          (
            Class_4 b (Type_variable_1 c d) u5 e,
            (
              ins_new b d i',
              ins_new b (Class_5 d g3) j0,
              Prelude.foldl (\v -> \(Name _ t0) -> Data.Map.insert b t0 v) x2 u5))) <$
        type_inh file_name b [b] g3 x2 <*>
        traverse
          (\(Constraint_0 (Name t2 t0) (Name x3 x4)) ->
            case x4 == c of
              False -> Left (Undefined "type variable" x4 file_name x3)
              True -> Right (Name t2 t0))
          g')
  type_class_1 ::
    (
      String ->
      Class_4 ->
      Dictionary Kind_0 ->
      Dictionary Kind_0 ->
      (New_dictionary Polymorphic_type, New_dictionary Class_6) ->
      Err (New_dictionary Polymorphic_type, New_dictionary Class_6))
  type_class_1 fn (Class_4 b (Type_variable_1 c k) g' e) y4 d (f0, f) =
    (
      (\m -> \e' ->
        (
          Prelude.foldl (\x -> \(Method_4 t s u) -> ins_new t (Ad_hoc_type (Type_variable_1 c k) s b u) x) f0 e',
          ins_new b (Class_6 (Type_variable_1 c k) m e') f)) <$>
      traverse
        (\(Name m g) ->
          case Data.Map.lookup g d of
            Nothing -> Left (Undefined "class" g fn m)
            Just h -> if h == k then Right g else Left (Kind_mismatch_in_class fn m))
        g' <*>
      type_methods_1 fn d e)
  type_class_args ::
    (
      Kind_0 ->
      [Type_pattern_3] ->
      Err ([Type_variable_1], Integer, Type_4, Dictionary Kind_0, Dictionary Nat) ->
      Kind_0 ->
      Integer ->
      Type_4 ->
      Dictionary Kind_0 ->
      Dictionary Nat ->
      Nat ->
      Err ([Type_variable_1], Integer, Type_4, Dictionary Kind_0, Dictionary Nat))
  type_class_args a b e g c x c0 c' n =
    case b of
      [] -> if a == g then Right ([], c, x, c0, c') else e
      h : d ->
        case a of
          Arrow_kind_0 l f ->
            let
              i j k =
                (
                  (\(t, u, v, t', t2) -> ((Type_variable_1 j l) : t, u, v, t', t2)) <$>
                  type_class_args
                    f
                    d
                    e
                    g
                    k
                    (Application_type_4 x (Name_type_4 j))
                    (Data.Map.insert j l c0)
                    (Data.Map.insert j n c')
                    (Nxt n))
            in
              case h of
                Blank_type_pattern_3 -> i (show c) (c + 1)
                Name_type_pattern_3 j -> i j c
          _ -> e
  type_classes ::
    (
      String ->
      Dictionary Kind_0 ->
      [Class_3] ->
      (New_dictionary Class_6, New_dictionary Polymorphic_type, New_dictionary Kind_0, New_dictionary Class_5) ->
      Err (New_dictionary Class_6, New_dictionary Polymorphic_type, New_dictionary Kind_0, New_dictionary Class_5))
  type_classes file_name c d (e, g, o, o') =
    (
      type_classes_0 file_name d (o, o', Data.Map.empty) >>=
      \(r, (p, p', _)) -> (\(k, n) -> (n, k, p, p')) <$> type_classes_1 file_name r c (fst <$> p) (g, e))
  type_classes_0 ::
    (
      String ->
      [Class_3] ->
      (New_dictionary Kind_0, New_dictionary Class_5, Dictionary String) ->
      Err ([Class_4], (New_dictionary Kind_0, New_dictionary Class_5, Dictionary String)))
  type_classes_0 file_name b c =
    case b of
      [] -> Right ([], c)
      d : e -> type_class_0 file_name d c >>= \(h, i) -> first ((:) h) <$> type_classes_0 file_name e i
  type_classes_1 ::
    (
      String ->
      [Class_4] ->
      Dictionary Kind_0 ->
      Dictionary Kind_0 ->
      (New_dictionary Polymorphic_type, New_dictionary Class_6) ->
      Err (New_dictionary Polymorphic_type, New_dictionary Class_6))
  type_classes_1 fn b y h c =
    case b of
      [] -> Right c
      d : e -> type_class_1 fn d y h c >>= type_classes_1 fn e y h
  type_cls_0 ::
    (
      String ->
      String ->
      [Method_4] ->
      Type_4 ->
      [Binding_2] ->
      String ->
      Line_and_char ->
      Err [(Name, Term_2, [Type_variable_1], Type_4)])
  type_cls_0 fn a b c d m n =
    case d of
      [] ->
        case b of
          [] -> Right []
          Method_4 e _ _ : _ -> Left (Missing_definition fn e m a n)
      Binding_2 (Name h i) j : k ->
        case b of
          [] -> Left (Def_err fn i h m)
          Method_4 e s f : g ->
            case i == e of
              False -> Left (Expected fn e h i)
              True -> (:) ((Name h i), j, s, f) <$> type_cls_0 fn a g c k m n
  type_constraint_0 ::
    (
      String ->
      Dictionary (Set String) ->
      Constraint_0 ->
      (Dictionary Class_5, Dictionary Kind_0) ->
      Err (Dictionary (Set String)))
  type_constraint_0 file_name k (Constraint_0 (Name b c) (Name d e)) (f, g) =
    case Data.Map.lookup c f of
      Nothing -> Left (Undefined "class" c file_name b)
      Just (Class_5 h y) ->
        case Data.Map.lookup e g of
          Just i ->
            if i == h
              then
                Right
                  (chain_constraints
                    y
                    f
                    (Data.Map.insert
                      c
                      (Data.Set.insert
                        e
                        (case Data.Map.lookup c k of
                          Just l -> l
                          Nothing -> Data.Set.empty))
                      k)
                    e)
              else
                Left (Kind_mismatch_in_constraint file_name b c e)
          Nothing -> Left (Undefined "type variable" e file_name d)
  type_constraint_1 ::
    Constraint_1 -> Dictionary (Dictionary [[String]]) -> Dictionary Class_6 -> Dictionary (Dictionary [[String]])
  type_constraint_1 (Constraint_1 c e) a b =
    let
      Class_6 _ f _ = b ! c
    in
      type_constraints_1
        ((\g -> Constraint_1 g e) <$> f)
        (Data.Map.insert
          c
          (Data.Map.insert
            e
            []
            (case Data.Map.lookup c a of
              Just l -> l
              Nothing -> Data.Map.empty))
          a)
        b
  type_constraints_0 ::
    (
      String ->
      Dictionary (Set String) ->
      [Constraint_0] ->
      (Dictionary Class_5, Dictionary Kind_0, Dictionary Nat) ->
      Err [Constraint_1])
  type_constraints_0 fn g a (f, t, u) =
    case a of
      [] -> Right (join ((\(i, j) -> Constraint_1 i <$> j) <$> assocs (Data.Set.elems <$> g)))
      b : c -> type_constraint_0 fn g b (f, t) >>= \d -> type_constraints_0 fn d c (f, t, u)
  type_constraints_1 ::
    [Constraint_1] -> Dictionary (Dictionary [[String]]) -> Dictionary Class_6 -> Dictionary (Dictionary [[String]])
  type_constraints_1 a e d =
    case a of
      [] -> e
      b : c -> type_constraints_1 c (type_constraint_1 b e d) d
  type_constructor ::
    (
      [Type_variable_1] ->
      Status ->
      String ->
      [Type_4] ->
      (Term_4 -> Polymorphic_term) ->
      (Type_4 -> Polymorphic_type) ->
      Type_4 ->
      (New_entry Polymorphic_type, New_entry Constructor_3, Entry Polymorphic_term))
  type_constructor c d e l m m' n =
    ((e, (m' (Prelude.foldr function_type n l), d)), (e, (Constructor_3 c l n, Public)), (e, (m (Algebraic_term_4 e []))))
  type_constructors ::
    (
      String ->
      Dictionary Kind_0 ->
      [Type_variable_1] ->
      [Constructor_2] ->
      (Term_4 -> Polymorphic_term) ->
      (Type_4 -> Polymorphic_type) ->
      Type_4 ->
      Status ->
      Err (New_dictionary Polymorphic_type, New_dictionary Constructor_3, Dictionary Polymorphic_term))
  type_constructors file_name e c d i i' m status =
    (
      (\a ->
        (
          Data.Map.fromList ((\(b, _, _) -> b) <$> a),
          Data.Map.fromList ((\(_, b, _) -> b) <$> a),
          Data.Map.fromList ((\(_, _, b) -> b) <$> a))) <$>
      traverse (\a -> type_algebraic_constructor file_name c a e i i' m status) d)
  type_data_0 :: Data_3 -> New_entry Kind_0
  type_data_0 (Data_3 status name type_variables _) =
    (name, (Prelude.foldr Arrow_kind_0 Star_kind_0 (type_type_variable_0 <$> type_variables), status))
  type_data_1 ::
    (
      String ->
      Dictionary Kind_0 ->
      Data_3 ->
      Err (New_dictionary Polymorphic_type, New_dictionary Constructor_3, Dictionary Polymorphic_term))
  type_data_1 file_name types (Data_3 status name type_variables data_branch) =
    type_data_branch
      file_name
      (type_type_variables type_variables types)
      status
      type_variables
      (Prelude.foldl Application_type_4 (Name_type_4 name) ((\(Type_variable_1 a _) -> Name_type_4 a) <$> type_variables))
      data_branch
  type_data_branch ::
    (
      String ->
      Dictionary Kind_0 ->
      Status ->
      [Type_variable_1] ->
      Type_4 ->
      Data_branch_3 ->
      Err (New_dictionary Polymorphic_type, New_dictionary Constructor_3, Dictionary Polymorphic_term))
  type_data_branch file_name types status type_variables typ data_branch =
    let
      polymorphic_term = Parametric_term ((\(Type_variable_1 c' _) -> c') <$> type_variables)
      polymorphic_type = Parametric_type type_variables []
    in
      case data_branch of
        Algebraic_data_3 constructors ->
          type_constructors file_name types type_variables constructors polymorphic_term polymorphic_type typ status
        Branch_data_3 (Name h j) (k, (l, m)) ->
          let
            i = Data.Map.delete j types
            f i' u4 k2 = type_data_branch file_name i' status u4 (sysrep j k2 typ)
          in
            (
              type_branch_data file_name (Name h j) type_variables >>=
              \(n, t) ->
                (
                  (\(q0, q1, q2) -> \(r0, r1, r2) -> (Data.Map.union q0 r0, Data.Map.union q1 r1, Data.Map.union q2 r2)) <$>
                  f i n (Name_type_4 "Zero") k <*>
                  f (Data.Map.insert l Nat_kind_0 i) t (Application_type_4 (Name_type_4 "Next") (Name_type_4 l)) m))
        Struct_data_3 loc k l ->
          case status of
            Private -> Left (Private_struct k file_name loc)
            Public ->
              (
                (\(h, (m, u)) ->
                  let
                    (w0, w1, w2) = type_constructor type_variables Public k h polymorphic_term polymorphic_type typ
                  in
                    (uncurry Data.Map.insert w0 m, uncurry Data.Map.singleton w1, uncurry Data.Map.insert w2 u)) <$>
                type_fields file_name l types polymorphic_term polymorphic_type typ)
  type_datas ::
    (
      String ->
      [Data_3] ->
      (Dictionary Kind_0, New_dictionary Constructor_3) ->
      Err
        (
          Dictionary Kind_0,
          Dictionary Kind_0,
          New_dictionary Constructor_3,
          New_dictionary Polymorphic_type,
          Dictionary Polymorphic_term))
  type_datas file_name datas (old_types, j) =
    let
      new_types = type_datas_0 datas
      all_types = Data.Map.union old_types (all_entries new_types)
    in
      (
        (\(c', v, w) -> (all_types, public_entries new_types, Data.Map.union j v, c', w)) <$>
        type_datas_1 file_name datas all_types)
  type_datas_0 :: [Data_3] -> New_dictionary Kind_0
  type_datas_0 datas = Data.Map.fromList (type_data_0 <$> datas)
  type_datas_1 ::
    (
      String ->
      [Data_3] ->
      Dictionary Kind_0 ->
      Err (New_dictionary Polymorphic_type, New_dictionary Constructor_3, Dictionary Polymorphic_term))
  type_datas_1 file_name a b =
    (
      (\c ->
        (
          Data.Map.unions ((\(e, _, _) -> e) <$> c),
          Data.Map.unions ((\(_, f, _) -> f) <$> c),
          Data.Map.unions ((\(_, _, g) -> g) <$> c))) <$>
      traverse (type_data_1 file_name b) a)
  type_def_1 ::
    (
      String ->
      Def_or_instance_2 ->
      Dictionary Kind_0 ->
      New_dictionary Polymorphic_type ->
      Dictionary Class_6 ->
      Dictionary Class_5 ->
      Dictionary (Dictionary ([[String]], Status)) ->
      Err (Def_or_instance_3, New_dictionary Polymorphic_type, Dictionary (Dictionary ([[String]], Status))))
  type_def_1 file_name a b c k k2 t' =
    case a of
      Def_2 lc f d e e' g i ->
        let
          j' = type_kinds e Data.Map.empty
        in
          (
            type_constraints_0 file_name Data.Map.empty e' (k2, j', (\_ -> Zr) <$> j') >>=
            \o1 ->
              (
                (\h -> (Def_3 lc d e o1 h i, Data.Map.insert d (Parametric_type e o1 h, f) c, t')) <$>
                type_typ file_name (Type_3 lc g) (type_kinds e b) Star_kind_0))
      Instance_2 d (Name e m) (Type_pattern_4 (Name f n) k') o' g ->
        case Data.Map.lookup m k of
          Nothing -> Left (Undefined "class" m file_name e)
          Just (Class_6 (Type_variable_1 o p) w0 q) ->
            case Data.Map.lookup n b of
              Nothing -> Left (Undefined "type" n file_name f)
              Just s ->
                (
                  type_class_args
                    s
                    k'
                    (Left (Too_many_arguments file_name f n))
                    p
                    0
                    (Name_type_4 n)
                    Data.Map.empty
                    Data.Map.empty Zr >>=
                  \(q', p', s', t0, t7) ->
                    (
                      type_constraints_0 file_name Data.Map.empty o' (k2, t0, t7) >>=
                      \o1 ->
                        let
                          r' =
                            (
                              (\(Type_variable_1 x' _) ->
                                (\(Constraint_1 y' _) -> y') <$> Data.List.filter (\(Constraint_1 _ y') -> y' == x') o1) <$>
                              q')
                        in
                          (
                            (\w ->
                              (
                                Instance_3 d m w0 o n q' p' w s' o1,
                                c,
                                (case Data.Map.lookup m t' of
                                  Just _ -> adjust (ins_new n r') m
                                  Nothing -> Data.Map.insert m (Data.Map.singleton n (r', Public))) t')) <$>
                            type_cls_0 file_name n q s' g m d)))
  type_def_2 ::
    (
      String ->
      Def_or_instance_3 ->
      (Dictionary Constructor_3, Dictionary Polymorphic_type) ->
      Dictionary Polymorphic_term ->
      Dictionary (Dictionary [[String]]) ->
      Dictionary Kind_0 ->
      Dictionary Class_6 ->
      Err (Dictionary Polymorphic_term))
  type_def_2 file_name a (l, k) c m n u0 =
    case a of
      Def_3 r e b x h i ->
        (
          (\t -> Data.Map.insert e (Parametric_term ((\(Type_variable_1 e2 _) -> e2) <$> b) t) c) <$>
          type_expr
            (Def_loc file_name r e)
            file_name
            (Just (Name r e))
            h
            (l, k)
            i
            (type_constraints_1 x m u0)
            0
            (Prelude.foldl (\y -> \(Type_variable_1 z w) -> Data.Map.insert z w y) n b))
      Instance_3 l' e' w0 w e e0 e1 f f' g' ->
        (
          check_conditions
            u0
            m
            e
            (\a0 -> Left (Illegal_instance file_name e' e l' a0))
            e'
            ((\(Type_variable_1 e2 _) -> e2) <$> e0)
            (
              (\(Type_variable_1 x' _) ->
                (\(Constraint_1 y' _) -> y') <$> Data.List.filter (\(Constraint_1 _ y') -> y' == x') g') <$>
              e0)
            w0 *>
          type_exprs
            file_name
            e
            (l, k)
            (type_constraints_1 g' m u0)
            f
            c
            e
            (sysrep w f')
            e1
            u0
            (Prelude.foldl (\x -> \(Type_variable_1 y g) -> Data.Map.insert y g x) n e0)
            ((\(Type_variable_1 e2 _) -> e2) <$> e0))
  type_defs ::
    (
      String ->
      [Def_or_instance_2] ->
      (Dictionary Kind_0, Dictionary Constructor_3) ->
      (Dictionary Polymorphic_term, New_dictionary Polymorphic_type) ->
      Dictionary Class_6 ->
      Dictionary Class_5 ->
      Dictionary (Dictionary ([[String]], Status)) ->
      Err (Dictionary Polymorphic_term, New_dictionary Polymorphic_type, Dictionary (Dictionary ([[String]], Status))))
  type_defs file_name a (b, j) (c, d) y y0 t =
    (
      type_defs_1 file_name a b d y y0 t >>=
      \(g, e, u) -> (\f -> (f, e, u)) <$> type_defs_2 file_name g (j, all_entries e) c (fmap fst <$> u) b y)
  type_defs_1 ::
    (
      String ->
      [Def_or_instance_2] ->
      Dictionary Kind_0 ->
      New_dictionary Polymorphic_type ->
      Dictionary Class_6 ->
      Dictionary Class_5 ->
      Dictionary (Dictionary ([[String]], Status)) ->
      Err ([Def_or_instance_3], New_dictionary Polymorphic_type, Dictionary (Dictionary ([[String]], Status))))
  type_defs_1 file_name a b c y y0 u =
    case a of
      [] -> Right ([], c, u)
      d : e ->
        (
          type_def_1 file_name d b c y y0 u >>=
          \(f, g, u') -> (\(k, l, t') -> (f : k, l, t')) <$> type_defs_1 file_name e b g y y0 u')
  type_defs_2 ::
    (
      String ->
      [Def_or_instance_3] ->
      (Dictionary Constructor_3, Dictionary Polymorphic_type) ->
      Dictionary Polymorphic_term ->
      Dictionary (Dictionary [[String]]) ->
      Dictionary Kind_0 ->
      Dictionary Class_6 ->
      Err (Dictionary Polymorphic_term))
  type_defs_2 file_name a b c g i u =
    case a of
      [] -> Right c
      d : e -> type_def_2 file_name d b c g i u >>= \h -> type_defs_2 file_name e b h g i u
  type_eqs :: String -> Integer -> Type_2 -> Dictionary Kind_0 -> Kind_1 -> Err (Integer, [(Kind_1, Kind_1)], Type_4)
  type_eqs file_name i a d k =
    case a of
      Application_type_2 b c ->
        (
          type_eqs file_name (i + 1) b d (Arrow_kind_1 (Var_kind_1 i) k) >>=
          \(i', t, b') -> (\(i2, u, c') -> (i2, t ++ u, Application_type_4 b' c')) <$> type_eqs file_name i' c d (Var_kind_1 i))
      Name_type_2 (Name l b) ->
        case Data.Map.lookup b d of
          Nothing -> Left (Undefined "type" b file_name l)
          Just q -> Right (i, [(k, kind_0_to_1 q)], Name_type_4 b)
  type_expr ::
    (
      Loc ->
      String ->
      Maybe Name ->
      Type_4 ->
      (Dictionary Constructor_3, Dictionary Polymorphic_type) ->
      Term_2 ->
      Dictionary (Dictionary [[String]]) ->
      Integer ->
      Dictionary Kind_0 ->
      Err Term_4)
  type_expr lc file_name k0 h (d, e) f m w b =
    (
      type_expression file_name d w (Left <$> e) f (type_transf h) b >>=
      \(g, i, _) -> (\m0 -> exprrepl (typeback <$> m0) g) <$> solve_all lc file_name m k0 i Data.Map.empty)
  type_expr' ::
    (
      (Dictionary Kind_0, Dictionary Constructor_3, Dictionary Polymorphic_type) ->
      Term_2 ->
      Dictionary (Dictionary [[String]]) ->
      Err Term_4)
  type_expr' (b, d, e) f g = type_expr Input_loc "input" Nothing (Name_type_4 "!") (d, e) f g 0 b
  type_expression ::
    (
      String ->
      Dictionary Constructor_3 ->
      Integer ->
      Dictionary (Either Polymorphic_type Type_6) ->
      Term_2 ->
      Type_6 ->
      Dictionary Kind_0 ->
      Err (Term_3, Equations, Integer))
  type_expression file_name w o d b e r7 =
    case b of
      Application_term_2 c g ->
        (
          type_expression file_name w (o + 1) d c (function_type' (Var_type_6 o) e) r7 >>=
          \(i, Equations k0 k1 k2 k3, p) ->
            (
              (\(l, Equations m0 m1 m2 m3, n) ->
                (Application_term_3 i l, Equations ([o] ++ k0 ++ m0) (k1 ++ m1) (k2 ++ m2) (k3 ++ m3), n)) <$>
              type_expression file_name w p d g (Var_type_6 o) r7))
      Arrow_term_2 (Arrow_2 c f) ->
        (
          get_pattern_type file_name w (o + 2, d) c (Var_type_6 o) >>=
          \((i, j, k, h), g) ->
            (
              (\(l, Equations m n p q, s) ->
                (
                  Arrow_term_3 g l,
                  Equations
                    ([o, o + 1] ++ j ++ m)
                    (
                      [
                        (
                          e,
                          Application_type_6 (Application_type_6 (Name_type_6 "Arrow") (Var_type_6 o)) (Var_type_6 (o + 1)))] ++
                      k ++
                      n)
                    p
                    q,
                  s)) <$>
              type_expression file_name w i h f (Var_type_6 (o + 1)) r7))
      Branch_term_2 (Name a c) (f, (g, h)) ->
        let
          i = Data.Map.delete c r7
        in
          case Data.Map.lookup c (Data.Map.delete "Zero" r7) of
            Nothing -> Left (Undefined "type" c file_name a)
            Just j ->
              case j of
                Nat_kind_0 ->
                  (
                    type_expression file_name w o d f (sysrep' c (Name_type_6 "Zero") e) i >>=
                    \(k, l, m) ->
                      let
                        q =
                          case g of
                            Blank_type_pattern_3 -> c ++ "-1"
                            Name_type_pattern_3 s -> s
                      in
                        (
                          (\(s, t, u) -> (Branch_term_3 c k g s, Equations [] [] [] [Branch_equations c (l, (q, t))], u)) <$>
                          type_expression
                            file_name
                            w
                            m
                            d
                            h
                            (sysrep' c (Application_type_6 (Name_type_6 "Next") (Name_type_6 q)) e)
                            (Data.Map.insert q Nat_kind_0 i)))
                _ -> Left (Type_variable_is_not_of_kind_Nat c file_name a)
      Int_term_2 c -> Right (Int_term_3 c, Equations [] [(e, int_type')] [] [], o)
      Match_term_2 c g ->
        (
          type_expression file_name w (o + 1) d c (Var_type_6 o) r7 >>=
          \(k, Equations m0 m1 m2 m3, n) ->
            (
              (\(q, Equations u0 u1 u2 u3, x) ->
                (Match_term_3 k q, Equations (o : m0 ++ u0) (m1 ++ u1) (m2 ++ u2) (m3 ++ u3), x)) <$>
              type_cases file_name w n d g o e r7))
      Modular_term_2 (Modular_0 c g1) -> Right (Modular_term_3 g1, Equations [] [(e, mod_type' (int_to_nat' c))] [] [], o)
      Name_term_2 (Name a7 c) ->
        case Data.Map.lookup c d of
          Nothing -> Left (Undefined "term" c file_name a7)
          Just x2 ->
            Right
              (case x2 of
                Left m7 ->
                  case m7 of
                    Ad_hoc_type (Type_variable_1 d5 _) d' cl j ->
                      let
                        ((s, p), (n, n9)) = typevars d' (o + 1, Data.Map.singleton d5 (Var_type_6 o))
                      in
                        (
                          Glob_term_3 c (Just (Var_type_6 o)) n9,
                          Equations (o : n) [(e, repl_transf p j)] [(cl, (Name a7 c, Var_type_6 o))] [],
                          s)
                    Parametric_type i a' j ->
                      let
                        ((s, p), (n, n9)) = typevars i (o, Data.Map.empty)
                      in
                        (
                          Glob_term_3 c Nothing n9,
                          Equations n [(e, repl_transf p j)] ((\(Constraint_1 a0 b0) -> (a0, (Name a7 c, p ! b0))) <$> a') [],
                          s)
                Right j -> (Loc_term_3 c, Equations [] [(e, j)] [] [], o))
  type_exprs ::
    (
      String ->
      String ->
      (Dictionary Constructor_3, Dictionary Polymorphic_type) ->
      Dictionary (Dictionary [[String]]) ->
      [(Name, Term_2, [Type_variable_1], Type_4)] ->
      (Dictionary Polymorphic_term) ->
      String ->
      (Type_4 -> Type_4) ->
      Integer ->
      Dictionary Class_6 ->
      Dictionary Kind_0 ->
      [String] ->
      Err (Dictionary Polymorphic_term))
  type_exprs file_name a c d h i t z w t0 x2 f' =
    case h of
      [] -> Right i
      (Name j y, k, s, l) : m ->
        (
          type_expr
            (Method_loc file_name j y a)
            file_name
            (Just (Name j (y ++ " " ++ a)))
            (z l)
            c
            k
            d
            w
            (Data.Map.union x2 (Data.Map.fromList ((\(Type_variable_1 s0 s1) -> (s0, s1)) <$> s))) >>=
          \g ->
            type_exprs
              file_name
              a
              c
              d
              m
              (adjust (\(Ad_hoc_term u3 v3) -> Ad_hoc_term u3 (Data.Map.insert t (Implementation f' g) v3)) y i)
              t
              z
              w
              t0
              x2
              f')
  type_field ::
    (
      String ->
      (Int, Field_2) ->
      Dictionary Kind_0 ->
      (Term_4 -> Polymorphic_term) ->
      (Type_4 -> Polymorphic_type) ->
      Type_4 ->
      Err (Type_4, (New_entry Polymorphic_type, Entry Polymorphic_term)))
  type_field file_name (o, Field_2 j k) e f f' h =
    (\m -> (m, ((j, (f' (function_type h m), Public)), (j, f (Field_term_4 o))))) <$> type_typ file_name k e Star_kind_0
  type_fields ::
    (
      String ->
      [Field_2] ->
      Dictionary Kind_0 ->
      (Term_4 -> Polymorphic_term) ->
      (Type_4 -> Polymorphic_type) ->
      Type_4 ->
      Err ([Type_4], (New_dictionary Polymorphic_type, Dictionary Polymorphic_term)))
  type_fields file_name d e f f' h =
    (
      (\u -> (fst <$> u, (Data.Map.fromList (fst <$> snd <$> u), Data.Map.fromList (snd <$> snd <$> u)))) <$>
      traverse (\v -> type_field file_name v e f f' h) (zip [0 ..] d))
  type_inh :: String -> String -> [String] -> [String] -> Dictionary String -> Err ()
  type_inh file_name a b c d =
    case c of
      [] -> Right ()
      e : f ->
        case a == e of
          False -> type_inh file_name a (e : b) (maybeToList (Data.Map.lookup e d) ++ f) d
          True -> Left (Circular_inheritance_between_classes file_name b)
  type_kinds :: [Type_variable_1] -> Dictionary Kind_0 -> Dictionary Kind_0
  type_kinds c d =
    case c of
      [] -> d
      Type_variable_1 e f : g -> type_kinds g (Data.Map.insert e f d)
  type_method_1 :: String -> Dictionary Kind_0 -> Method_3 -> Err Method_4
  type_method_1 fn y (Method_3 a b d) = Method_4 a b <$> type_typ fn d (type_kinds b y) Star_kind_0
  type_methods_1 :: String -> Dictionary Kind_0 -> [Method_3] -> Err [Method_4]
  type_methods_1 fn y a =
    case a of
      [] -> Right []
      b : c -> type_method_1 fn y b >>= \d -> (:) d <$> type_methods_1 fn y c
  type_transf :: Type_4 -> Type_6
  type_transf a =
    case a of
      Application_type_4 b c -> Application_type_6 (type_transf b) (type_transf c)
      Name_type_4 b -> Name_type_6 b
  type_typ :: String -> Type_3 -> Dictionary Kind_0 -> Kind_0 -> Err Type_4
  type_typ file_name (Type_3 b c) d e =
    type_eqs file_name 0 c d (kind_0_to_1 e) >>= \(_, h, i) -> i <$ solve_type_eqs file_name b h
  type_type_variable_0 :: Type_variable_1 -> Kind_0
  type_type_variable_0 (Type_variable_1 _ kind) = kind
  type_type_variable_1 :: Type_variable_1 -> (String, Kind_0)
  type_type_variable_1 (Type_variable_1 name kind) = (name, kind)
  type_type_variables :: [Type_variable_1] -> Dictionary Kind_0 -> Dictionary Kind_0
  type_type_variables type_variables types = Data.Map.union types (Data.Map.fromList (type_type_variable_1 <$> type_variables))
  type_types :: String -> [Type_3] -> Dictionary Kind_0 -> Err [Type_4]
  type_types file_name a b =
    case a of
      [] -> Right []
      c : d -> type_typ file_name c b Star_kind_0 >>= \e -> (:) e <$> type_types file_name d b
  typest :: Dictionary Polymorphic_type
  typest =
    Data.Map.fromList
      [
        (
          "Add",
          Ad_hoc_type
            (Type_variable_1 "T" Star_kind_0)
            []
            "Ring"
            (function_type (Name_type_4 "T") (function_type (Name_type_4 "T") (Name_type_4 "T")))),
        (
          "Compare",
          Ad_hoc_type
            (Type_variable_1 "T" Star_kind_0)
            []
            "Ord"
            (function_type (Name_type_4 "T") (function_type (Name_type_4 "T") comparison_type))),
        ("Convert", Ad_hoc_type (Type_variable_1 "T" Star_kind_0) [] "Ring" (function_type int_type (Name_type_4 "T"))),
        ("Crash", Parametric_type [Type_variable_1 "T" Star_kind_0] [] (Name_type_4 "T")),
        ("Div", Parametric_type [] [] (function_type int_type (function_type int_type (maybe_type int_type)))),
        ("EQ", Parametric_type [] [] (Name_type_4 "Ordering")),
        ("GT", Parametric_type [] [] (Name_type_4 "Ordering")),
        (
          "Inverse",
          Ad_hoc_type
            (Type_variable_1 "T" Star_kind_0)
            []
            "Field"
            (function_type (Name_type_4 "T") (maybe_type (Name_type_4 "T")))),
        (
          "Just",
          Parametric_type
            [Type_variable_1 "T" Star_kind_0]
            []
            (function_type (Name_type_4 "T") (Application_type_4 (Name_type_4 "Maybe") (Name_type_4 "T")))),
        ("LT", Parametric_type [] [] (Name_type_4 "Ordering")),
        ("Mod", Parametric_type [] [] (function_type int_type (function_type int_type (maybe_type int_type)))),
        (
          "Nothing",
          Parametric_type [Type_variable_1 "T" Star_kind_0] [] (Application_type_4 (Name_type_4 "Maybe") (Name_type_4 "T"))),
        (
          "Times",
          Ad_hoc_type
            (Type_variable_1 "T" Star_kind_0)
            []
            "Ring"
            (function_type (Name_type_4 "T") (function_type (Name_type_4 "T") (Name_type_4 "T"))))]
  typestring :: Type_4 -> [Type_4] -> (String, [Type_4])
  typestring a d =
    case a of
      Application_type_4 b c -> typestring b (c : d)
      Name_type_4 b -> (b, d)
  typevars :: [Type_variable_1] -> (Integer, Dictionary Type_6) -> ((Integer, Dictionary Type_6), ([Integer], [Type_6]))
  typevars a (b, c) =
    case a of
      [] -> ((b, c), ([], []))
      Type_variable_1 d _ : e ->
        second (bimap ((:) b) ((:) (Var_type_6 b))) (typevars e (b + 1, Data.Map.insert d (Var_type_6 b) c))
  typing ::
    (
      String ->
      (
        (
          Dictionary Kind_0,
          Dictionary Constructor_3,
          Dictionary Polymorphic_type,
          Dictionary Class_6,
          Dictionary Class_5,
          Dictionary (Dictionary [[String]]),
          Dictionary Kind_0),
        Dictionary Polymorphic_term) ->
      File_4 ->
      Err
        (
          (
            Dictionary Kind_0,
            Dictionary Constructor_3,
            Dictionary Polymorphic_type,
            Dictionary Class_6,
            Dictionary Class_5,
            Dictionary (Dictionary [[String]]),
            Dictionary Kind_0),
          Dictionary Polymorphic_term))
  typing file_name ((old_types, old_constructors, old_terms, b', c5, x, t2), l) (File_4 datas classes defs_and_instances) =
    (
      type_datas file_name datas (old_types, old old_constructors) >>=
      \(all_types, public_types, h, g3, f) ->
        (
          type_classes file_name all_types classes (old b', Data.Map.union (old old_terms) g3, old t2, old c5) >>=
          \(c', g0, x2, t3) ->
            (
              (\(i, j, y) ->
                (
                  (
                    public_types,
                    public_entries h,
                    public_entries j,
                    public_entries c',
                    public_entries t3,
                    rem_old' y,
                    public_entries x2),
                  i)) <$>
              type_defs
                file_name
                defs_and_instances
                (all_types, all_entries h)
                (Data.Map.union l f, g0)
                (all_entries c')
                (all_entries t3)
                (old' x))))
--------------------------------------------------------------------------------------------------------------------------------