{-
tests
"requires instance or constraint" -> "requires instance" / "requires constraint"
internal: make the system of specifying built-in algebraic data types and things better and safer
generalise Branching data type to branch from any of the type variables?
    Branching Array[! : Nat, T : Star]{Zero -> ..., Next N -> ...}
remove special semantics of missing pattern match arguments?
Allow hiding things to functions outside module - so that helper functions are not exported from the module?
move modular checking to parser?
allow to make algebraic data types and branching data types hidden?
expand restricted constructor thing to algebraic and branching data types and definitions (and class methods?)
check for duplicate constraints everywhere
take some things out of imported-only contexts and put them into global program context
  (they can result in unfound things and should be included everywhere transitively)
change hidden/private parsing from keyword Hidden to some symbol/operator before the token?
algebraic pattern matching - check in Typing module that all constructors are actually legit
is circular inheritance a problem when we don't have type erasure and hidden arguments? if not, remove checks
check if things are put correctly into local / global context
add Modular arithmetic methods to Nonzero class; this also prevents the user for extending Nonzero to Zero!
div - give error not only when divisor is zero but also when divisor is negative!
Check that incomplete patterns don't crash but give a nice error during execution of the program
circular inheritance checking should be moved into second pass of class checking?
-}
--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Typing (
  Algebraic (..),
  Arrow_4 (..),
  Class_5 (..),
  Class_6 (..),
  Constructor_3 (..),
  Expr_2 (..),
  Term_4 (..),
  Term_pattern_6 (..),
  Type_4 (..),
  Type_5 (..),
  algebraics',
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
  data Algebraic = Algebraic [Type_variable_1] [Constructor_3] deriving Show
  data Arrow_3 = Arrow_3 Term_pattern_6 Term_3 deriving Show
  data Arrow_4 = Arrow_4 Term_pattern_6 Term_4 deriving Show
  data Branch_equations = Branch_equations String (Equations, (String, Equations)) deriving Show
  data Class_4 = Class_4 String Type_variable_1 [Name] [Method_3] deriving Show
  data Class_5 = Class_5 Kind_0 [String] deriving Show
  data Class_6 = Class_6 Type_variable_1 [String] [Method_4] deriving Show
  data Constraint_1 = Constraint_1 String String deriving Show
  data Constructor_3 = Constructor_3 String [Type_4] deriving Show
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
      [(Name, Term_2, [Type_variable_1], [Constraint_1], Type_4)]
      Type_4
      [Constraint_1]
        deriving Show
  data Method_4 = Method_4 String [Type_variable_1] [Constraint_1] Type_4 deriving Show
  data Term_pattern_6 =
    Blank_term_pattern_6 |
    Int_term_pattern_6 Integer |
    Modular_term_pattern_6 Integer |
    Name_term_pattern_6 String |
    Struct_term_pattern_6 String [Term_pattern_6]
      deriving Show
  data Equations = Equations [Integer] [(Type_6, Type_6)] [(String, (Name, Type_6))] [Branch_equations] deriving Show
  data Expr_2 = Expr_2 (Maybe [String]) [String] Term_4 deriving Show
  data Form_2 = Form_2 String [Type_4] deriving Show
  data Kind_1 = Arrow_kind_1 Kind_1 Kind_1 | Nat_kind_1 | Star_kind_1 | Var_kind_1 Integer deriving (Eq, Show)
  data Nat = Nxt Nat | Zr deriving (Eq, Ord, Show)
  data Status' = Old | New deriving (Eq, Show)
  data Term_3 =
    Application_expression_4 Term_3 Term_3 |
    Branch_expression_4 String Term_3 Type_pattern_3 Term_3 |
    Function_expression_4 Term_pattern_6 Term_3 |
    Glob_expression_4 String (Maybe Type_6) [Type_6] |
    Int_expression_4 Integer |
    Loc_expression_4 String |
    Match_expression_4 Term_3 [Arrow_3] |
    Modular_expression_4 Integer
      deriving Show
  data Term_4 =
    Add_Int_0_expression_2 |
    Add_Int_1_expression_2 Integer |
    Add_Modular_0_expression_2 Integer |
    Add_Modular_1_expression_2 Integer Integer |
    Algebraic_expression_2 String [Term_4] |
    Application_expression_2 Term_4 Term_4 |
    Branch_expression_2 Type_4 Term_4 Type_pattern_3 Term_4 |
    Compare_Int_0_expression_2 |
    Compare_Int_1_expression_2 Integer |
    Compare_Modular_0_expression_2 |
    Compare_Modular_1_expression_2 Integer |
    Constr_expression_2  |
    Convert_Int_expression_2 |
    Convert_Modular_expression_2 Integer |
    Div_0_expression_2 |
    Div_1_expression_2 Integer |
    Field_expression_2 Integer |
    Function_expression_2 Term_pattern_6 Term_4 |
    Glob_expression_2 String (Maybe Type_4) [Type_4] |
    Int_expression_2 Integer |
    Inverse_Modular_expression_2 Integer |
    Loc_expression_2 String |
    Match_expression_2 Term_4 [Arrow_4] |
    Mod_0_expression_2 |
    Mod_1_expression_2 Integer |
    Modular_expression_2 Integer |
    Multiply_Int_0_expression_2 |
    Multiply_Int_1_expression_2 Integer |
    Multiply_Modular_0_expression_2 Integer |
    Multiply_Modular_1_expression_2 Integer Integer |
    Negate_Int_expression_2 |
    Negate_Modular_expression_2 Integer |
    Write_Brackets_Int_expression_2 |
    Write_Brackets_Modular_expression_2 Integer
      deriving Show
  data Type_4 = Application_type_4 Type_4 Type_4 | Name_type_4 String deriving Show
  data Type_5 = Basic_type_4 [Type_variable_1] (Maybe Constraint_1) [Constraint_1] Type_4 deriving Show
  data Type_6 = Application_type_6 Type_6 Type_6 | Name_type_6 String | Var_type_6 Integer deriving Show
  algebraics :: Dictionary [Type_variable_1]
  algebraics = (\(Algebraic a _) -> a) <$> Data.Map.fromList algebraics'
  algebraics' :: [(String, Algebraic)]
  algebraics' =
    [
      (
        "Maybe",
        Algebraic [(Type_variable_1 "T" Star_kind_0)] [Constructor_3 "Nothing" [], Constructor_3 "Wrap" [Name_type_4 "T"]]),
      ("Ordering", Algebraic [] [Constructor_3 "LT" [], Constructor_3 "EQ" [], Constructor_3 "GT" []])]
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
            [Method_4 "Inverse" [] [] (function_type (Name_type_4 "T") (maybe_type (Name_type_4 "T")))]),
        ("Nonzero", Class_6 (Type_variable_1 "N" Nat_kind_0) [] []),
        (
          "Ord",
          Class_6
            (Type_variable_1 "T" Star_kind_0)
            []
            [Method_4 "Compare" [] [] (function_type (Name_type_4 "T") (function_type (Name_type_4 "T") comparison_type))]),
        (
          "Ring",
          Class_6
            (Type_variable_1 "T" Star_kind_0)
            []
            [
              Method_4 "Add" [] [] (function_type (Name_type_4 "T") (function_type (Name_type_4 "T") (Name_type_4 "T"))),
              Method_4 "Convert" [] [] (function_type int_type (Name_type_4 "T")),
              Method_4 "Times" [] [] (function_type (Name_type_4 "T") (function_type (Name_type_4 "T") (Name_type_4 "T")))])]
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
  constrs = join ((\(a, Algebraic _ b) -> (\(Constructor_3 c d) -> (c, Constructor_3 a d)) <$> b) <$> algebraics')
  context_union ::
    (
      (
        (
          Dictionary Kind_0,
          Dictionary [Type_variable_1],
          Dictionary Constructor_3,
          Dictionary Type_5,
          Dictionary Class_6,
          Dictionary Class_5,
          Dictionary (Dictionary [[String]]),
          Dictionary Kind_0),
        (Dictionary Operator_0, Dictionary Operator_0)) ->
      (
        (
          Dictionary Kind_0,
          Dictionary [Type_variable_1],
          Dictionary Constructor_3,
          Dictionary Type_5,
          Dictionary Class_6,
          Dictionary Class_5,
          Dictionary (Dictionary [[String]]),
          Dictionary Kind_0),
        (Dictionary Operator_0, Dictionary Operator_0)) ->
      (
        (
          Dictionary Kind_0,
          Dictionary [Type_variable_1],
          Dictionary Constructor_3,
          Dictionary Type_5,
          Dictionary Class_6,
          Dictionary Class_5,
          Dictionary (Dictionary [[String]]),
          Dictionary Kind_0),
        (Dictionary Operator_0, Dictionary Operator_0)))
  context_union ((b, i, j, d, e, q, t, g), (a7, t0)) ((f, k, l, h, m, r, u, n), (a8, t2)) =
    (
      (
        Data.Map.union b f,
        Data.Map.union i k,
        Data.Map.union j l,
        Data.Map.union d h,
        Data.Map.union e m,
        Data.Map.union q r,
        unionWith Data.Map.union t u,
        Data.Map.union g n),
      (Data.Map.union a7 a8, Data.Map.union t0 t2))
  defs :: Dictionary Expr_2
  defs =
    Data.Map.fromList
      (
        join
          (
            (\(_, Algebraic a c) ->
              (\(Constructor_3 d _) ->
                (d, Expr_2 Nothing ((\(Type_variable_1 g _) -> g) <$> a) (Algebraic_expression_2 d []))) <$> c) <$>
            algebraics') ++
        [
          ("Add Int", Expr_2 (Just []) [] Add_Int_0_expression_2),
          ("Compare Int", Expr_2 (Just []) [] Compare_Int_0_expression_2),
          ("Convert Int", Expr_2 (Just []) [] Convert_Int_expression_2),
          ("Div", Expr_2 Nothing [] Div_0_expression_2),
          ("Mod", Expr_2 Nothing [] Mod_0_expression_2),
          ("Times Int", Expr_2 (Just []) [] Multiply_Int_0_expression_2),
          ("Write_brackets Int", Expr_2 (Just []) [] Write_Brackets_Int_expression_2)])
  exprrepl :: Map Integer Type_4 -> Term_3 -> Term_4
  exprrepl a b =
    case b of
      Application_expression_4 c d -> Application_expression_2 (exprrepl a c) (exprrepl a d)
      Branch_expression_4 c d e f -> Branch_expression_2 (Name_type_4 c) (exprrepl a d) e (exprrepl a f)
      Function_expression_4 c d -> Function_expression_2 c (exprrepl a d)
      Glob_expression_4 c d e -> Glob_expression_2 c (exprrepl' a <$> d) (exprrepl' a <$> e)
      Int_expression_4 c -> Int_expression_2 c
      Loc_expression_4 c -> Loc_expression_2 c
      Match_expression_4 c d -> Match_expression_2 (exprrepl a c) ((\(Arrow_3 e f) -> Arrow_4 e (exprrepl a f)) <$> d)
      Modular_expression_4 c -> Modular_expression_2 c
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
      Dictionary [Type_variable_1] ->
      Dictionary Constructor_3 ->
      (Integer, Dictionary (Either Type_5 Type_6)) ->
      Term_pattern_5 ->
      Type_6 ->
      Err ((Integer, [Integer], [(Type_6, Type_6)], Dictionary (Either Type_5 Type_6)), Term_pattern_6))
  get_pattern_type file_name b c (d, n) g h =
    case g of
      Blank_term_pattern_5 -> Right ((d, [], [], n), Blank_term_pattern_6)
      Int_term_pattern_5 i -> Right ((d, [], [(h, int_type')], n), Int_term_pattern_6 i)
      Modular_term_pattern_5 (Modular_0 j z) -> Right ((d, [], [(h, mod_type' (int_to_nat' j))], n), Modular_term_pattern_6 z)
      Name_term_pattern_5 i -> Right ((d, [], [], Data.Map.insert i (Right h) n), Name_term_pattern_6 i)
      Struct_term_pattern_5 (Name o i) j ->
        case Data.Map.lookup i c of
          Nothing -> Left (Undefined "constructor" i file_name o)
          Just (Constructor_3 p y) ->
            let
              ((q, r), (s, q4)) = typevars (b ! p) (d, Data.Map.empty)
            in
              (
                bimap
                  (\(t, x, y', z) -> (t, s ++ x, (h, Prelude.foldl Application_type_6 (Name_type_6 p) q4) : y', z))
                  (Struct_term_pattern_6 i) <$>
                get_pattern_types file_name b c (q, n) j (repl_transf r <$> y) (Name o i))
  get_pattern_types ::
    (
      String ->
      Dictionary [Type_variable_1] ->
      Dictionary Constructor_3 ->
      (Integer, Dictionary (Either Type_5 Type_6)) ->
      [Term_pattern_5] ->
      [Type_6] ->
      Name ->
      Err ((Integer, [Integer], [(Type_6, Type_6)], Dictionary (Either Type_5 Type_6)), [Term_pattern_6]))
  get_pattern_types fn b c (d0, d3) e f (Name m n) =
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
                get_pattern_type fn b c (d0, d3) g i >>=
                \((k, k0, k1, k2), l) ->
                  (
                    bimap (\(m0, m1, m2, m3) -> (m0, k0 ++ m1, k1 ++ m2, m3)) ((:) l) <$>
                    get_pattern_types fn b c (k, k2) h j (Name m n)))
  init_type_context ::
    (
      (
        Dictionary Kind_0,
        Dictionary [Type_variable_1],
        Dictionary Constructor_3,
        Dictionary Type_5,
        Dictionary Class_6,
        Dictionary Class_5,
        Dictionary (Dictionary [[String]]),
        Dictionary Kind_0),
      (Dictionary Operator_0, Dictionary Operator_0))
  init_type_context =
    (
      (kinds, algebraics, Data.Map.fromList constrs, types, classes_0, classes_1, instances, classes_2),
      (Data.Map.empty, Data.Map.empty))
  ins_new :: String -> t -> Dictionary (t, Status') -> Dictionary (t, Status')
  ins_new a b = Data.Map.insert a (b, New)
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
  old :: Dictionary t -> Dictionary (t, Status')
  old = fmap (\a -> (a, Old))
  old' :: Dictionary (Dictionary t) -> Dictionary (Dictionary (t, Status'))
  old' = (<$>) old
  rem_old :: Dictionary (t, Status') -> Dictionary t
  rem_old x = fst <$> Data.Map.filter (\(_, b) -> New == b) x
  rem_old' :: Dictionary (Dictionary (t, Status')) -> Dictionary (Dictionary t)
  rem_old' a = Data.Map.filter (\b -> not (Data.Map.null b)) (rem_old <$> a)
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
  typeback :: Type_6 -> Type_4
  typeback a =
    case a of
      Application_type_6 b c -> Application_type_4 (typeback b) (typeback c)
      Name_type_6 b -> Name_type_4 b
      Var_type_6 _ -> undefined
  type_br ::
    (
      Status ->
      String ->
      String ->
      [Type_variable_1] ->
      Data_branch_3 ->
      Dictionary Kind_0 ->
      String ->
      (
        Dictionary ([Type_variable_1], Status'),
        Dictionary (Type_5, Status'),
        Dictionary (Constructor_3, Status'),
        Dictionary Expr_2) ->
      Err
        (
          Dictionary ([Type_variable_1], Status'),
          Dictionary (Type_5, Status'),
          Dictionary (Constructor_3, Status'),
          Dictionary Expr_2))
  type_br stat file_name b c d e r (f, o, p, q) =
    let
      g = Prelude.foldl (\h -> \(Type_variable_1 i n) -> Data.Map.insert i n h) e c
      s = Prelude.foldl Application_type_4 (Name_type_4 b) (Name_type_4 <$> (\(Type_variable_1 c' _) -> c') <$> c)
    in
      case d of
        Algebraic_data_3 j ->
          (
            (\(i, k, l) -> (ins_new r c f, i, k, l)) <$>
            type_forms file_name c j g (Expr_2 Nothing ((\(Type_variable_1 c' _) -> c') <$> c)) r s (o, p, q))
        Branch_data_3 (Name h j) (k, (l, m)) ->
          let
            i = Data.Map.delete j e
          in
            (
              type_br_help file_name (Name h j) c >>=
              \(n, t) ->
                (
                  type_br stat file_name b n k i (r ++ " Zero") (f, o, p, q) >>=
                  type_br stat file_name b t m (Data.Map.insert l Nat_kind_0 i) (r ++ " Next")))
        Struct_data_3 j k l ->
          (
            (\(h, (m, u)) ->
              (
                ins_new r c f,
                Data.Map.insert
                  k
                  (
                    Basic_type_4 c Nothing [] (Prelude.foldr function_type s h),
                    case j of
                      Private -> Old
                      Public -> New)
                  m,
                ins_new k (Constructor_3 r h) p,
                Data.Map.insert k (Expr_2 Nothing ((\(Type_variable_1 c' _) -> c') <$> c) (Algebraic_expression_2 k [])) u)) <$>
            type_fields file_name k c l g (Expr_2 Nothing ((\(Type_variable_1 c' _) -> c') <$> c)) s 0 (o, q))
  type_br_help :: String -> Name -> [Type_variable_1] -> Err ([Type_variable_1], [Type_variable_1])
  type_br_help file_name (Name a b) c =
    case c of
      [] -> Left (Undefined "type variable" b file_name a)
      Type_variable_1 d e : f ->
        case b == d of
          False -> bimap ((:) (Type_variable_1 d e)) ((:) (Type_variable_1 d e)) <$> type_br_help file_name (Name a b) f
          True ->
            case e of
              Nat_kind_0 -> Right (f, Type_variable_1 b Nat_kind_0 : f)
              _ -> Left (Type_variable_is_not_of_kind_Nat b file_name a)
  type_case ::
    (
      String ->
      Dictionary [Type_variable_1] ->
      Dictionary Constructor_3 ->
      Integer ->
      Dictionary (Either Type_5 Type_6) ->
      Arrow_2 ->
      Integer ->
      Type_6 ->
      Dictionary Kind_0 ->
      Err (Arrow_3, Equations, Integer))
  type_case file_name a b d f (Arrow_2 g h) i j k =
    (
      get_pattern_type file_name a b (d, f) g (Var_type_6 i) >>=
      \((m, n, s, t), o) ->
        (
          (\(u, Equations e p q r, w) -> (Arrow_3 o u, Equations (n ++ e) (s ++ p) q r, w)) <$>
          type_expression file_name a b m t h j k))
  type_cases ::
    (
      String ->
      Dictionary [Type_variable_1] ->
      Dictionary Constructor_3 ->
      Integer ->
      Dictionary (Either Type_5 Type_6) ->
      [Arrow_2] ->
      Integer ->
      Type_6 ->
      Dictionary Kind_0 ->
      Err ([Arrow_3], Equations, Integer))
  type_cases file_name a b d f g n h i =
    case g of
      [] -> Right ([], Equations [] [] [] [], d)
      l : m ->
        (
          type_case file_name a b d f l n h i >>=
          \(o, Equations p0 p1 p2 p3, q) ->
            (
              (\(r, Equations s0 s1 s2 s3, t) -> (o : r, Equations (p0 ++ s0) (p1 ++ s1) (p2 ++ s2) (p3 ++ s3), t)) <$>
              type_cases file_name a b q f m n h i))
  type_class_0 ::
    (
      String ->
      Class_3 ->
      (Dictionary (Kind_0, Status'), Dictionary (Class_5, Status'), Dictionary String) ->
      Err (Class_4, (Dictionary (Kind_0, Status'), Dictionary (Class_5, Status'), Dictionary String)))
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
      Dictionary Class_5 ->
      (Dictionary (Type_5, Status'), Dictionary (Class_6, Status')) ->
      Err (Dictionary (Type_5, Status'), Dictionary (Class_6, Status')))
  type_class_1 fn (Class_4 b (Type_variable_1 c k) g' e) y4 d f1 (f0, f) =
    let
      x1 = Constraint_1 b c
    in
      (
        (\m -> \e' ->
          (
            Prelude.foldl
              (\x -> \(Method_4 t s u0 u) ->
                ins_new t (Basic_type_4 ((Type_variable_1 c k) : s) (Just x1) (x1 : u0) u) x)
              f0
              e',
            ins_new b (Class_6 (Type_variable_1 c k) m e') f)) <$>
        traverse
          (\(Name m g) ->
            case Data.Map.lookup g d of
              Nothing -> Left (Undefined "class" g fn m)
              Just h -> if h == k then Right g else Left (Kind_mismatch_in_class fn m))
          g' <*>
        type_methods_1 fn b f1 d e)
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
      (
        Dictionary (Class_6, Status'),
        Dictionary (Type_5, Status'),
        Dictionary (Kind_0, Status'),
        Dictionary (Class_5, Status')) ->
      Err
        (
          Dictionary (Class_6, Status'),
          Dictionary (Type_5, Status'),
          Dictionary (Kind_0, Status'),
          Dictionary (Class_5, Status')))
  type_classes file_name c d (e, g, o, o') =
    (
      type_classes_0 file_name d (o, o', Data.Map.empty) >>=
      \(r, (p, p', _)) -> (\(k, n) -> (n, k, p, p')) <$> type_classes_1 file_name r c (fst <$> p) (fst <$> p') (g, e))
  type_classes_0 ::
    (
      String ->
      [Class_3] ->
      (Dictionary (Kind_0, Status'), Dictionary (Class_5, Status'), Dictionary String) ->
      Err ([Class_4], (Dictionary (Kind_0, Status'), Dictionary (Class_5, Status'), Dictionary String)))
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
      Dictionary Class_5 ->
      (Dictionary (Type_5, Status'), Dictionary (Class_6, Status')) ->
      Err (Dictionary (Type_5, Status'), Dictionary (Class_6, Status')))
  type_classes_1 fn b y h i c =
    case b of
      [] -> Right c
      d : e -> type_class_1 fn d y h i c >>= type_classes_1 fn e y h i
  type_cls_0 ::
    (
      String ->
      String ->
      [Method_4] ->
      Type_4 ->
      [Binding_2] ->
      String ->
      Line_and_char ->
      Err [(Name, Term_2, [Type_variable_1], [Constraint_1], Type_4)])
  type_cls_0 fn a b c d m n =
    case d of
      [] ->
        case b of
          [] -> Right []
          Method_4 e _ _ _ : _ -> Left (Missing_definition fn e m a n)
      Binding_2 (Name h i) j : k ->
        case b of
          [] -> Left (Def_err fn i h m)
          Method_4 e s y f : g ->
            case i == e of
              False -> Left (Expected fn e h i)
              True -> (:) ((Name h i), j, s, y, f) <$> type_cls_0 fn a g c k m n
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
  type_data_1 :: Data_3 -> Dictionary (Kind_0, Status') -> Dictionary (Kind_0, Status')
  type_data_1 (Data_3 stat a b _) =
    Data.Map.insert
      a
      (
        Prelude.foldr Arrow_kind_0 Star_kind_0 ((\(Type_variable_1 _ c) -> c) <$> b),
        case stat of
          Private -> Old
          Public -> New)
  type_data_2 ::
    (
      String ->
      Data_3 ->
      Dictionary Kind_0 ->
      (
        Dictionary ([Type_variable_1], Status'),
        Dictionary (Type_5, Status'),
        Dictionary (Constructor_3, Status'),
        Dictionary Expr_2) ->
      Err
        (
          Dictionary ([Type_variable_1], Status'),
          Dictionary (Type_5, Status'),
          Dictionary (Constructor_3, Status'),
          Dictionary Expr_2))
  type_data_2 file_name (Data_3 x b c d) e = type_br x file_name b c d e b
  type_datas ::
    (
      String ->
      [Data_3] ->
      (
        Dictionary (Kind_0, Status'),
        Dictionary ([Type_variable_1], Status'),
        Dictionary (Constructor_3, Status'),
        Dictionary (Type_5, Status'),
        Dictionary Expr_2) ->
      Err
        (
          Dictionary (Kind_0, Status'),
          Dictionary ([Type_variable_1], Status'),
          Dictionary (Constructor_3, Status'),
          Dictionary (Type_5, Status'),
          Dictionary Expr_2))
  type_datas fn a (b, i, j, d, c) =
    let
      u = type_datas_1 a b
    in
      (\(b', c', v, w) -> (u, b', v, c', w)) <$> type_datas_2 fn a (fst <$> u) (i, d, j, c)
  type_datas_1 :: [Data_3] -> Dictionary (Kind_0, Status') -> Dictionary (Kind_0, Status')
  type_datas_1 = flip (Prelude.foldl (flip type_data_1))
  type_datas_2 ::
    (
      String ->
      [Data_3] ->
      Dictionary Kind_0 ->
      (
        Dictionary ([Type_variable_1], Status'),
        Dictionary (Type_5, Status'),
        Dictionary (Constructor_3, Status'),
        Dictionary Expr_2) ->
      Err
        (
          Dictionary ([Type_variable_1], Status'),
          Dictionary (Type_5, Status'),
          Dictionary (Constructor_3, Status'),
          Dictionary Expr_2))
  type_datas_2 fn a b c =
    case a of
      [] -> Right c
      d : e -> type_data_2 fn d b c >>= type_datas_2 fn e b
  type_def_1 ::
    (
      String ->
      Def_or_instance_2 ->
      Dictionary Kind_0 ->
      Dictionary (Type_5, Status') ->
      Dictionary Class_6 ->
      Dictionary Class_5 ->
      Dictionary (Dictionary ([[String]], Status')) ->
      Err (Def_or_instance_3, Dictionary (Type_5, Status'), Dictionary (Dictionary ([[String]], Status'))))
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
                (\h ->
                  (
                    Def_3 lc d e o1 h i,
                    Data.Map.insert
                      d
                      (
                        Basic_type_4 e Nothing o1 h,
                        case f of
                          Private -> Old
                          Public -> New)
                      c,
                    t')) <$>
                type_typ file_name d (Type_3 lc g) (type_kinds e b) Star_kind_0))
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
                                  Nothing -> Data.Map.insert m (Data.Map.singleton n (r', New))) t')) <$>
                            type_cls_0 file_name n q s' g m d)))
  type_def_2 ::
    (
      String ->
      Def_or_instance_3 ->
      (Dictionary [Type_variable_1], Dictionary Constructor_3, Dictionary Type_5) ->
      Dictionary Expr_2 ->
      Dictionary (Dictionary [[String]]) ->
      Dictionary Kind_0 ->
      Dictionary Class_6 ->
      Err (Dictionary Expr_2))
  type_def_2 file_name a (d, l, k) c m n u0 =
    case a of
      Def_3 r e b x h i ->
        (
          (\t -> Data.Map.insert e (Expr_2 Nothing ((\(Type_variable_1 e2 _) -> e2) <$> b) t) c) <$>
          type_expr
            (Def_loc file_name r e)
            file_name
            (Just (Name r e))
            h
            (d, l, k)
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
            (d, l, k)
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
      (Dictionary Kind_0, Dictionary [Type_variable_1], Dictionary Constructor_3) ->
      (Dictionary Expr_2, Dictionary (Type_5, Status')) ->
      Dictionary Class_6 ->
      Dictionary Class_5 ->
      Dictionary (Dictionary ([[String]], Status')) ->
      Err (Dictionary Expr_2, Dictionary (Type_5, Status'), Dictionary (Dictionary ([[String]], Status'))))
  type_defs file_name a (b, i, j) (c, d) y y0 t =
    (
      type_defs_1 file_name a b d y y0 t >>=
      \(g, e, u) -> (\f -> (f, e, u)) <$> type_defs_2 file_name g (i, j, fst <$> e) c (fmap fst <$> u) b y)
  type_defs_1 ::
    (
      String ->
      [Def_or_instance_2] ->
      Dictionary Kind_0 ->
      Dictionary (Type_5, Status') ->
      Dictionary Class_6 ->
      Dictionary Class_5 ->
      Dictionary (Dictionary ([[String]], Status')) ->
      Err ([Def_or_instance_3], Dictionary (Type_5, Status'), Dictionary (Dictionary ([[String]], Status'))))
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
      (Dictionary [Type_variable_1], Dictionary Constructor_3, Dictionary Type_5) ->
      Dictionary Expr_2 ->
      Dictionary (Dictionary [[String]]) ->
      Dictionary Kind_0 ->
      Dictionary Class_6 ->
      Err (Dictionary Expr_2))
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
      (Dictionary [Type_variable_1], Dictionary Constructor_3, Dictionary Type_5) ->
      Term_2 ->
      Dictionary (Dictionary [[String]]) ->
      Integer ->
      Dictionary Kind_0 ->
      Err Term_4)
  type_expr lc file_name k0 h (c, d, e) f m w b =
    (
      type_expression file_name c d w (Left <$> e) f (type_transf h) b >>=
      \(g, i, _) -> (\m0 -> exprrepl (typeback <$> m0) g) <$> solve_all lc file_name m k0 i Data.Map.empty)
  type_expr' ::
    (
      (Dictionary Kind_0, Dictionary [Type_variable_1], Dictionary Constructor_3, Dictionary Type_5) ->
      Term_2 ->
      Dictionary (Dictionary [[String]]) ->
      Err Term_4)
  type_expr' (b, c, d, e) f g = type_expr Input_loc "input" Nothing (Name_type_4 "!") (c, d, e) f g 0 b
  type_expression ::
    (
      String ->
      Dictionary [Type_variable_1] ->
      Dictionary Constructor_3 ->
      Integer ->
      Dictionary (Either Type_5 Type_6) ->
      Term_2 ->
      Type_6 ->
      Dictionary Kind_0 ->
      Err (Term_3, Equations, Integer))
  type_expression file_name v w o d b e r7 =
    case b of
      Application_term_2 c g ->
        (
          type_expression file_name v w (o + 1) d c (function_type' (Var_type_6 o) e) r7 >>=
          \(i, Equations k0 k1 k2 k3, p) ->
            (
              (\(l, Equations m0 m1 m2 m3, n) ->
                (Application_expression_4 i l, Equations ([o] ++ k0 ++ m0) (k1 ++ m1) (k2 ++ m2) (k3 ++ m3), n)) <$>
              type_expression file_name v w p d g (Var_type_6 o) r7))
      Arrow_term_2 (Arrow_2 c f) ->
        (
          get_pattern_type file_name v w (o + 2, d) c (Var_type_6 o) >>=
          \((i, j, k, h), g) ->
            (
              (\(l, Equations m n p q, s) ->
                (
                  Function_expression_4 g l,
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
              type_expression file_name v w i h f (Var_type_6 (o + 1)) r7))
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
                    type_expression file_name v w o d f (sysrep' c (Name_type_6 "Zero") e) i >>=
                    \(k, l, m) ->
                      let
                        q =
                          case g of
                            Blank_type_pattern_3 -> c ++ "-1"
                            Name_type_pattern_3 s -> s
                      in
                        (
                          (\(s, t, u) ->
                            (Branch_expression_4 c k g s, Equations [] [] [] [Branch_equations c (l, (q, t))], u)) <$>
                          type_expression
                            file_name
                            v
                            w
                            m
                            d
                            h
                            (sysrep' c (Application_type_6 (Name_type_6 "Next") (Name_type_6 q)) e)
                            (Data.Map.insert q Nat_kind_0 i)))
                _ -> Left (Type_variable_is_not_of_kind_Nat c file_name a)
      Int_term_2 c -> Right (Int_expression_4 c, Equations [] [(e, int_type')] [] [], o)
      Match_term_2 c g ->
        (
          type_expression file_name v w (o + 1) d c (Var_type_6 o) r7 >>=
          \(k, Equations m0 m1 m2 m3, n) ->
            (
              (\(q, Equations u0 u1 u2 u3, x) ->
                (Match_expression_4 k q, Equations (o : m0 ++ u0) (m1 ++ u1) (m2 ++ u2) (m3 ++ u3), x)) <$>
              type_cases file_name v w n d g o e r7))
      Modular_term_2 (Modular_0 c g1) -> Right (Modular_expression_4 g1, Equations [] [(e, mod_type' (int_to_nat' c))] [] [], o)
      Name_term_2 (Name a7 c) ->
        case Data.Map.lookup c d of
          Nothing -> Left (Undefined "term" c file_name a7)
          Just x2 ->
            Right
              (case x2 of
                Left (Basic_type_4 i x0 a' j) ->
                  let
                    g7 k2 d3 e4 s' r' =
                      let
                        ((s, p), (n, n9)) = typevars d3 (s', e4)
                      in
                        (
                          Glob_expression_4 c k2 n9,
                          Equations
                            (r' ++ n)
                            [(e, repl_transf p j)]
                            ((\(Constraint_1 a0 b0) -> (a0, (Name a7 c, p ! b0))) <$> a')
                            [],
                          s)
                  in
                    case x0 of
                      Just (Constraint_1 _ _) ->
                        case i of
                          [] -> undefined
                          Type_variable_1 d5 _ : d' ->
                            g7 (Just (Var_type_6 o)) d' (Data.Map.singleton d5 (Var_type_6 o)) (o + 1) [o]
                      Nothing -> g7 Nothing i Data.Map.empty o []
                Right j -> (Loc_expression_4 c, Equations [] [(e, j)] [] [], o))
  type_exprs ::
    (
      String ->
      String ->
      (Dictionary [Type_variable_1], Dictionary Constructor_3, Dictionary Type_5) ->
      Dictionary (Dictionary [[String]]) ->
      [(Name, Term_2, [Type_variable_1], [Constraint_1], Type_4)] ->
      (Dictionary Expr_2) ->
      String ->
      (Type_4 -> Type_4) ->
      Integer ->
      Dictionary Class_6 ->
      Dictionary Kind_0 ->
      [String] ->
      Err (Dictionary Expr_2))
  type_exprs file_name a c d h i t z w t0 x2 f' =
    case h of
      [] -> Right i
      (Name j y, k, s, t5, l) : m ->
        (
          type_expr
            (Method_loc file_name j y a)
            file_name
            (Just (Name j (y ++ " " ++ a)))
            (z l)
            c
            k
            (type_constraints_1 t5 d t0)
            w
            (Data.Map.union x2 (Data.Map.fromList ((\(Type_variable_1 s0 s1) -> (s0, s1)) <$> s))) >>=
          \g ->
            type_exprs
              file_name
              a
              c
              d
              m
              (Data.Map.insert (y ++ " " ++ t) (Expr_2 (Just f') ((\(Type_variable_1 s' _) -> s') <$> s) g) i)
              t
              z
              w
              t0
              x2
              f')
  type_fields ::
    (
      String ->
      String ->
      [Type_variable_1] ->
      [Field_2] ->
      Dictionary Kind_0 ->
      (Term_4 -> Expr_2) ->
      Type_4 ->
      Integer ->
      (Dictionary (Type_5, Status'), Dictionary Expr_2) ->
      Err ([Type_4], (Dictionary (Type_5, Status'), Dictionary Expr_2)))
  type_fields file_name x c d e f h o (i, n) =
    case d of
      [] -> Right ([], (i, n))
      Field_2 j k : l ->
        (
          type_typ file_name x k e Star_kind_0 >>=
          \m ->
            (
              first ((:) m) <$>
              type_fields
                file_name
                x
                c
                l
                e
                f
                h
                (o + 1)
                (ins_new j (Basic_type_4 c Nothing [] (function_type h m)) i, Data.Map.insert j (f (Field_expression_2 o)) n)))
  type_form ::
    (
      String ->
      [Type_variable_1] ->
      Constructor_2 ->
      Dictionary Kind_0 ->
      (Term_4 -> Expr_2) ->
      String ->
      Type_4 ->
      (Dictionary (Type_5, Status'), Dictionary (Constructor_3, Status'), Dictionary Expr_2) ->
      Err (Dictionary (Type_5, Status'), Dictionary (Constructor_3, Status'), Dictionary Expr_2))
  type_form file_name c (Constructor_2 d e f) g m h n (i, j, k) =
    (
      (\l ->
        (
          Data.Map.insert
            e
            (
              Basic_type_4 c Nothing [] (Prelude.foldr function_type n l),
              case d of
                Private -> Old
                Public -> New)
            i,
          ins_new e (Constructor_3 h l) j,
          Data.Map.insert e (m (Algebraic_expression_2 e [])) k)) <$>
      type_types file_name h f g)
  type_forms ::
    (
      String ->
      [Type_variable_1] ->
      [Constructor_2] ->
      Dictionary Kind_0 ->
      (Term_4 -> Expr_2) ->
      String ->
      Type_4 ->
      (Dictionary (Type_5, Status'), Dictionary (Constructor_3, Status'), Dictionary Expr_2) ->
      Err (Dictionary (Type_5, Status'), Dictionary (Constructor_3, Status'), Dictionary Expr_2))
  type_forms file_name c d e i l m f =
    case d of
      [] -> Right f
      g : h -> type_form file_name c g e i l m f >>= type_forms file_name c h e i l m
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
  type_method_1 :: String -> String -> Dictionary Class_5 -> Dictionary Kind_0 -> Method_3 -> Err Method_4
  type_method_1 fn x g y (Method_3 a b c d) =
    let
      m = Prelude.foldl (\h -> \(Type_variable_1 i j) -> Data.Map.insert i j h) Data.Map.empty b
    in
      (
        Method_4 a b <$>
        type_constraints_0 fn Data.Map.empty c (g, m, (\_ -> Zr) <$> m) <*>
        type_typ fn (a ++ " " ++ x) d (type_kinds b y) Star_kind_0)
  type_methods_1 :: String -> String -> Dictionary Class_5 -> Dictionary Kind_0 -> [Method_3] -> Err [Method_4]
  type_methods_1 fn x f y a =
    case a of
      [] -> Right []
      b : c -> type_method_1 fn x f y b >>= \d -> (:) d <$> type_methods_1 fn x f y c
  type_transf :: Type_4 -> Type_6
  type_transf a =
    case a of
      Application_type_4 b c -> Application_type_6 (type_transf b) (type_transf c)
      Name_type_4 b -> Name_type_6 b
  type_typ :: String -> String -> Type_3 -> Dictionary Kind_0 -> Kind_0 -> Err Type_4
  type_typ file_name x (Type_3 b c) d e = type_eqs file_name 0 c d (kind_0_to_1 e) >>= \(_, h, i) -> i <$ solve_type_eqs x b h
  type_types :: String -> String -> [Type_3] -> Dictionary Kind_0 -> Err [Type_4]
  type_types file_name x a b =
    case a of
      [] -> Right []
      c : d -> type_typ file_name x c b Star_kind_0 >>= \e -> (:) e <$> type_types file_name x d b
  types :: Dictionary Type_5
  types =
    Data.Map.fromList
      (
        (
          second
            (\(Constructor_3 a b) ->
              let
                c = algebraics ! a
              in
                Basic_type_4
                  c
                  Nothing
                  []
                  (Prelude.foldr
                    function_type
                    (Prelude.foldl Application_type_4 (Name_type_4 a) ((\(Type_variable_1 x _) -> Name_type_4 x) <$> c))
                    b)) <$>
          constrs) ++
        [
          (
            "Add",
            Basic_type_4
              [Type_variable_1 "T" Star_kind_0]
              (Just (Constraint_1 "Ring" "T"))
              [Constraint_1 "Ring" "T"]
              (function_type (Name_type_4 "T") (function_type (Name_type_4 "T") (Name_type_4 "T")))),
          (
            "Compare",
            Basic_type_4
              [Type_variable_1 "T" Star_kind_0]
              (Just (Constraint_1 "Ord" "T"))
              [Constraint_1 "Ord" "T"]
              (function_type (Name_type_4 "T") (function_type (Name_type_4 "T") comparison_type))),
          (
            "Convert",
            Basic_type_4
              [Type_variable_1 "T" Star_kind_0]
              (Just (Constraint_1 "Ring" "T"))
              [Constraint_1 "Ring" "T"]
              (function_type int_type (Name_type_4 "T"))),
          ("Crash", Basic_type_4 [Type_variable_1 "T" Star_kind_0] Nothing [] (Name_type_4 "T")),
          ("Div", Basic_type_4 [] Nothing [] (function_type int_type (function_type int_type (maybe_type int_type)))),
          (
            "Inverse",
            Basic_type_4
              [Type_variable_1 "T" Star_kind_0]
              (Just (Constraint_1 "Field" "T"))
              [Constraint_1 "Field" "T", Constraint_1 "Ring" "T"]
              (function_type (Name_type_4 "T") (maybe_type (Name_type_4 "T")))),
          ("Mod", Basic_type_4 [] Nothing [] (function_type int_type (function_type int_type (maybe_type int_type)))),
          (
            "Times",
            Basic_type_4
              [Type_variable_1 "T" Star_kind_0]
              (Just (Constraint_1 "Ring" "T"))
              [Constraint_1 "Ring" "T"]
              (function_type (Name_type_4 "T") (function_type (Name_type_4 "T") (Name_type_4 "T"))))])
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
      File_4 ->
      (
        (
          Dictionary Kind_0,
          Dictionary [Type_variable_1],
          Dictionary Constructor_3,
          Dictionary Type_5,
          Dictionary Class_6,
          Dictionary Class_5,
          Dictionary (Dictionary [[String]]),
          Dictionary Kind_0),
        Dictionary Expr_2) ->
      Err
        (
          (
            Dictionary Kind_0,
            Dictionary [Type_variable_1],
            Dictionary Constructor_3,
            Dictionary Type_5,
            Dictionary Class_6,
            Dictionary Class_5,
            Dictionary (Dictionary [[String]]),
            Dictionary Kind_0),
          Dictionary Expr_2))
  typing file_name (File_4 a a' c) ((d, t, u, v, b', c5, x, t2), l) =
    (
      type_datas file_name a (old d, old t, old u, old v, l) >>=
      \(e, b, h, g, f) ->
        (
          type_classes file_name (fst <$> e) a' (old b', g, old t2, old c5) >>=
          \(c', g0, x2, t3) ->
            (
              (\(i, j, y) ->
                ((rem_old e, rem_old b, rem_old h, rem_old j, rem_old c', rem_old t3, rem_old' y, rem_old x2), i)) <$>
              type_defs file_name c (fst <$> e, fst <$> b, fst <$> h) (f, g0) (fst <$> c') (fst <$> t3) (old' x))))
--------------------------------------------------------------------------------------------------------------------------------