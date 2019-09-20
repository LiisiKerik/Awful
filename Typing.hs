{-
tests
write long types and kinds in error messages?
"requires instance or constraint" -> "requires instance" / "requires constraint"
internal: make the system of specifying built-in algebraic data types and things better and safer
generalise Branching data type to branch from any of the type variables?
    Branching Array[! : Nat, T : Star]{Zero -> ..., Next N -> ...}
remove special semantics of missing pattern match arguments?
Allow hiding things to functions outside module - so that helper functions are not exported from the module?
move modular checking to parser?
make syntax case more general (full expression, not just variable, as argument)
allow to make algebraic data types and branching data types hidden?
expand restricted constructor thing to algebraic and branching data types and definitions (and class methods?)
safe, restricted, statically checked division?
check for duplicate constraints everywhere
take some things out of imported-only contexts and put them into global program context
  (they can result in unfound things and should be included everywhere transitively)
change hidden/private parsing from keyword Hidden to some symbol/operator before the token?
Conflicting definitions of Entry at Data.awf:4:1 and at Data.awf:6:1. - remove second at from error message
undefined operator -> undefined type operator, for error in type operators
fix Write_Brackets implementations for all types
allow blanks instead of type variables
special function for writing ![...]
algebraic pattern matching - check in Typing module that all constructors are actually legit
is circular inheritance a problem when we don't have type erasure and hidden arguments? if not, remove checks
check if things are put correctly into local / global context
move operator name checking into naming module
allow incomplete patternmatches?
add Modular arithmetic methods to Nonzero class; this also prevents the user for extending Nonzero to Zero!
disallow mixing different associativities with same precedence when parsing
div - give error not only when divisor is zero but also when divisor is negative!
remove $ from operator chars
süntaktilise suhkru korral ja operaatorite korral kontrollida, kas seal sees esinevad funktsiooninimed on skoobis
-}
--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Typing where
  import Control.Monad
  import Data.Bifunctor
  import Data.List
  import Data.Map
  import Data.Maybe
  import Data.Set
  import Naming
  import Standard
  import Tokenise
  import Tree
  data Alg = Alg [(String, Kind_0)] Type_1 [String] deriving Show
  data Alg' = Alg' [(String, Kind_0)] Type_1 [(String, [Type_1])] deriving Show
  data Alg_pat_2 =
    Application_alg_pat_2 String [Alg_pat_2] |
    Blank_alg_pat_2 |
    Char_alg_pat_2 Char |
    Int_alg_pat_2 Integer |
    Modular_alg_pat_2 Integer |
    Name_alg_pat_2 String
      deriving Show
  data Alg_pat_3 =
    Blank_alg_pat_3 |
    Char_alg_pat_3 Char |
    Int_alg_pat_3 Integer |
    Modular_alg_pat_3 Modular |
    Struct_alg_pat_3 String [Alg_pat_3]
      deriving Show
  type Algebraics = Map' (Alg, Status)
  data Case_3 = Case_3 Alg_pat_2 Expression_2 deriving Show
  data Case_m = Case_m Alg_pat_2 Expression_4 deriving Show
  data Chck = Chck Name ([String], Expression_2) [Expression_4] deriving Show
  data Chck' = Chck' Name ([String], Expression_2) [Expression_2] deriving Show
  data Class_3 = Class_3 String (String, Kind_0) [Name] [Method_3] deriving Show
  data Class_4 = Class_4 (String, Kind_0) [String] [Method_4] deriving Show
  data Class_5 = Class_5 Kind_0 [String] deriving Show
  data Cond_eqtns = Cond_eqtns String Eqtns String Eqtns deriving Show
  data Constraint_1 = Constraint_1 String String deriving Show
  data Constructor = Constructor String [Type_1] deriving Show
  data Def_4 =
    Basic_def_4 Location_0 String Kinds_constraints_2 Type_1 Expression_1 |
    Instance_4
      Location_0
      String
      [String]
      String
      String
      [(String, Kind_0)]
      Integer
      [(Name, Expression_1, [(String, Kind_0)], [Constraint_1], Type_1)]
      Type_1
      [Constraint_1]
      [[String]]
        deriving Show
  data Eqtns = Eqtns [Integer] [(Type_6, Type_6)] [(String, (Name_3, Type_6))] [Cond_eqtns] deriving Show
  data Expr_2 = Expr_2 (Maybe [String]) [String] Expression_2 deriving Show
  data Expression_2 =
    Add_Int_0_expression_2 |
    Add_Int_1_expression_2 Integer |
    Add_Modular_0_expression_2 Integer |
    Add_Modular_1_expression_2 Integer Integer |
    Algebraic_expression_2 String [Expression_2] |
    Application_expression_2 Expression_2 Expression_2 |
    Branch_expression_2 Type_1 Expression_2 Pattern_0 Expression_2 |
    Char_expression_2 Char |
    Compare_Char_0_expression_2 |
    Compare_Char_1_expression_2 Char |
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
    Function_expression_2 Pat_1 Expression_2 |
    Glob_expression_2 String (Maybe Type_1) [Type_1] |
    Int_expression_2 Integer |
    Inverse_Modular_expression_2 Integer |
    Loc_expression_2 String |
    Match_expression_2 Expression_2 [Case_3] |
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
  data Expression_4 =
    Application_expression_4 Expression_4 Expression_4 |
    Branch_expression_4 String Expression_4 Pattern_0 Expression_4 |
    Char_expression_4 Char |
    Function_expression_4 Pat_1 Expression_4 |
    Glob_expression_4 String (Maybe Type_6) [Type_6] |
    Int_expression_4 Integer |
    Loc_expression_4 String |
    Match_expression_4 Expression_4 [Case_m] |
    Modular_expression_4 Integer
      deriving Show
  data File =
    File
      (Map' Kind_0)
      (Map' Alg)
      (Map' Constructor)
      (Map' Type_2)
      (Map' Class_4)
      (Map' Class_5)
      (Map' (Map' [[String]]))
      (Map' Kind_0)
        deriving Show
  data Form_2 = Form_2 String [Type_1] deriving Show
  data Kind_1 = Arrow_kind_1 Kind_1 Kind_1 | Nat_kind_1 | Star_kind_1 | Var_kind_1 Integer deriving (Eq, Show)
  data Kinds_constraints_2 = Kinds_constraints_2 [(String, Kind_0)] [Constraint_1] deriving Show
  data Method_3 = Method_3 String Kinds_constraints' Type_1 deriving Show
  data Method_4 = Method_4 String Kinds_constraints_2 Type_1 deriving Show
  -- data Normaliser_0 = Normaliser_0 String Kinds_constraints' [(String, Type_1)] Location_0 Expression_1 deriving Show
  data Pattern_5 =
    Blank_pattern_5 |
    Char_blank_pattern_5 (Set Char) |
    Char_pattern_5 Char |
    Int_blank_pattern_5 (Set Integer) |
    Int_pattern_5 Integer |
    Modular_pattern_5 Integer |
    Struct_pattern_5 String [Pattern_5]
      deriving Show
  data Name_3 = Writeable | Realname Name deriving Show
  data Nat = Nxt Nat | Zr deriving (Eq, Ord, Show)
  data Pat_1 = Application_pat_1 [Pat_1] | Blank_pat_1 | Name_pat_1 String deriving Show
  data Type_1 = Application_type_1 Type_1 Type_1 | Name_type_1 String deriving Show
  data Type_2 = Basic_type_1 [(String, Kind_0)] (Maybe Constraint_1) [Constraint_1] Type_1 (Maybe ([String], Expression_2))
    deriving Show
  data Type_6 = Application_type_6 Type_6 Type_6 | Name_type_6 String | Var_type_6 Integer deriving Show
  type Types = Map' (Type_2, Status)
  algebraics :: Map' Alg
  algebraics = (\(Alg' a b c) -> Alg a b (fst <$> c)) <$> Data.Map.fromList algebraics'
  algebraics' :: [(String, Alg')]
  algebraics' =
    [
      ("Associativity", Alg' [] associativity_type [("Lft", []), ("Rght", [])]),
      (
        "List",
        Alg'
          [("T", Star_kind_0)]
          (list_type (Name_type_1 "T"))
          [("Empty_List", []), ("Construct_List", [Name_type_1 "T", list_type (Name_type_1 "T")])]),
      ("Maybe", Alg' [("T", Star_kind_0)] (maybe_type (Name_type_1 "T")) [("Nothing", []), ("Wrap", [Name_type_1 "T"])]),
      ("Op", Alg' [] op_type [("Op", [list_type char_type, int_type, associativity_type])]),
      ("Ordering", Alg' [] comparison_type [("LT", []), ("EQ", []), ("GT", [])])]
  associativity_type :: Type_1
  associativity_type = Name_type_1 "Associativity"
  chain_constraints :: [String] -> Map' Class_5 -> Map' (Set String) -> String -> Map' (Set String)
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
  char_type :: Type_1
  char_type = Name_type_1 "Char"
  char_type' :: Type_6
  char_type' = Name_type_6 "Char"
  check_conditions ::
    (
      Map' Class_4 ->
      Map' (Map' [[String]]) ->
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
                  case constr_check ((\(Class_4 _ q0 _) -> q0) <$> u0) e0 u r' of
                    Just r0 -> s' ("it requires " ++ r0 ++ " due to constraints on " ++ q ++ " " ++ e)
                    Nothing -> check_conditions u0 m e s' e' e0 r' w'
                Nothing -> s
            Nothing -> s
  chcks :: ([Chck], Maybe Chck) -> [Chck]
  chcks (a, b) = a ++ maybeToList b
  classes_0 :: Map' Class_4
  classes_0 =
    Data.Map.fromList
      [
        (
          "Field",
          Class_4
            ("T", Star_kind_0)
            ["Ring"]
            [Method_4 "Inverse" (Kinds_constraints_2 [] []) (function_type (Name_type_1 "T") (maybe_type (Name_type_1 "T")))]),
        ("Nonzero", Class_4 ("N", Nat_kind_0) [] []),
        (
          "Ord",
          Class_4
            ("T", Star_kind_0)
            []
            [
              Method_4
                "Compare"
                (Kinds_constraints_2 [] [])
                (function_type (Name_type_1 "T") (function_type (Name_type_1 "T") comparison_type))]),
        (
          "Ring",
          Class_4
            ("T", Star_kind_0)
            []
            [
              Method_4
                "Add"
                (Kinds_constraints_2 [] [])
                (function_type (Name_type_1 "T") (function_type (Name_type_1 "T") (Name_type_1 "T"))),
              Method_4 "Convert" (Kinds_constraints_2 [] []) (function_type int_type (Name_type_1 "T")),
              Method_4
                "Times"
                (Kinds_constraints_2 [] [])
                (function_type (Name_type_1 "T") (function_type (Name_type_1 "T") (Name_type_1 "T")))]),
        (
          "Writeable",
          Class_4
            ("T", Star_kind_0)
            []
            [Method_4 "Write_brackets" (Kinds_constraints_2 [] []) (function_type (Name_type_1 "T") op_type)])]
  classes_1 :: Map' Class_5
  classes_1 = (\(Class_4 (_, a) b _) -> Class_5 a b) <$> classes_0
  classes_2 :: Map' Kind_0
  classes_2 = (\(Class_4 (_, a) _ _) -> a) <$> classes_0
  comparison_type :: Type_1
  comparison_type = Name_type_1 "Ordering"
  constr_check :: Map' [String] -> [String] -> [[String]] -> [[String]] -> Maybe String
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
  constr_check' :: Map' [String] -> String -> [String] -> [String] -> Maybe String
  constr_check' n t x y =
    case x of 
      [] -> Nothing
      a : x' ->
        case constr_check'' n t a y of
          Left m -> Just m
          Right y' -> constr_check' n t x' y'
  constr_check'' :: Map' [String] -> String -> String -> [String] -> Either String [String]
  constr_check'' m t c x =
    case x of
      [] -> Left (c ++ " " ++ t)
      a : x' -> if constr_check_3 m c [a] then Right x' else (:) a <$> constr_check'' m t c x'
  constr_check_3 :: Map' [String] -> String -> [String] -> Bool
  constr_check_3 m x y =
    case y of
      [] -> True
      a : b ->
        case x == a of
          False -> constr_check_3 m x (m ! a ++ b)
          True -> constr_check_3 m x b
  constrs :: [(String, Constructor)]
  constrs = join ((\(a, Alg' _ _ b) -> (\(c, d) -> (c, Constructor a d)) <$> b) <$> algebraics')
  context_union ::
    (
      (File, Map' ([String], Syntax_type'), (Map' Op, Map' Op)) ->
      (File, Map' ([String], Syntax_type'), (Map' Op, Map' Op)) ->
      (File, Map' ([String], Syntax_type'), (Map' Op, Map' Op)))
  context_union (File b i j d e q t g, x0, (a7, t0)) (File f k l h m r u n, x1, (a8, t2)) =
    (
      File
        (Data.Map.union b f)
        (Data.Map.union i k)
        (Data.Map.union j l)
        (Data.Map.union d h)
        (Data.Map.union e m)
        (Data.Map.union q r)
        (unionWith Data.Map.union t u)
        (Data.Map.union g n),
      Data.Map.union x0 x1,
      (Data.Map.union a7 a8, Data.Map.union t0 t2))
  defs :: Map' Expr_2
  defs =
    Data.Map.fromList
      (
        join
          (
            (\(_, Alg' a _ c) -> (\(d, _) -> (d, Expr_2 Nothing (fst <$> a) (Algebraic_expression_2 d []))) <$> c) <$>
            algebraics') ++
        [
          ("Add Int", Expr_2 (Just []) [] Add_Int_0_expression_2),
          ("Compare Char", Expr_2 (Just []) [] Compare_Char_0_expression_2),
          ("Compare Int", Expr_2 (Just []) [] Compare_Int_0_expression_2),
          ("Convert Int", Expr_2 (Just []) [] Convert_Int_expression_2),
          ("Div", Expr_2 Nothing [] Div_0_expression_2),
          ("Mod", Expr_2 Nothing [] Mod_0_expression_2),
          ("Times Int", Expr_2 (Just []) [] Multiply_Int_0_expression_2),
          ("Write_brackets Int", Expr_2 (Just []) [] Write_Brackets_Int_expression_2)])
  exprrepl :: Map Integer Type_1 -> Expression_4 -> Expression_2
  exprrepl a b =
    case b of
      Application_expression_4 c d -> Application_expression_2 (exprrepl a c) (exprrepl a d)
      Branch_expression_4 c d e f -> Branch_expression_2 (Name_type_1 c) (exprrepl a d) e (exprrepl a f)
      Char_expression_4 c -> Char_expression_2 c
      Function_expression_4 c d -> Function_expression_2 c (exprrepl a d)
      Glob_expression_4 c d e -> Glob_expression_2 c (exprrepl' a <$> d) (exprrepl' a <$> e)
      Int_expression_4 c -> Int_expression_2 c
      Loc_expression_4 c -> Loc_expression_2 c
      Match_expression_4 c d -> Match_expression_2 (exprrepl a c) ((\(Case_m e f) -> Case_3 e (exprrepl a f)) <$> d)
      Modular_expression_4 c -> Modular_expression_2 c
  exprrepl' :: Map Integer Type_1 -> Type_6 -> Type_1
  exprrepl' a b =
    case b of
      Application_type_6 c d -> Application_type_1 (exprrepl' a c) (exprrepl' a d)
      Name_type_6 c -> Name_type_1 c
      Var_type_6 c -> a ! c
  find_and_delete :: Ord t => Map t u -> t -> Maybe (u, Map t u)
  find_and_delete a b = (\c -> (c, Data.Map.delete b a)) <$> Data.Map.lookup b a
  function_type :: Type_1 -> Type_1 -> Type_1
  function_type a = Application_type_1 (Application_type_1 (Name_type_1 "Arrow") a)
  function_type' :: Type_6 -> Type_6 -> Type_6
  function_type' a = Application_type_6 (Application_type_6 (Name_type_6 "Arrow") a)
  get_pattern_type ::
    (
      Map' Alg ->
      Map' Constructor ->
      (Integer, Map' (Either Type_2 Type_6)) ->
      Alg_pat_1 ->
      Type_6 ->
      Err ((Integer, [Integer], [(Type_6, Type_6)], Map' (Either Type_2 Type_6)), Alg_pat_2))
  get_pattern_type b c (d, n) g h =
    case g of
      Application_alg_pat_1 o i j ->
        und_err
          i
          c
          "constructor"
          o
          (\(Constructor p y) ->
            let
              Alg k m _ = b ! p
              ((q, r), (s, _)) = typevars k (d, Data.Map.empty)
            in
              (
                bimap (\(t, x, y', z) -> (t, s ++ x, (h, repl_transf r m) : y', z)) (Application_alg_pat_2 i) <$>
                get_pattern_types b c (q, n) j (repl_transf r <$> y) (Name o i)))
      Blank_alg_pat_1 -> Right ((d, [], [], n), Blank_alg_pat_2)
      Char_alg_pat_1 i -> Right ((d, [], [(h, char_type')], n), Char_alg_pat_2 i)
      Int_alg_pat_1 i -> Right ((d, [], [(h, int_type')], n), Int_alg_pat_2 i)
      Modular_alg_pat_1 (Modular j z) -> Right ((d, [], [(h, mod_type' (int_to_nat' j))], n), Modular_alg_pat_2 z)
      Name_alg_pat_1 i -> Right ((d, [], [], Data.Map.insert i (Right h) n), Name_alg_pat_2 i)
  get_pattern_types ::
    (
      Map' Alg ->
      Map' Constructor ->
      (Integer, Map' (Either Type_2 Type_6)) ->
      [Alg_pat_1] ->
      [Type_6] ->
      Name ->
      Err ((Integer, [Integer], [(Type_6, Type_6)], Map' (Either Type_2 Type_6)), [Alg_pat_2]))
  get_pattern_types b c (d0, d3) e f (Name m n) =
    let
      t = show <$> [0 .. length f - 1]
    in
      case e of
        [] ->
          case t of
            [] -> Right ((d0, [], [], Prelude.foldl (\u -> \(m', v) -> Data.Map.insert m' (Right v) u) d3 (zip t f)), [])
            _ -> Left (Error ("Constructor " ++ n ++ " at ") (location m ++ " has been given too few arguments."))
        g : h ->
          case f of
            [] -> Left (Error ("Constructor " ++ n ++ " at ") (location m ++ " has been given too many arguments."))
            i : j ->
              (
                get_pattern_type b c (d0, d3) g i >>=
                \((k, k0, k1, k2), l) ->
                  (
                    bimap (\(m0, m1, m2, m3) -> (m0, k0 ++ m1, k1 ++ m2, m3)) ((:) l) <$>
                    get_pattern_types b c (k, k2) h j (Name m n)))
  init_type_context :: (File, Map' ([String], Syntax_type'), (Map' Op, Map' Op))
  init_type_context =
    (
      File kinds algebraics (Data.Map.fromList constrs) types classes_0 classes_1 instances classes_2,
      Data.Map.singleton "$undefined" (["$t"], Name_st' "$t"),
      (Data.Map.empty, Data.Map.empty))
  instances :: Map' (Map' [[String]])
  instances =
    Data.Map.fromList
      [
        ("Field", Data.Map.fromList [("Modular", [["Nonzero"]])]),
        ("Nonzero", Data.Map.fromList [("Next", [[]])]),
        ("Ord", Data.Map.fromList [("Char", []), ("Int", []), ("Modular", [[]])]),
        ("Ring", Data.Map.fromList [("Int", []), ("Modular", [["Nonzero"]])]),
        ("Writeable", Data.Map.fromList [("Int", []), ("Modular", [[]])])]
  int_to_nat' :: Integer -> Type_6
  int_to_nat' a =
    case a of
      0 -> Name_type_6 "Zero"
      _ -> Application_type_6 (Name_type_6 "Next") (int_to_nat' (a - 1))
  int_type :: Type_1
  int_type = Name_type_1 "Int"
  int_type' :: Type_6
  int_type' = Name_type_6 "Int"
  isLeft :: Either t u -> Bool
  isLeft a =
    case a of
      Left _ -> True
      Right _ -> False
  kind_0_to_1 :: Kind_0 -> Kind_1
  kind_0_to_1 a =
    case a of
      Arrow_kind_0 b c -> Arrow_kind_1 (kind_0_to_1 b) (kind_0_to_1 c)
      Nat_kind_0 -> Nat_kind_1
      Star_kind_0 -> Star_kind_1
  kind_err :: Location_0 -> Err t
  kind_err a = Left (Error ("Kind mismatch at ") (location' a))
  kind_mism_err :: Location_0 -> Kind_1 -> Kind_1 -> Err t
  kind_mism_err a b c =
    Left (Error ("Kind mismatch at ") (location a ++ " between " ++ show_kind b ++ " and " ++ show_kind c ++ "."))
  kinds :: Map' Kind_0
  kinds =
    Data.Map.fromList
      [
        ("Arrow", Arrow_kind_0 Star_kind_0 (Arrow_kind_0 Star_kind_0 Star_kind_0)),
        ("Associativity", Star_kind_0),
        ("Char", Star_kind_0),
        ("Int", Star_kind_0),
        ("List", Arrow_kind_0 Star_kind_0 Star_kind_0),
        ("Maybe", Arrow_kind_0 Star_kind_0 Star_kind_0),
        ("Modular", Arrow_kind_0 Nat_kind_0 Star_kind_0),
        ("Next", Arrow_kind_0 Nat_kind_0 Nat_kind_0),
        ("Op", Star_kind_0),
        ("Ordering", Star_kind_0),
        ("Zero", Nat_kind_0)]
  list_type :: Type_1 -> Type_1
  list_type = Application_type_1 (Name_type_1 "List")
  list_type' :: Type_6 -> Type_6
  list_type' = Application_type_6 (Name_type_6 "List")
  location_err' :: String -> Location_1 -> Location_0 -> Error
  location_err' a b = location_err a (Library b)
  locations_0 :: Locations
  locations_0 =
    Data.Map.fromList
      (
        (\x -> (x, Language)) <$>
        ["Arrow", "Associativity", "Char", "Int", "List", "Maybe", "Modular", "Next", "Op", "Ordering", "Zero"])
  locations_1 :: Locations
  locations_1 = Data.Map.fromList ((\x -> (x, Language)) <$> ["Field", "Nonzero", "Ord", "Ring", "Writeable"])
  locations_2 :: Locations
  locations_2 =
    Data.Map.fromList
      (
        (\x -> (x, Language)) <$>
        ((fst <$> constrs) ++ ["Add", "Compare", "Convert", "Crash", "Div", "Inverse", "Mod", "Times", "Write_brackets"]))
  maybe_type :: Type_1 -> Type_1
  maybe_type = Application_type_1 (Name_type_1 "Maybe")
  mod_type :: Type_1 -> Type_1
  mod_type = Application_type_1 (Name_type_1 "Modular")
  mod_type' :: Type_6 -> Type_6
  mod_type' = Application_type_6 (Name_type_6 "Modular")
  not_promoted :: String -> Bool
  not_promoted a =
    case a of
      '!' : _ -> False
      _ -> True
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
  old' :: Map' (Map' t) -> Map' (Map' (t, Status))
  old' = (<$>) old
  op_type :: Type_1
  op_type = Name_type_1 "Op"
  primitive_pattern_0 :: (t -> Pattern_5, Set t -> Pattern_5) -> t -> ([(Pattern_5, Bool)], Bool)
  primitive_pattern_0 (f, g) x = ([(f x, True), (g (Data.Set.singleton x), False)], True)
  primitive_pattern_1 :: Eq t => (t -> Pattern_5) -> t -> t -> ([(Pattern_5, Bool)], Bool)
  primitive_pattern_1 f x y =
    let
      z = y == x
    in
      ([(f x, z)], z)
  primitive_pattern_2 :: Ord t => (t -> Pattern_5, Set t -> Pattern_5) -> Set t -> t -> ([(Pattern_5, Bool)], Bool)
  primitive_pattern_2 (f, g) x y =
    case Data.Set.member y x of
      False -> ([(f y, True), (g (Data.Set.insert y x), False)], True)
      True -> ([(g x, False)], False)
  rem_old' :: Map' (Map' (t, Status)) -> Map' (Map' t)
  rem_old' a = Data.Map.filter (\b -> not (Data.Map.null b)) (rem_old <$> a)
  repl' :: Map Integer Type_6 -> Type_6 -> Type_6
  repl' a b =
    case b of
      Application_type_6 c d -> Application_type_6 (repl' a c) (repl' a d)
      Var_type_6 c ->
        case Data.Map.lookup c a of
          Just d -> d
          Nothing -> b
      _ -> b
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
  repl_transf :: Map' Type_6 -> Type_1 -> Type_6
  repl_transf a b =
    case b of
      Application_type_1 c d -> Application_type_6 (repl_transf a c) (repl_transf a d)
      Name_type_1 c ->
        case Data.Map.lookup c a of
          Nothing -> Name_type_6 c
          Just d -> d
  show_char :: Char -> String
  show_char c = show [c]
  show_kind :: Kind_1 -> String
  show_kind a = fst (show_kind' a)
  show_kind' :: Kind_1 -> (String, Bool)
  show_kind' a =
    case a of
      Arrow_kind_1 b c ->
        let
          (d, e) = show_kind' b
        in
          (
            (
              (case e of
                False -> d
                True -> "(" ++ d ++ ")") ++
              " -> " ++
              show_kind c),
            True)
      Nat_kind_1 -> ("Nat", False)
      Star_kind_1 -> ("Star", False)
      Var_kind_1 b -> (show b, False)
  slv :: Map' (Map' [[String]]) -> [(String, (Name_3, Type_1))] -> (Name_3 -> String -> String -> Err ()) -> Err ()
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
      Map' (Map' [[String]]) ->
      [(String, (Name_3, Type_1))] ->
      (Name_3 -> String -> String -> Err ()) ->
      [Type_1] ->
      [[String]] ->
      Name_3 ->
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
  solve_all :: Map' (Map' [[String]]) -> Maybe Name -> Eqtns -> Map Integer Type_6 -> Err (Map Integer Type_6)
  solve_all b c (Eqtns d e f g) h =
    (
      solvesys c e (f, Data.Set.fromList d, g, h) >>=
      \(i, k, l, j) ->
        case Data.Set.null k of
          False ->
            Left
              (case c of
                Nothing -> Error "Unresolved type variables in " "."
                Just (Name c0 c1) -> Error ("Unresolved type variables in " ++ c1 ++ " at ") (location' c0))
          True ->
            (
              slv
                b
                (second (second typeback) <$> i)
                (\u -> \o -> \p ->
                  Left
                    (case u of
                      Writeable ->
                        Error "Method Write invoked by Awful on " (" for printing the result requires instance or constraint " ++ o ++ " " ++ p ++ ".")
                      Realname (Name m n) ->
                        Error
                          (
                            "Method " ++
                            n ++
                            (case c of
                              Nothing -> ""
                              Just (Name _ c1) -> " in " ++ c1) ++
                            " at ")
                          (location m ++ " requires instance or constraint " ++ o ++ " " ++ p ++ "."))) >>
              solve_conds b c l j))
  solve_cond :: Map' (Map' [[String]]) -> Maybe Name -> Cond_eqtns -> Map Integer Type_6 -> Err (Map Integer Type_6)
  solve_cond b c (Cond_eqtns _ e _ g) h = solve_all b c e h >>= solve_all b c g
  solve_conds :: Map' (Map' [[String]]) -> Maybe Name -> [Cond_eqtns] -> Map Integer Type_6 -> Err (Map Integer Type_6)
  solve_conds b c d e =
    case d of
      [] -> Right e
      f : g -> solve_cond b c f e >>= solve_conds b c g
  solve_type_eqs :: Location_0 -> [(Kind_1, Kind_1)] -> Err ()
  solve_type_eqs a b =
    case b of
      [] -> Right ()
      d : e ->
        case d of
          (Arrow_kind_1 f g, Arrow_kind_1 h i) -> solve_type_eqs a ((f, h) : (g, i) : e)
          (Nat_kind_1, Nat_kind_1) -> solve_type_eqs a e
          (Star_kind_1, Star_kind_1) -> solve_type_eqs a e
          (Var_kind_1 f, Var_kind_1 g) -> solve_type_eqs a (repl_kind_eqs f (Var_kind_1 g) e)
          (Var_kind_1 f, g) -> solve_type_eqs' True a f g e
          (f, Var_kind_1 g) -> solve_type_eqs' False a g f e
          (f, g) -> kind_mism_err a f g
  solve_type_eqs' :: Bool -> Location_0 -> Integer -> Kind_1 -> [(Kind_1, Kind_1)] -> Err ()
  solve_type_eqs' e a b c d =
    case occ_kind b c of
      False -> solve_type_eqs a (repl_kind_eqs b c d)
      True ->
        case e of
          False -> kind_mism_err a c (Var_kind_1 b)
          True -> kind_mism_err a (Var_kind_1 b) c
  solvesys ::
    Maybe Name ->
    [(Type_6, Type_6)] ->
    ([(String, (Name_3, Type_6))], Set Integer, [Cond_eqtns], Map Integer Type_6) ->
    Err ([(String, (Name_3, Type_6))], Set Integer, [Cond_eqtns], Map Integer Type_6)
  solvesys m b (a', u, u1, t) =
    case b of
      [] -> Right (a', u, u1, t)
      c' : g ->
        case c' of
          (Application_type_6 e f, Application_type_6 h i) -> solvesys m ((e, h) : (f, i) : g) (a', u, u1, t)
          (Name_type_6 e, Name_type_6 f) ->
            case e == f of
              False -> type_mism_err m (Name_type_6 e) (Name_type_6 f)
              True -> solvesys m g (a', u, u1, t)
          (Var_type_6 e, Var_type_6 f) -> solvesys_rep m e (Var_type_6 f) g (a', u, u1, t)
          (Var_type_6 e, d) -> solvesys' m e d g (a', u, u1, t) True
          (c, Var_type_6 h) -> solvesys' m h c g (a', u, u1, t) False
          (c, d) -> type_mism_err m c d
  solvesys' ::
    (
      Maybe Name ->
      Integer ->
      Type_6 ->
      [(Type_6, Type_6)] ->
      ([(String, (Name_3, Type_6))], Set Integer, [Cond_eqtns], Map Integer Type_6) ->
      Bool ->
      Err ([(String, (Name_3, Type_6))], Set Integer, [Cond_eqtns], Map Integer Type_6))
  solvesys' h b c d (x, a, w, t) e =
    case (Data.Set.member b a, occtype b c) of
      (True, False) -> solvesys_rep h b c d (x, a, w, t)
      _ ->
        case e of
          False -> type_mism_err h c (Var_type_6 b)
          True -> type_mism_err h (Var_type_6 b) c
  solvesys_rep ::
    (
      Maybe Name ->
      Integer ->
      Type_6 ->
      [(Type_6, Type_6)] ->
      ([(String, (Name_3, Type_6))], Set Integer, [Cond_eqtns], Map Integer Type_6) ->
      Err ([(String, (Name_3, Type_6))], Set Integer, [Cond_eqtns], Map Integer Type_6))
  solvesys_rep a c d e (x, k, w, t) =
    let
      m = sysrep'' c d
    in
      solvesys
        a
        (bimap m m <$> e)
        (second (second m) <$> x, Data.Set.delete c k, sysrep_cond c d <$> w, Data.Map.insert c d (m <$> t))
  sysrep :: String -> Type_1 -> Type_1 -> Type_1
  sysrep a b c =
    case c of
      Application_type_1 d e -> Application_type_1 (sysrep a b d) (sysrep a b e)
      Name_type_1 d -> if d == a then b else c
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
  sysrep_cond :: Integer -> Type_6 -> Cond_eqtns -> Cond_eqtns
  sysrep_cond a b (Cond_eqtns c d e f) =
    let
      g h i = sysrep_eqs a c h b i
    in
      Cond_eqtns c (g (Name_type_6 "Zero") d) e (g (Application_type_6 (Name_type_6 "Next") (Name_type_6 e)) f)
  sysrep_eqs :: Integer -> String -> Type_6 -> Type_6 -> Eqtns -> Eqtns
  sysrep_eqs a b h i (Eqtns c d e f) =
    let
      g = sysrep' b h i
      j = sysrep'' a g
    in
      Eqtns c (bimap j j <$> d) (second (second j) <$> e) (sysrep_cond a g <$> f)
  typeback :: Type_6 -> Type_1
  typeback a =
    case a of
      Application_type_6 b c -> Application_type_1 (typeback b) (typeback c)
      Name_type_6 b -> Name_type_1 b
      Var_type_6 _ -> undefined
  type_br ::
    (
      String ->
      [(String, Kind_0)] ->
      Data_br_2 ->
      Map' Kind_0 ->
      String ->
      (Algebraics, Types, Map' (Constructor, Status), Map' Expr_2) ->
      Err (Algebraics, Types, Map' (Constructor, Status), Map' Expr_2))
  type_br b c d e r (f, o, p, q) =
    let
      g = Prelude.foldl (\h -> \(i, n) -> Data.Map.insert i n h) e c
      s = Prelude.foldl Application_type_1 (Name_type_1 b) (Name_type_1 <$> fst <$> c)
    in
      case d of
        Algebraic_data_2 j ->
          (
            (\(h, (i, k, l)) -> (ins_new r (Alg c s h) f, i, k, l)) <$>
            type_forms c j g (Expr_2 Nothing (fst <$> c)) r s (o, p, q))
        Branching_data_2 (Name h j) (k, l, m) ->
          let
            i = Data.Map.delete j e
          in
            (
              type_br_help (Name h j) c >>=
              \(n, t) ->
                (
                  type_br b n k i (r ++ " Zero") (f, o, p, q) >>=
                  type_br b t m (Data.Map.insert l Nat_kind_0 i) (r ++ " Next")))
        Struct_data_2 j k l ->
          (
            (\(h, (m, u)) ->
              (
                ins_new r (Alg c s [k]) f,
                Data.Map.insert k (Basic_type_1 c Nothing [] (Prelude.foldr function_type s h) Nothing, j) m,
                ins_new k (Constructor r h) p,
                Data.Map.insert k (Expr_2 Nothing (fst <$> c) (Algebraic_expression_2 k [])) u)) <$>
            type_fields c l g (Expr_2 Nothing (fst <$> c)) s 0 (o, q))
  type_br_help :: Name -> [(String, Kind_0)] -> Err ([(String, Kind_0)], [(String, Kind_0)])
  type_br_help (Name a b) c =
    case c of
      [] -> Left (Error ("Undefined type variable " ++ b ++ " at ") (location' a))
      (d, e) : f ->
        case b == d of
          False -> bimap ((:) (d, e)) ((:) (d, e)) <$> type_br_help (Name a b) f
          True ->
            case e of
              Nat_kind_0 -> Right (f, (b, Nat_kind_0) : f)
              _ -> Left (Error ("Type variable " ++ b ++ " at ") (location a ++ " is not of kind Nat."))
  type_case ::
    (
      Map' Alg ->
      Map' Constructor ->
      Integer ->
      Map' (Either Type_2 Type_6) ->
      Case_2 ->
      Integer ->
      Type_6 ->
      Map' Kind_0 ->
      Err (Case_m, Eqtns, Integer, [Chck]))
  type_case a b d f (Case_2 g h) i j k =
    (
      get_pattern_type a b (d, f) g (Var_type_6 i) >>=
      \((m, n, s, t), o) ->
        (
          (\(u, Eqtns e p q r, w, r7) -> (Case_m o u, Eqtns (n ++ e) (s ++ p) q r, w, chcks r7)) <$>
          type_expression a b m t h j k))
  type_cases ::
    (
      Map' Alg ->
      Map' Constructor ->
      Integer ->
      Map' (Either Type_2 Type_6) ->
      [Case_2] ->
      Integer ->
      Type_6 ->
      Map' Kind_0 ->
      Err ([Case_m], Eqtns, Integer, [Chck]))
  type_cases a b d f g n h i =
    case g of
      [] -> Right ([], Eqtns [] [] [] [], d, [])
      l : m ->
        (
          type_case a b d f l n h i >>=
          \(o, Eqtns p0 p1 p2 p3, q, x2) ->
            (
              (\(r, Eqtns s0 s1 s2 s3, t, y2) -> (o : r, Eqtns (p0 ++ s0) (p1 ++ s1) (p2 ++ s2) (p3 ++ s3), t, x2 ++ y2)) <$>
              type_cases a b q f m n h i))
  type_class_0 ::
    (
      Map' Kind_0 ->
      Class_2 ->
      (Map' (Kind_0, Status), Map' (Class_5, Status), Map' String) ->
      Err (Class_3, (Map' (Kind_0, Status), Map' (Class_5, Status), Map' String)))
  type_class_0 j (Class_2 b (c, d) g' e) (i', j0, x2) =
    let
      g3 = (\(Constraint_0 (Name _ t4) _) -> t4) <$> g'
    in
      (
        (\u5 -> \g ->
          (
            Class_3 b (c, d) u5 g,
            (
              ins_new b d i',
              ins_new b (Class_5 d g3) j0,
              Prelude.foldl (\v -> \(Name _ t0) -> Data.Map.insert b t0 v) x2 u5))) <$
        type_inh b [b] g3 x2 <*>
        traverse
          (\(Constraint_0 (Name t2 t0) (Name x3 x4)) ->
            case x4 == c of
              False -> Left (Error ("Undefined type variable " ++ x4 ++ " at ") (location' x3))
              True -> Right (Name t2 t0))
          g' <*>
        type_methods_0 e (Data.Map.insert c d j))
  type_class_1 ::
    Class_3 ->
    Map' Kind_0 ->
    Map' Class_5 ->
    (Map' (Type_2, Status), Map' (Class_4, Status)) ->
    Err (Map' (Type_2, Status), Map' (Class_4, Status))
  type_class_1 (Class_3 b (c, k) g' e) d f1 (f0, f) =
    let
      x1 = Constraint_1 b c
    in
      (
        (\m -> \e' ->
          (
            Prelude.foldl
              (\x -> \(Method_4 t (Kinds_constraints_2 s u0) u) ->
                ins_new t (Basic_type_1 ((c, k) : s) (Just x1) (x1 : u0) u Nothing) x)
              f0
              e',
            ins_new b (Class_4 (c, k) m e') f)) <$>
        traverse (\(Name m g) -> und_err g d "class" m (\h -> if h == k then Right g else kind_err m)) g' <*>
        type_methods_1 f1 e)
  type_class_args ::
    Kind_0 ->
    [Pattern_0] ->
    Err ([(String, Kind_0)], Integer, Type_1, Map' Kind_0, Map' Nat) ->
    Kind_0 ->
    Integer ->
    Type_1 ->
    Map' Kind_0 ->
    Map' Nat ->
    Nat ->
    Err ([(String, Kind_0)], Integer, Type_1, Map' Kind_0, Map' Nat)
-- TODO: give an error with exact information about kind mismatch
  type_class_args a b e g c x c0 c' n =
    case b of
      [] -> if a == g then Right ([], c, x, c0, c') else e
      h : d ->
        case a of
          Arrow_kind_0 l f ->
            let
              i j k =
                (
                  (\(t, u, v, t', t2) -> ((j, l) : t, u, v, t', t2)) <$>
                  type_class_args
                    f
                    d
                    e
                    g
                    k
                    (Application_type_1 x (Name_type_1 j))
                    (Data.Map.insert j l c0)
                    (Data.Map.insert j n c')
                    (Nxt n))
            in
              case h of
                Blank_pattern -> i (show c) (c + 1)
                Name_pattern j -> i j c
          _ -> e
  type_classes ::
    (
      Map' Kind_0 ->
      [Class_2] ->
      (Map' (Class_4, Status), Map' (Type_2, Status), Map' (Kind_0, Status), Map' (Class_5, Status)) ->
      Err (Map' (Class_4, Status), Map' (Type_2, Status), Map' (Kind_0, Status), Map' (Class_5, Status)))
  type_classes c d (e, g, o, o') =
    (
      type_classes_0 c d (o, o', Data.Map.empty) >>=
      \(r, (p, p', _)) -> (\(k, n) -> (n, k, p, p')) <$> type_classes_1 r (fst <$> p) (fst <$> p') (g, e))
  type_classes_0 ::
    (
      Map' Kind_0 ->
      [Class_2] ->
      (Map' (Kind_0, Status), Map' (Class_5, Status), Map' String) ->
      Err ([Class_3], (Map' (Kind_0, Status), Map' (Class_5, Status), Map' String)))
  type_classes_0 g b c =
    case b of
      [] -> Right ([], c)
      d : e -> type_class_0 g d c >>= \(h, i) -> first ((:) h) <$> type_classes_0 g e i
  type_classes_1 ::
    (
      [Class_3] ->
      Map' Kind_0 ->
      Map' Class_5 ->
      (Map' (Type_2, Status), Map' (Class_4, Status)) ->
      Err (Map' (Type_2, Status), Map' (Class_4, Status)))
  type_classes_1 b h i c =
    case b of
      [] -> Right c
      d : e -> type_class_1 d h i c >>= type_classes_1 e h i
  type_cls_0 ::
    (
      String ->
      [Method_4] ->
      Type_1 ->
      [(Name, Expression_1)] ->
      String ->
      Location_0 ->
      Err [(Name, Expression_1, [(String, Kind_0)], [Constraint_1], Type_1)])
  type_cls_0 a b c d m n =
    case d of
      [] ->
        case b of
          [] -> Right []
          Method_4 e _ _ : _ ->
            Left (Error ("Missing definition " ++ e ++ " in instance " ++ m ++ " " ++ a ++ " at ") (location' n))
      (p' @ (Name h i), j) : k ->
        let
          o p = Left (Error ("Definition " ++ i ++ " at ") (location h ++ " is not a component of class " ++ m ++ p))
        in
          case b of
            [] -> o "."
            Method_4 e (Kinds_constraints_2 s y) f : g ->
              case i == e of
                False -> o " or the definitions are in a wrong order."
                True -> (:) (p', j, s, y, f) <$> type_cls_0 a g c k m n
  type_constraint_0 :: Map' (Set String) -> Constraint_0 -> (Map' Class_5, Map' Kind_0) -> Err (Map' (Set String))
  type_constraint_0 k (Constraint_0 (Name b c) (Name d e)) (f, g) =
    und_err
      c
      f
      "class"
      b
      (\(Class_5 h y) ->
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
                Left
                  (Error
                    "Kind mismatch in constraint at "
                    (location b ++ " between class " ++ c ++ " and type variable " ++ e ++ "."))
          Nothing -> Left (Error ("Undefined type variable " ++ e ++ " at ") (location' d)))
  type_constraint_1 :: Constraint_1 -> Map' (Map' [[String]]) -> Map' Class_4 -> Map' (Map' [[String]])
  type_constraint_1 (Constraint_1 c e) a b =
    let
      Class_4 _ f _ = b ! c
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
  type_constraints_0 :: Map' (Set String) -> [Constraint_0] -> (Map' Class_5, Map' Kind_0, Map' Nat) -> Err [Constraint_1]
  type_constraints_0 g a (f, t, u) =
    case a of
      [] -> Right (join ((\(i, j) -> Constraint_1 i <$> j) <$> assocs (Data.Set.elems <$> g)))
      b : c -> type_constraint_0 g b (f, t) >>= \d -> type_constraints_0 d c (f, t, u)
  type_constraints_1 :: [Constraint_1] -> Map' (Map' [[String]]) -> Map' Class_4 -> Map' (Map' [[String]])
  type_constraints_1 a e d =
    case a of
      [] -> e
      b : c -> type_constraints_1 c (type_constraint_1 b e d) d
  type_data_1 :: Data_2 -> Map' (Kind_0, Status) -> Map' (Kind_0, Status)
  type_data_1 (Data_2 a b _) = ins_new a (Prelude.foldr Arrow_kind_0 Star_kind_0 (snd <$> b))
  type_data_2 ::
    (
      Data_2 ->
      Map' Kind_0 ->
      (Algebraics, Types, Map' (Constructor, Status), Map' Expr_2) ->
      Err (Algebraics, Types, Map' (Constructor, Status), Map' Expr_2))
  type_data_2 (Data_2 b c d) e = type_br b c d e b
  type_datas ::
    (
      [Data_2] ->
      (Map' (Kind_0, Status), Algebraics, Map' (Constructor, Status), Types, Map' Expr_2) ->
      Err (Map' (Kind_0, Status), Algebraics, Map' (Constructor, Status), Types, Map' Expr_2))
  type_datas a (b, i, j, d, c) =
    let
      u = type_datas_1 a b
    in
      (\(b', c', v, w) -> (u, b', v, c', w)) <$> type_datas_2 a (fst <$> u) (i, d, j, c)
  type_datas_1 :: [Data_2] -> Map' (Kind_0, Status) -> Map' (Kind_0, Status)
  type_datas_1 = flip (Prelude.foldl (flip type_data_1))
  type_datas_2 ::
    (
      [Data_2] ->
      Map' Kind_0 ->
      (Algebraics, Types, Map' (Constructor, Status), Map' Expr_2) ->
      Err (Algebraics, Types, Map' (Constructor, Status), Map' Expr_2))
  type_datas_2 a b c =
    case a of
      [] -> Right c
      d : e -> type_data_2 d b c >>= type_datas_2 e b
  type_def_1 ::
    (
      Def_3 ->
      Map' Kind_0 ->
      Map' (Type_2, Status) ->
      Map' Class_4 ->
      Map' Class_5 ->
      Map' (Map' ([[String]], Status)) ->
      Err (Def_4, Map' (Type_2, Status), Map' (Map' ([[String]], Status))))
  type_def_1 a b c k k2 t' =
    case a of
      Basic_def_3 f d (Kinds_constraints' e e') g i ->
        let
          j' = type_kinds e Data.Map.empty
        in
          (
            type_constraints_0 Data.Map.empty e' (k2, j', (\_ -> Zr) <$> j') >>=
            \o1 ->
              (
                (\h ->
                  (Basic_def_4 f d (Kinds_constraints_2 e o1) h i, ins_new d (Basic_type_1 e Nothing o1 h Nothing) c, t')) <$>
                type_typ g (type_kinds e b) Star_kind_0))
      Instance_3 d (Name e m) (Name f n, k') o' g ->
        und_err
          m
          k
          "class"
          e
          (\(Class_4 (o, p) w0 q) ->
            und_err
              n
              b
              "type"
              f
              (\s ->
                (
                  type_class_args s k' (kind_err f) p 0 (Name_type_1 n) Data.Map.empty Data.Map.empty Zr >>=
                  \(q', p', s', t0, t7) ->
                    (
                      type_constraints_0 Data.Map.empty o' (k2, t0, t7) >>=
                      \o1 ->
                        let
                          r' =
                            (
                              (\(x', _) ->
                                (\(Constraint_1 y' _) -> y') <$> Data.List.filter (\(Constraint_1 _ y') -> y' == x') o1) <$>
                              q')
                        in
                          (
                            (\w ->
                              (
                                Instance_4 d m w0 o n q' p' w s' o1 r',
                                c,
                                (case Data.Map.lookup m t' of
                                  Just _ -> adjust (ins_new n r') m
                                  Nothing -> Data.Map.insert m (Data.Map.singleton n (r', New))) t')) <$>
                            type_cls_0 n q s' g m d)))))
  type_def_2 ::
    (
      Def_4 ->
      (Map' Alg, Map' Constructor, Map' Type_2) ->
      Map' Expr_2 ->
      Map' (Map' [[String]]) ->
      Map' Kind_0 ->
      Map' Class_4 ->
      Err (Map' Expr_2, [Chck']))
  type_def_2 a (d, l, k) c m n u0 =
    case a of
      Basic_def_4 r e (Kinds_constraints_2 b x) h i ->
        (
          first (\t -> Data.Map.insert e (Expr_2 Nothing (fst <$> b) t) c) <$>
          type_expr
            (Just (Name r e))
            h
            (d, l, k)
            i
            (type_constraints_1 x m u0)
            0
            (Prelude.foldl (\y -> \(z, w) -> Data.Map.insert z w y) n b))
      Instance_4 l' e' w0 w e e0 e1 f f' g' r' ->
        (
          check_conditions
            u0
            m
            e
            (\w1 -> Left (Error (e' ++ " " ++ e ++ " at ") (location l' ++ " is an illegal instance as " ++ w1 ++ ".")))
            e'
            (fst <$> e0)
            r'
            w0 *>
          type_exprs
            e
            (d, l, k)
            (type_constraints_1 g' m u0)
            f
            c
            e
            (sysrep w f')
            e1
            u0
            (Prelude.foldl (\x -> \(y, g) -> Data.Map.insert y g x) n e0)
            (fst <$> e0))
  type_defs ::
    (
      [Def_3] ->
      (Map' Kind_0, Map' Alg, Map' Constructor) ->
      (Map' Expr_2, Types) ->
      Map' Class_4 ->
      Map' Class_5 ->
      Map' (Map' ([[String]], Status)) ->
      Err (Map' Expr_2, Types, Map' (Map' ([[String]], Status)), [Chck']))
  type_defs a (b, i, j) (c, d) y y0 t =
    (
      type_defs_1 a b d y y0 t >>=
      \(g, e, u) -> (\(f, n0) -> (f, e, u, n0)) <$> type_defs_2 g (i, j, fst <$> e) c (fmap fst <$> u) b y)
  type_defs_1 ::
    (
      [Def_3] ->
      Map' Kind_0 ->
      Types ->
      Map' Class_4 ->
      Map' Class_5 ->
      Map' (Map' ([[String]], Status)) ->
      Err ([Def_4], Types, Map' (Map' ([[String]], Status))))
  type_defs_1 a b c y y0 u =
    case a of
      [] -> Right ([], c, u)
      d : e -> type_def_1 d b c y y0 u >>= \(f, g, u') -> (\(k, l, t') -> (f : k, l, t')) <$> type_defs_1 e b g y y0 u'
  type_defs_2 ::
    (
      [Def_4] ->
      (Map' Alg, Map' Constructor, Map' Type_2) ->
      Map' Expr_2 ->
      Map' (Map' [[String]]) ->
      Map' Kind_0 ->
      Map' Class_4 ->
      Err (Map' Expr_2, [Chck']))
  type_defs_2 a b c g i u =
    case a of
      [] -> Right (c, [])
      d : e -> type_def_2 d b c g i u >>= \(h, j) -> second ((++) j) <$> type_defs_2 e b h g i u
  type_eqs :: Integer -> Type_5 -> Map' Kind_0 -> Kind_1 -> Err (Integer, [(Kind_1, Kind_1)], Type_1)
  type_eqs i a d k =
    case a of
      Application_type_5 b c ->
        (
          type_eqs (i + 1) b d (Arrow_kind_1 (Var_kind_1 i) k) >>=
          \(i', t, b') -> (\(i2, u, c') -> (i2, t ++ u, Application_type_1 b' c')) <$> type_eqs i' c d (Var_kind_1 i))
      Name_type_5 (Name l b) -> und_err b d "type" l (\q -> Right (i, [(k, kind_0_to_1 q)], Name_type_1 b))
  type_expr ::
    (
      Maybe Name ->
      Type_1 ->
      (Map' Alg, Map' Constructor, Map' Type_2) ->
      Expression_1 ->
      Map' (Map' [[String]]) ->
      Integer ->
      Map' Kind_0 ->
      Err (Expression_2, [Chck']))
  type_expr k0 h (c, d, e) f m w b =
    (
      type_expression c d w (Left <$> e) f (type_transf h) b >>=
      \(g, i, _, z') ->
        (
          (\m0 ->
            let
              m' = typeback <$> m0
            in
              (exprrepl m' g, (\(Chck u v l) -> Chck' u v (exprrepl m' <$> l)) <$> chcks z')) <$>
          solve_all m k0 i Data.Map.empty))
  type_expr' ::
    (
      (Map' Kind_0, Map' Alg, Map' Constructor, Map' Type_2) ->
      Expression_1 ->
      Map' (Map' [[String]]) ->
      Err (Expression_2, [Chck']))
  type_expr' (b, c, d, e) f g =
    type_expr Nothing (list_type char_type) (c, d, e) (Application_expression_1 Write_expression_1 f) g 0 b
  type_expression ::
    (
      Map' Alg ->
      Map' Constructor ->
      Integer ->
      Map' (Either Type_2 Type_6) ->
      Expression_1 ->
      Type_6 ->
      Map' Kind_0 ->
      Err (Expression_4, Eqtns, Integer, ([Chck], Maybe Chck)))
  type_expression v w o d b e r7 =
    case b of
      Application_expression_1 c g ->
        (
          type_expression v w (o + 1) d c (function_type' (Var_type_6 o) e) r7 >>=
          \(i, Eqtns k0 k1 k2 k3, p, (x0, x1)) ->
            (
              (\(l, Eqtns m0 m1 m2 m3, n, x2) ->
                (
                  Application_expression_4 i l,
                  Eqtns ([o] ++ k0 ++ m0) (k1 ++ m1) (k2 ++ m2) (k3 ++ m3),
                  n,
                  (x0 ++ chcks x2, (\(Chck x3 x4 x5) -> Chck x3 x4 (x5 ++ [l])) <$> x1))) <$>
              type_expression v w p d g (Var_type_6 o) r7))
      Branch_expression_1 (Name a c) f g h ->
        let
          i = Data.Map.delete c r7
        in
          und_err
            c
            (Data.Map.delete "Zero" r7)
            "type variable"
            a
            (\j ->
              case j of
                Nat_kind_0 ->
                  (
                    type_expression v w o d f (sysrep' c (Name_type_6 "Zero") e) i >>=
                    \(k, l, m, p) ->
                      let
                        q =
                          case g of
                            Blank_pattern -> c ++ "-1"
                            Name_pattern s -> s
                      in
                        (
                          (\(s, t, u, y) ->
                            (
                              Branch_expression_4 c k g s,
                              Eqtns [] [] [] [Cond_eqtns c l q t],
                              u,
                              (chcks p ++ chcks y, Nothing))) <$>
                          type_expression
                            v
                            w
                            m
                            d
                            h
                            (sysrep' c (Application_type_6 (Name_type_6 "Next") (Name_type_6 q)) e)
                            (Data.Map.insert q Nat_kind_0 i)))
                _ -> Left (Error ("Type " ++ c ++ " at ") (location a ++ " is not of kind Nat.")))
      Char_expression_1 c -> Right (Char_expression_4 c, Eqtns [] [(e, char_type')] [] [], o, ([], Nothing))
      Function_expression_1 c f ->
        (
          type_pat (v, w) c (Var_type_6 o) d (o + 2) >>=
          \(g, h, i, j, k) ->
            (
              (\(l, Eqtns m n p q, s, u) ->
                (
                  Function_expression_4 g l,
                  Eqtns
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
                  s,
                  (chcks u, Nothing))) <$>
              type_expression v w i h f (Var_type_6 (o + 1)) r7))
      Int_expression_1 c -> Right (Int_expression_4 c, Eqtns [] [(e, int_type')] [] [], o, ([], Nothing))
      Match_expression_1 c g ->
        (
          type_expression v w (o + 1) d c (Var_type_6 o) r7 >>=
          \(k, Eqtns m0 m1 m2 m3, n, n') ->
            (
              (\(q, Eqtns u0 u1 u2 u3, x, n7) ->
                (
                  Match_expression_4 k q,
                  Eqtns (o : m0 ++ u0) (m1 ++ u1) (m2 ++ u2) (m3 ++ u3),
                  x,
                  (chcks n' ++ n7, Nothing))) <$>
              type_cases v w n d g o e r7))
      Modular_expression_1 (Modular c g1) ->
        Right (Modular_expression_4 g1, Eqtns [] [(e, mod_type' (int_to_nat' c))] [] [], o, ([], Nothing))
      Name_expression_1 (Name a7 c) ->
        und_err
          c
          d
          "variable"
          a7
          (\x2 ->
            Right
              (case x2 of
                Left (Basic_type_1 i x0 a' j n5) ->
                  let
                    g7 k2 d3 e4 s' r' =
                      let
                        ((s, p), (n, n9)) = typevars d3 (s', e4)
                      in
                        (
                          Glob_expression_4 c k2 n9,
                          Eqtns
                            (r' ++ n)
                            [(e, repl_transf p j)]
                            ((\(Constraint_1 a0 b0) -> (a0, (Realname (Name a7 c), p ! b0))) <$> a')
                            [],
                          s,
                          ([], (\n6 -> Chck (Name a7 c) n6 []) <$> n5))
                  in
                    case x0 of
                      Just (Constraint_1 _ _) ->
                        case i of
                          [] -> undefined
                          (d5, _) : d' -> g7 (Just (Var_type_6 o)) d' (Data.Map.singleton d5 (Var_type_6 o)) (o + 1) [o]
                      Nothing -> g7 Nothing i Data.Map.empty o []
                Right j -> (Loc_expression_4 c, Eqtns [] [(e, j)] [] [], o, ([], Nothing))))
      Write_expression_1 ->
        Right
          (
            Glob_expression_4 "Write" (Just (Var_type_6 o)) [],
            Eqtns
              [o]
              [(e, function_type' (Var_type_6 o) (list_type' char_type'))]
              [("Writeable", (Writeable, Var_type_6 o))]
              [],
            o + 1,
            ([], Nothing))
  type_exprs ::
    (
      String ->
      (Map' Alg, Map' Constructor, Map' Type_2) ->
      Map' (Map' [[String]]) ->
      [(Name, Expression_1, [(String, Kind_0)], [Constraint_1], Type_1)] ->
      (Map' Expr_2) ->
      String ->
      (Type_1 -> Type_1) ->
      Integer ->
      Map' Class_4 ->
      Map' Kind_0 ->
      [String] ->
      Err (Map' Expr_2, [Chck']))
  type_exprs a c d h i t z w t0 x2 f' =
    case h of
      [] -> Right (i, [])
      (Name j y, k, s, t5, l) : m ->
        (
          type_expr
            (Just (Name j (y ++ " " ++ a)))
            (z l)
            c
            k
            (type_constraints_1 t5 d t0)
            w
            (Prelude.foldl (\k' -> \(l', m0) -> Data.Map.insert l' m0 k') x2 s) >>=
          \(g, h') ->
            (
              second ((++) h') <$>
              type_exprs a c d m (Data.Map.insert (y ++ " " ++ t) (Expr_2 (Just f') (fst <$> s) g) i) t z w t0 x2 f'))
  type_fields ::
    (
      [(String, Kind_0)] ->
      [(String, Type_8)] ->
      Map' Kind_0 ->
      (Expression_2 -> Expr_2) ->
      Type_1 ->
      Integer ->
      (Types, Map' Expr_2) ->
      Err ([Type_1], (Types, Map' Expr_2)))
  type_fields c d e f h o (i, n) =
    case d of
      [] -> Right ([], (i, n))
      (j, k) : l ->
        (
          type_typ k e Star_kind_0 >>=
          \m ->
            (
              first ((:) m) <$>
              type_fields
                c
                l
                e
                f
                h
                (o + 1)
                (
                  ins_new j (Basic_type_1 c Nothing [] (function_type h m) Nothing) i,
                  Data.Map.insert j (f (Field_expression_2 o)) n)))
  type_form ::
    (
      [(String, Kind_0)] ->
      Form_1 ->
      Map' Kind_0 ->
      (Expression_2 -> Expr_2) ->
      String ->
      Type_1 ->
      (Types, Map' (Constructor, Status), Map' Expr_2) ->
      Err (String, (Types, Map' (Constructor, Status), Map' Expr_2)))
  type_form c (Form_1 d e f) g m h n (i, j, k) =
    (
      (\l ->
        (
          e,
          (
            Data.Map.insert e (Basic_type_1 c Nothing [] (Prelude.foldr function_type n l) Nothing, d) i,
            ins_new e (Constructor h l) j,
            Data.Map.insert e (m (Algebraic_expression_2 e [])) k))) <$>
      type_types f g)
  type_forms ::
    (
      [(String, Kind_0)] ->
      [Form_1] ->
      Map' Kind_0 ->
      (Expression_2 -> Expr_2) ->
      String ->
      Type_1 ->
      (Types, Map' (Constructor, Status), Map' Expr_2) ->
      Err ([String], (Types, Map' (Constructor, Status), Map' Expr_2)))
  type_forms c d e i l m f =
    case d of
      [] -> Right ([], f)
      g : h -> type_form c g e i l m f >>= \(j, k) -> first ((:) j) <$> type_forms c h e i l m k
  type_inh :: String -> [String] -> [String] -> Map' String -> Err ()
  type_inh a b c d =
    case c of
      [] -> Right ()
      e : f ->
        case a == e of
          False -> type_inh a (e : b) (maybeToList (Data.Map.lookup e d) ++ f) d
          True -> Left (Error ("Circular dependency between classes [" ++ intercalate ", " b ++ "] in ") ".")
  type_kinds :: [(String, Kind_0)] -> Map' Kind_0 -> Map' Kind_0
  type_kinds c d =
    case c of
      [] -> d
      (e, f) : g -> type_kinds g (Data.Map.insert e f d)
  type_method :: Method_2 -> Map' Kind_0 -> Err Method_3
  type_method (Method_2 b (Kinds_constraints' c f) d) e =
    Method_3 b (Kinds_constraints' c f) <$> type_typ d (type_kinds c e) Star_kind_0
  type_method_1 :: Map' Class_5 -> Method_3 -> Err Method_4
  type_method_1 g (Method_3 a (Kinds_constraints' b c) d) =
    let
      m = Prelude.foldl (\h -> \(i, j) -> Data.Map.insert i j h) Data.Map.empty b
    in
      (\f -> Method_4 a (Kinds_constraints_2 b f) d) <$> type_constraints_0 Data.Map.empty c (g, m, (\_ -> Zr) <$> m)
  type_methods_0 :: [Method_2] -> Map' Kind_0 -> Err [Method_3]
  type_methods_0 b c =
    case b of
      [] -> Right []
      e : g -> type_method e c >>= \h -> (:) h <$> type_methods_0 g c
  type_methods_1 :: Map' Class_5 -> [Method_3] -> Err [Method_4]
  type_methods_1 f a =
    case a of
      [] -> Right []
      b : c -> type_method_1 f b >>= \d -> (:) d <$> type_methods_1 f c
  type_mism_err :: Maybe Name -> Type_6 -> Type_6 -> Err t
  type_mism_err a0 b c =
    let
      k = "Type mismatch between " ++ typestr b ++ " and " ++ typestr c ++ " in "
    in
      Left
        (case a0 of
          Nothing -> Error k "."
          Just (Name d a) -> Error (k ++ a ++ " at ") (location' d))
  type_pat ::
    (
      (Map' Alg, Map' Constructor) ->
      Pat' ->
      Type_6 ->
      Map' (Either Type_2 Type_6) ->
      Integer ->
      Err (Pat_1, Map' (Either Type_2 Type_6), Integer, [Integer], [(Type_6, Type_6)]))
  type_pat (h, h') b c d l =
    case b of
      Application_pat' (Name g e) f ->
        und_err
          e
          h'
          "constructor"
          g
          (\(Constructor f' g') ->
            let
              Alg i j m = h ! f'
              ((p, q), (r, _)) = typevars i (l, Data.Map.empty)
            in
              case m of
                [_] ->
                  (
                    (\(s, t, u, v, w) -> (Application_pat_1 s, t, u, r ++ v, (c, repl_transf q j) : w)) <$>
                    type_pats (h, h') f (repl_transf q <$> g') d p (Name g e))
                _ -> Left (Error ("Constructor " ++ e ++ " at ") (location g ++ " is not a struct constructor.")))
      Blank_pat' -> Right (Blank_pat_1, d, l, [], [])
      Name_pat' e -> Right (Name_pat_1 e, Data.Map.insert e (Right c) d, l, [], [])
  type_pats ::
    (
      (Map' Alg, Map' Constructor) ->
      [Pat'] ->
      [Type_6] ->
      Map' (Either Type_2 Type_6) ->
      Integer ->
      Name ->
      Err ([Pat_1], Map' (Either Type_2 Type_6), Integer, [Integer], [(Type_6, Type_6)]))
  type_pats b d e f g (Name x y) =
    case d of 
      [] ->
        let
          n = show <$> [0 .. length e - 1]
        in
          case e of
            [] ->
              Right (Name_pat_1 <$> n, Prelude.foldl (\j -> \(k, m) -> Data.Map.insert k (Right m) j) f (zip n e), g, [], [])
            _ -> Left (Error ("Constructor " ++ y ++ " at ") (location x ++ " has been given too few arguments."))
      j : k ->
        case e of
          [] -> Left (Error ("Constructor " ++ y ++ " at ") (location x ++ " has been given too many arguments."))
          m : n ->
            (
              type_pat b j m f g >>=
              \(c, o, p, q, r) -> (\(s, t, u, v, w) -> (c : s, t, u, q ++ v, r ++ w)) <$> type_pats b k n o p (Name x y))
  type_transf :: Type_1 -> Type_6
  type_transf a =
    case a of
      Application_type_1 b c -> Application_type_6 (type_transf b) (type_transf c)
      Name_type_1 b -> Name_type_6 b
  type_typ :: Type_8 -> Map' Kind_0 -> Kind_0 -> Err Type_1
  type_typ (Type_8 b c) d e = type_eqs 0 c d (kind_0_to_1 e) >>= \(_, h, i) -> i <$ solve_type_eqs b h
  type_types :: [Type_8] -> Map' Kind_0 -> Err [Type_1]
  type_types a b =
    case a of
      [] -> Right []
      c : d -> type_typ c b Star_kind_0 >>= \e -> (:) e <$> type_types d b
  type_types' :: Map' Kind_0 -> [(String, Type_8)] -> Err [(String, Type_1)]
  type_types' c d =
    case d of
      [] -> Right []
      (e, f) : g -> type_typ f c Star_kind_0 >>= \h -> (:) (e, h) <$> type_types' c g
  types :: Map' Type_2
  types =
    Data.Map.fromList
      (
        (
          second
            (\(Constructor a b) ->
              let
                Alg c d _ = algebraics ! a
              in
                Basic_type_1 c Nothing [] (Prelude.foldr function_type d b) Nothing) <$>
          constrs) ++
        [
          (
            "Add",
            Basic_type_1
              [("T", Star_kind_0)]
              (Just (Constraint_1 "Ring" "T"))
              [Constraint_1 "Ring" "T"]
              (function_type (Name_type_1 "T") (function_type (Name_type_1 "T") (Name_type_1 "T")))
              Nothing),
          (
            "Compare",
            Basic_type_1
              [("T", Star_kind_0)]
              (Just (Constraint_1 "Ord" "T"))
              [Constraint_1 "Ord" "T"]
              (function_type (Name_type_1 "T") (function_type (Name_type_1 "T") comparison_type))
              Nothing),
          (
            "Convert",
            Basic_type_1
              [("T", Star_kind_0)]
              (Just (Constraint_1 "Ring" "T"))
              [Constraint_1 "Ring" "T"]
              (function_type int_type (Name_type_1 "T"))
              Nothing),
          ("Crash", Basic_type_1 [("T", Star_kind_0)] Nothing [] (Name_type_1 "T") Nothing),
          ("Div", Basic_type_1 [] Nothing [] (function_type int_type (function_type int_type (maybe_type int_type))) Nothing),
          (
            "Inverse",
            Basic_type_1
              [("T", Star_kind_0)]
              (Just (Constraint_1 "Field" "T"))
              [Constraint_1 "Field" "T", Constraint_1 "Ring" "T"]
              (function_type (Name_type_1 "T") (maybe_type (Name_type_1 "T")))
              Nothing),
          ("Mod", Basic_type_1 [] Nothing [] (function_type int_type (function_type int_type (maybe_type int_type))) Nothing),
          (
            "Times",
            Basic_type_1
              [("T", Star_kind_0)]
              (Just (Constraint_1 "Ring" "T"))
              [Constraint_1 "Ring" "T"]
              (function_type (Name_type_1 "T") (function_type (Name_type_1 "T") (Name_type_1 "T")))
              Nothing),
          (
            "Write_brackets",
            Basic_type_1
              [("T", Star_kind_0)]
              (Just (Constraint_1 "Writeable" "T"))
              [Constraint_1 "Writeable" "T"]
              (function_type (Name_type_1 "T") op_type)
              Nothing),
          (
            "Write",
            Basic_type_1
              [("T", Star_kind_0)]
              (Just (Constraint_1 "Writeable" "T"))
              [Constraint_1 "Writeable" "T"]
              (function_type (Name_type_1 "T") (list_type char_type))
              Nothing)])
  typestr :: Type_6 -> String
  typestr a = fst (typestr' a)
  typestr' :: Type_6 -> (String, Bool)
  typestr' a =
    case a of
      Application_type_6 b c ->
        let
          (d, e) = typestr' c
        in
          (
            (
              typestr b ++
              " " ++
              case e of
                False -> d
                True -> "(" ++ d ++ ")"),
            True)
      Name_type_6 b -> (b, False)
      Var_type_6 b -> (show b, False)
  typestring :: Type_1 -> [Type_1] -> (String, [Type_1])
  typestring a d =
    case a of
      Application_type_1 b c -> typestring b (c : d)
      Name_type_1 b -> (b, d)
  typevars :: [(String, Kind_0)] -> (Integer, Map' Type_6) -> ((Integer, Map' Type_6), ([Integer], [Type_6]))
  typevars a (b, c) =
    case a of
      [] -> ((b, c), ([], []))
      (d, _) : e -> second (bimap ((:) b) ((:) (Var_type_6 b))) (typevars e (b + 1, Data.Map.insert d (Var_type_6 b) c))
  typevars' ::
    (
      Map' Kind_0 ->
      (String -> Err (Map' Type_6, [Type_6])) ->
      [(String, Kind_0)] ->
      [Type_8] ->
      Map' Type_6 ->
      Err (Map' Type_6, [Type_6]))
  typevars' j a b c d =
    case b of
      [] ->
        case c of
          [] -> Right (d, [])
          _ -> a "many"
      (e, f) : g ->
        case c of
          [] -> a "few"
          h : i ->
            (
              type_typ h j f >>=
              \m ->
                let
                  t = type_transf m
                in
                  second ((:) t) <$> typevars' j a g i (Data.Map.insert e t d))
  typing :: Tree_5 -> (File, Map' Expr_2) -> Err (File, Map' Expr_2, [Chck'])
  typing (Tree_5 a a' c) (File d t u v b' c5 x t2, l) =
    (
      type_datas a (old d, old t, old u, old v, l) >>=
      \(e, b, h, g, f) ->
        (
          type_classes (fst <$> e) a' (old b', g, old t2, old c5) >>=
          \(c', g0, x2, t3) ->
            (
              (\(i, j, y, z9) ->
                (
                  File
                    (rem_old e)
                    (rem_old b)
                    (rem_old h)
                    (Prelude.foldl
                      (\j3 -> \(j2, a7) ->
                        Data.Map.adjust (\(Basic_type_1 z0 z1 z2 z3 _) -> Basic_type_1 z0 z1 z2 z3 (Just a7)) j2 j3)
                      (rem_old j)
                      [])
                    (rem_old c')
                    (rem_old t3)
                    (rem_old' y)
                    (rem_old x2),
                  i,
                  z9)) <$>
              type_defs c (fst <$> e, fst <$> b, fst <$> h) (f, g0) (fst <$> c') (fst <$> t3) (old' x))))
  unsafe_left :: Either t u -> t
  unsafe_left a =
    case a of
      Left b -> b
      Right _ -> undefined
--------------------------------------------------------------------------------------------------------------------------------