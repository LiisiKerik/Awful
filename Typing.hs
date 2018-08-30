{-
internal: make the system of specifying built-in algebraic data types and things better and safer
Allow hiding things to functions outside module - so that helper functions are not exported from the module?
normalising constructors for some data types (polynomial, fraction) which assume a certain normal form of fields?
allow to hide (prevent exporting) constructors and field accessors which can potentially have bad behavior
syntactic sugar for lists, vectors, matrices... allow writing (tiny, limited to expression parsing) language extensions?
boolean function library
implement map and set (AVL trees?)
different ways of folding lists, vectors etc
module system related functions into a separate file?
todo: make a function writing operator/function. For printing stuff like "Complex (Fraction 0 1) (Fraction 1 1)"
checki abil võiks saada tüübikontrollida korraga mitut moodulit, andes ette nimekirja
süntaktiline suhkur (listide sün.suhk.) standard moodulisse?
operaatorid struktuuride ja algebraliste andmetüüpide patternmatchides
semantics of "Pair -> f" should be "Pair x y -> f x y"
mis juhtub kui esimeses moodulis on kusagil tüübimuutuja T ja järgmises moodulis sama nimega globaalne tüüp?
Let f = Crash, x = f f In 0 -- tüüpimine läheb lõpmatusse tsüklisse sest puudub occur check
"./Awful eval "List (0)"" without importing Standard.awf - error message about Writeable class looks bad; fix
let expr de-sugaring (and therefore struct name collection) completely to Standard.hs module
all de-sugaring: remove from Tree.hs, put into Standard.hs
simplify parsing of match expression and remove duplicate code from de-sugaring, name checking, typechecking & eval
What happens with unary minus and binary minus during parsing?
allow using operators in class method definitions? Instance Ring{Complex T}<Ring T>(..., Complex x y * Complex z w = ...)
Match expression parsing has a bug. Match Foo{Int} vs Match x {False -> ...
check that struct pattern matching for branching types never occurs in worng place: Branch N {Zero -> Construct_Array x y -> ...
Ensure that <Nonzero N> constraint gets translated to N = Next N'.
syntactic sugar in pattern matching. List (x, y, z) -> ...    and Array (x, y, z) in function argument
move single-name-pattern disambiguation from Naming to Standard?
unused variable warnings?
eta reduction warnings?
variable defined under let used in only one place warning?
move modular checks to parser, std or namer?
special type (with special constructor for flexible type variables) for type equations?
not give equations as argument; instead, compose equations with ++
check in Naming module that all pattern constructors are valid
special class for countable/enumerable things (like Integer). Stronger than Ord, weaker than Finite
tests
make Integer counter part of Eqtns and construct the set after all flexi type variables have been generated, in the end
write long types and kinds in error messages?
"requires instance or constraint" -> "requires instance" / "requires constraint"
syntax: allow writing List[sometype] to disambiguate type of list when using list syntactic sugar
teha eraldi algebraline andmetüüp juba väärtustatud avaldise jaoks et undef-e vähemaks saada?
-}
--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Typing where
  import Control.Monad
  import Data.Bifunctor
  import Data.Foldable
  import Data.List
  import Data.Map
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
    Modular_alg_pat_3 Modular' |
    Struct_alg_pat_3 String [Alg_pat_3]
      deriving Show
  type Algebraics = Map' (Alg, Status)
  data Case_3 = Case_3 Alg_pat_2 Expression_2 deriving Show
  data Class_3 = Class_3 String (String, Kind_0) (Maybe Name) [Method_3] deriving Show
  data Class_4 = Class_4 (String, Kind_0) (Maybe String) [Method_4] deriving Show
  data Class_5 = Class_5 Kind_0 (Maybe String) [String] deriving Show
  data Cond_eqtns = Cond_eqtns String Eqtns String Eqtns deriving Show
  data Constraint_1 = Constraint_1 String String deriving Show
  data Constructor = Constructor String [Type_1] deriving Show
  data Def_4 =
    Basic_def_4 Location_0 String [(String, Kind_0)] [Constraint_1] Type_1 Expression_1 |
    Instance_4
      Location_0
      String
      (Maybe String)
      String
      String
      [(String, Kind_0)]
      Integer
      [(Name, Expression_1, [(String, Kind_0)], [Constraint_1], Type_1)]
      Type_1
      [Constraint_1]
      [[String]]
        deriving Show
  data Eqtns = Eqtns (Set String) [(Type_1, Type_1)] [(String, (Name, Type_1))] [Cond_eqtns] deriving Show
  data Expr_2 = Expr_2 (Maybe [String]) [String] Expression_2 deriving Show
  data Expression_2 =
    Add_Int_0_expression_2 |
    Add_Int_1_expression_2 Integer |
    Add_Modular_0_expression_2 Integer |
    Add_Modular_1_expression_2 Integer Integer |
    Algebraic_expression_2 String [Expression_2] |
    Application_expression_2 Expression_2 Expression_2 |
    Branch_expression_2 Type_1 Expression_2 String Expression_2 |
    Char_expression_2 Char |
    Compare_Char_0_expression_2 |
    Compare_Char_1_expression_2 Char |
    Compare_Int_0_expression_2 |
    Compare_Int_1_expression_2 Integer |
    Compare_Modular_0_expression_2 |
    Compare_Modular_1_expression_2 Integer |
    Convert_Int_expression_2 |
    Convert_Modular_expression_2 Integer |
    Div_0_expression_2 |
    Div_1_expression_2 Integer |
    Div'_expression_2 Integer |
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
  data Globloc = Glob | Loc deriving Show
  data Kind_1 = Arrow_kind_1 Kind_1 Kind_1 | Nat_kind_1 | Star_kind_1 | Var_kind_1 Integer deriving (Eq, Show)
  data Method_3 = Method_3 String [(String, Kind_0)] [Constraint_0] Type_1 deriving Show
  data Method_4 = Method_4 String [(String, Kind_0)] [Constraint_1] Type_1 deriving Show
  data Modular' = Modular' Integer Integer deriving Show
  data Pattern_5 =
    Blank_pattern_5 |
    Char_blank_pattern_5 (Set Char) |
    Char_pattern_5 Char |
    Int_blank_pattern_5 (Set Integer) |
    Int_pattern_5 Integer |
    Modular_pattern_5 Integer |
    Struct_pattern_5 String [Pattern_5]
      deriving Show
  data Nat = Nxt Nat | Zr deriving (Eq, Ord, Show)
  data Pat_1 = Application_pat_1 [Pat_1] | Blank_pat_1 | Name_pat_1 String deriving Show
  data Type_1 = Application_type_1 Type_1 Type_1 | Name_type_1 String deriving (Eq, Show)
  data Type_2 = Basic_type_1 [(String, Kind_0)] (Maybe Constraint_1) [Constraint_1] Type_1 deriving Show
  type Types = Map' (Type_2, Status)
  algebraics :: Map' Alg
  algebraics = (\(Alg' a b c) -> Alg a b (fst <$> c)) <$> Data.Map.fromList algebraics'
  algebraics' :: [(String, Alg')]
  algebraics' =
    [
      ("Comparison", Alg' [] comparison_type [("LT", []), ("EQ", []), ("GT", [])]),
      (
        "Either",
        Alg'
          [("T", Star_kind_0), ("U", Star_kind_0)]
          (either_type (Name_type_1 "T") (Name_type_1 "U"))
          [("Left", [Name_type_1 "T"]), ("Right", [Name_type_1 "U"])]),
      (
        "List",
        Alg'
          [("T", Star_kind_0)]
          (list_type (Name_type_1 "T"))
          [("Empty_List", []), ("Construct_List", [Name_type_1 "T", list_type (Name_type_1 "T")])]),
      ("Logical", Alg' [] logical_type [("False", []), ("True", [])]),
      ("Maybe", Alg' [("T", Star_kind_0)] (maybe_type (Name_type_1 "T")) [("Nothing", []), ("Wrap", [Name_type_1 "T"])]),
      (
        "Pair",
        Alg'
          [("T", Star_kind_0), ("U", Star_kind_0)]
          (pair_type (Name_type_1 "T") (Name_type_1 "U"))
          [("Pair", [Name_type_1 "T", Name_type_1 "U"])])]
  chain_constraints :: Maybe String -> Map' Class_5 -> Map' (Map' [String]) -> String -> Map' (Map' [String])
  chain_constraints a b c e =
    case a of
      Just d ->
        let
          Class_5 _ f o = b ! d
        in
          chain_constraints
            f
            b
            (Data.Map.insert
              d
              (Data.Map.insert
                e 
                ((\u -> u ++ " " ++ e) <$> o)
                (case Data.Map.lookup d c of
                  Just g -> g
                  Nothing -> Data.Map.empty))
              c)
            e
      Nothing -> c
  char_type :: Type_1
  char_type = Name_type_1 "Char"
  check_kind :: String -> String -> Map' (Either Kind_0 Kind_0) -> Type_1 -> Err Kind_0
  check_kind j c a b =
    let
      x = Left j
    in
      case b of
        Application_type_1 d e -> check_kind j c a d >>= \f -> case f of
          Arrow_kind_0 g h -> check_kind j c a e >>= \i -> if i == g then Right h else x
          _ -> x
        Name_type_1 d ->
          if d == c
            then x
            else
              Right
                (
                  case a ! d of
                    Left e -> e
                    Right e -> e)
  check_mod :: (Location_0 -> Location_1) -> Modular -> Err (Modular', Type_1)
  check_mod a (Modular d b c) =
    if c < b
      then Right (Modular' b c, mod_type (int_to_nat' b))
      else Left ("Invalid Modular " ++ show c ++ " # " ++ show b ++ location' (a d))
  classes_0 :: Map' Class_4
  classes_0 =
    Data.Map.fromList
      [
        (
          "Field",
          Class_4
            ("T", Star_kind_0)
            (Just "Ring")
            [Method_4 "Inverse" [] [] (function_type (Name_type_1 "T") (maybe_type (Name_type_1 "T")))]),
        ("Nonzero", Class_4 ("N", Nat_kind_0) Nothing [Method_4 "Div'" [] [] (function_type int_type int_type)]),
        (
          "Ord",
          Class_4
            ("T", Star_kind_0)
            Nothing
            [Method_4 "Compare" [] [] (function_type (Name_type_1 "T") (function_type (Name_type_1 "T") comparison_type))]),
        (
          "Ring",
          Class_4
            ("T", Star_kind_0)
            Nothing
            [
              Method_4 "Add" [] [] (function_type (Name_type_1 "T") (function_type (Name_type_1 "T") (Name_type_1 "T"))),
              Method_4 "Convert" [] [] (function_type int_type (Name_type_1 "T")),
              Method_4
                "Multiply"
                []
                []
                (function_type (Name_type_1 "T") (function_type (Name_type_1 "T") (Name_type_1 "T"))),
              Method_4 "Negate" [] [] (function_type (Name_type_1 "T") (Name_type_1 "T"))]),
        (
          "Writeable",
          Class_4
            ("T", Star_kind_0)
            Nothing
            [
              Method_4
                "Write_Brackets"
                []
                []
                (function_type (Name_type_1 "T") (pair_type (list_type char_type) logical_type))])]
  classes_1 :: Map' Class_5
  classes_1 = (\(Class_4 (_, a) b c) -> Class_5 a b ((\(Method_4 d _ _ _) -> d) <$> c)) <$> classes_0
  classes_2 :: Map' Kind_0
  classes_2 = (\(Class_4 (_, a) _ _) -> a) <$> classes_0
  comparison_type :: Type_1
  comparison_type = Name_type_1 "Comparison"
  compose_patterns :: (Pattern_5, Bool) -> ([Pattern_5], Bool) -> ([Pattern_5], Bool)
  compose_patterns (x, y) (z, w) = (x : z, y && w)
  constr_check :: Map' (Maybe String) -> [String] -> [[String]] -> [[String]] -> Maybe String
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
  constr_check' :: Map' (Maybe String) -> String -> [String] -> [String] -> Maybe String
  constr_check' n t x y =
    case x of 
      [] -> Nothing
      a : x' ->
        case constr_check'' n t a y of
          Left m -> Just m
          Right y' -> constr_check' n t x' y'
  constr_check'' :: Map' (Maybe String) -> String -> String -> [String] -> Err [String]
  constr_check'' m t c x =
    case x of
      [] -> Left (c ++ " " ++ t)
      a : x' -> if constr_check_3 m c a then Right x' else (:) a <$> constr_check'' m t c x'
  constr_check_3 :: Map' (Maybe String) -> String -> String -> Bool
  constr_check_3 m x y =
    if x == y
      then True
      else
        case m ! y of
          Just y' -> constr_check_3 m x y'
          Nothing -> False
  constrs :: [(String, Constructor)]
  constrs = join ((\(a, Alg' _ _ b) -> (\(c, d) -> (c, Constructor a d)) <$> b) <$> algebraics')
  context_union :: (File, Map' Op) -> (File, Map' Op) -> (File, Map' Op)
  context_union (File b i j d e q t g, t0) (File f k l h m r u n, t2) =
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
      Data.Map.union t0 t2)
  defs :: Map' Expr_2
  defs =
    Data.Map.fromList
      (
        join
          (
            (\(_, Alg' a _ c) ->
              (\(d, e) ->
                let
                  f = show <$> [0 .. length e - 1]
                in
                  (
                    d,
                    Expr_2
                      Nothing
                      (fst <$> a)
                      (Prelude.foldr
                        (\g -> Function_expression_2 (Name_pat_1 g))
                        (Algebraic_expression_2 d (Loc_expression_2 <$> f))
                        f))) <$> c) <$>
            algebraics') ++
        [
          ("Add Int", Expr_2 (Just []) [] Add_Int_0_expression_2),
          ("Compare Char", Expr_2 (Just []) [] Compare_Char_0_expression_2),
          ("Compare Int", Expr_2 (Just []) [] Compare_Int_0_expression_2),
          ("Convert Int", Expr_2 (Just []) [] Convert_Int_expression_2),
          ("Div", Expr_2 Nothing [] Div_0_expression_2),
          ("First", Expr_2 Nothing ["T", "U"] (Field_expression_2 0)),
          ("Mod", Expr_2 Nothing [] Mod_0_expression_2),
          ("Multiply Int", Expr_2 (Just []) [] Multiply_Int_0_expression_2),
          ("Negate Int", Expr_2 (Just []) [] Negate_Int_expression_2),
          ("Second", Expr_2 Nothing ["T", "U"] (Field_expression_2 1)),
          ("Write_Brackets Int", Expr_2 (Just []) [] Write_Brackets_Int_expression_2)])
  either_type :: Type_1 -> Type_1 -> Type_1
  either_type x = Application_type_1 (Application_type_1 (Name_type_1 "Either") x)
  find_and_delete :: Ord t => Map t u -> t -> Maybe (u, Map t u)
  find_and_delete a b = (\c -> (c, Data.Map.delete b a)) <$> Data.Map.lookup b a
  function_type :: Type_1 -> Type_1 -> Type_1
  function_type a = Application_type_1 (Application_type_1 (Name_type_1 "Function") a)
  get_pattern_type ::
    (
      (Location_0 -> Location_1) ->
      Map' Alg ->
      Map' Constructor ->
      (Integer, Set String, [(Type_1, Type_1)], Map' (Type_2, Globloc)) ->
      Alg_pat_1 ->
      Type_1 ->
      Err ((Integer, Set String, [(Type_1, Type_1)], Map' (Type_2, Globloc)), (Alg_pat_2, Alg_pat_3, Maybe (Name, [String]))))
  get_pattern_type a b c (d, e, f, n) g h =
    case g of
      Application_alg_pat_1 o i j ->
        let
          Constructor p y = c ! i
          Alg k m _ = b ! p
          ((q, r, s), _) = typevars k (d, Data.Map.empty, e)
        in
          (
            second (\(a0, b0, c0) -> (Application_alg_pat_2 i a0, Struct_alg_pat_3 i b0, c0)) <$>
            get_pattern_types a b c (q, s, (h, repl' r m) : f, n) j (repl' r <$> y) (Name o i))
      Blank_alg_pat_1 -> Right ((d, e, f, n), (Blank_alg_pat_2, Blank_alg_pat_3, Nothing))
      Char_alg_pat_1 i -> Right ((d, e, (h, char_type) : f, n), (Char_alg_pat_2 i, Char_alg_pat_3 i, Nothing))
      Int_alg_pat_1 i -> Right ((d, e, (h, int_type) : f, n), (Int_alg_pat_2 i, Int_alg_pat_3 i, Nothing))
      Modular_alg_pat_1 i ->
        (
          (\(Modular' j z, k) -> ((d, e, (h, k) : f, n), (Modular_alg_pat_2 z, Modular_alg_pat_3 (Modular' j z), Nothing))) <$>
          check_mod a i)
      Name_alg_pat_1 i ->
        Right ((d, e, f, Data.Map.insert i (Basic_type_1 [] Nothing [] h, Loc) n), (Name_alg_pat_2 i, Blank_alg_pat_3, Nothing))
  get_pattern_types ::
    (
      (Location_0 -> Location_1) ->
      Map' Alg ->
      Map' Constructor ->
      (Integer, Set String, [(Type_1, Type_1)], Map' (Type_2, Globloc)) ->
      [Alg_pat_1] ->
      [Type_1] ->
      Name ->
      Err
        ((Integer, Set String, [(Type_1, Type_1)], Map' (Type_2, Globloc)), ([Alg_pat_2], [Alg_pat_3], Maybe (Name, [String]))))
  get_pattern_types a b c (d0, d1, d2, d3) e f (Name m n) =
    let
      t = show <$> [0 .. length f - 1]
    in
      case e of
        [] ->
          Right
            (
              (
                d0,
                d1,
                d2,
                Prelude.foldl (\u -> \(m', v) -> Data.Map.insert m' (Basic_type_1 [] Nothing [] v, Loc) u) d3 (zip t f)),
              (
                [],
                [],
                case t of
                  [] -> Nothing
                  _ -> Just (Name m n, t)))
        g : h ->
          case f of
            [] -> Left ("Constructor " ++ n ++ location (a m) ++ " has been given too many arguments.")
            i : j ->
              (
                get_pattern_type a b c (d0, d1, d2, d3) g i >>=
                \(k, (l, p, q)) ->
                  case q of
                    Nothing -> second (\(v, w, u1) -> (l : v, p : w, u1)) <$> get_pattern_types a b c k h j (Name m n)
                    Just (Name f0 f1, _) ->
                      Left ("Constructor " ++ f1 ++ location (a f0) ++ " has been given too few arguments."))
  init_type_context :: (File, Map' Op)
  init_type_context = (File kinds algebraics (Data.Map.fromList constrs) types classes_0 classes_1 instances classes_2, Data.Map.empty)
  instances :: Map' (Map' [[String]])
  instances =
    Data.Map.fromList
      [
        ("Field", Data.Map.fromList [("Modular", [["Nonzero"]])]),
        ("Nonzero", Data.Map.fromList [("Next", [[]])]),
        ("Ord", Data.Map.fromList [("Char", []), ("Int", []), ("Modular", [[]])]),
        ("Ring", Data.Map.fromList [("Int", []), ("Modular", [["Nonzero"]])]),
        ("Writeable", Data.Map.fromList [("Int", []), ("Modular", [[]])])]
  int_to_nat' :: Integer -> Type_1
  int_to_nat' a =
    case a of
      0 -> Name_type_1 "Zero"
      _ -> Application_type_1 (Name_type_1 "Next") (int_to_nat' (a - 1))
  int_type :: Type_1
  int_type = Name_type_1 "Int"
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
  kind_err :: Location_1 -> Err t
  kind_err a = Left ("Kind mismatch" ++ location' a)
  kind_mism_err :: Location_1 -> Kind_1 -> Kind_1 -> String
  kind_mism_err a b c = "Kind mismatch" ++ location a ++ " between " ++ show_kind b ++ " and " ++ show_kind c ++ "."
  kinds :: Map' Kind_0
  kinds =
    Data.Map.fromList
      [
        ("Char", Star_kind_0),
        ("Comparison", Star_kind_0),
        ("Either", Arrow_kind_0 Star_kind_0 (Arrow_kind_0 Star_kind_0 Star_kind_0)),
        ("Function", Arrow_kind_0 Star_kind_0 (Arrow_kind_0 Star_kind_0 Star_kind_0)),
        ("Int", Star_kind_0),
        ("List", Arrow_kind_0 Star_kind_0 Star_kind_0),
        ("Logical", Star_kind_0),
        ("Maybe", Arrow_kind_0 Star_kind_0 Star_kind_0),
        ("Modular", Arrow_kind_0 Nat_kind_0 Star_kind_0),
        ("Next", Arrow_kind_0 Nat_kind_0 Nat_kind_0),
        ("Pair", Arrow_kind_0 Star_kind_0 (Arrow_kind_0 Star_kind_0 Star_kind_0)),
        ("Zero", Nat_kind_0)]
  list_type :: Type_1 -> Type_1
  list_type = Application_type_1 (Name_type_1 "List")
  location_err' :: String -> Location_1 -> Location_1 -> String
  location_err' a b = location_err a (Library b)
  locations :: Locations
  locations =
    Data.Map.fromList
      (
        (\x -> (x, Language)) <$>
        (
          (fst <$> constrs) ++
          [
            "Add",
            "Char",
            "Compare",
            "Comparison",
            "Convert",
            "Crash",
            "Div",
            "Div'",
            "Either",
            "Field",
            "First",
            "Function",
            "Int",
            "Inverse",
            "List",
            "Logical",
            "Maybe",
            "Mod",
            "Modular",
            "Multiply",
            "Negate",
            "Next",
            "Nonzero",
            "Ord",
            "Ring",
            "Second",
            "Write_Brackets",
            "Writeable",
            "Zero"]))
  logical_type :: Type_1
  logical_type = Name_type_1 "Logical"
  maybe_type :: Type_1 -> Type_1
  maybe_type = Application_type_1 (Name_type_1 "Maybe")
  mod_type :: Type_1 -> Type_1
  mod_type = Application_type_1 (Name_type_1 "Modular")
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
  old' :: Map' (Map' t) -> Map' (Map' (t, Status))
  old' = (<$>) old
  pair_type :: Type_1 -> Type_1 -> Type_1
  pair_type x = Application_type_1 (Application_type_1 (Name_type_1 "Pair") x)
  pats :: (Location_0 -> Location_1) -> (Map' Constructor, Map' [String]) -> [(Location_0, [Alg_pat_3])] -> Err ()
  pats a b = traverse_ (\(d, e) -> patterns b (a d, e))
  pattern :: (Map' Constructor, Map' [String]) -> [(Pattern_5, Bool)] -> Alg_pat_3 -> ([(Pattern_5, Bool)], Bool)
  pattern context x y =
    case x of
      [] -> ([], False)
      z : a ->
        let
          (b, c) = pattern' context z y
        in
          bimap ((++) b) ((||) c) (pattern context a y)
  pattern' :: (Map' Constructor, Map' [String]) -> (Pattern_5, Bool) -> Alg_pat_3 -> ([(Pattern_5, Bool)], Bool)
  pattern' context (x, y) z =
    case y of
      False -> split_pattern context x z
      True -> ([(x, True)], False)
  patterns :: (Map' Constructor, Map' [String]) -> (Location_1, [Alg_pat_3]) -> Err ()
  patterns context (l, x) =
    (
      patterns' context [(Blank_pattern_5, False)] (l, x) >>=
      \y ->
        case all snd y of
          False -> Left ("Incomplete match" ++ location' l)
          True -> Right ())
  patterns' :: (Map' Constructor, Map' [String]) -> [(Pattern_5, Bool)] -> (Location_1, [Alg_pat_3]) -> Err [(Pattern_5, Bool)]
  patterns' context x (l, y) =
    case y of
      [] -> Right x
      z : a ->
        let
          (b, c) = pattern context x z
        in
          case c of
            False -> Left ("Unnecessary patterns" ++ location' l)
            True -> patterns' context b (l, a)
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
  repl' :: Map' Type_1 -> Type_1 -> Type_1
  repl' a b =
    case b of
      Application_type_1 c d -> Application_type_1 (repl' a c) (repl' a d)
      Name_type_1 c ->
        case Data.Map.lookup c a of
          Just d -> d
          Nothing -> b
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
  slv :: Map' (Map' [[String]]) -> [(String, (Name, Type_1))] -> (Name -> String -> String -> String) -> Err ()
  slv a b h =
    case b of
      [] -> Right ()
      (c, (y, d)) : e ->
        let
          (f, g) = typestring d []
          i = Left (h y c f)
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
      [(String, (Name, Type_1))] ->
      (Name -> String -> String -> String) ->
      [Type_1] ->
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
  solve_all :: (Location_0 -> Location_1) -> Map' (Map' [[String]]) -> String -> Eqtns -> Expression_2 -> Err Expression_2
  solve_all a b c (Eqtns d e f g) h =
    (
      solvesys (\i -> \j -> Left ("Type mismatch between " ++ min i j ++ " and " ++ max i j ++ c)) e (f, h, d, g) >>=
      \(i, j, k, l) ->
        case Data.Set.null k of
-- TODO: make this error message more detailed (say which branch this occurred in, with location)?
          False -> Left ("Unresolved type variables" ++ c)
          True ->
            (
              slv
                b
                i
                (\(Name m n) -> \o -> \p ->
                  "Function " ++ n ++ location (a m) ++ " requires instance or constraint " ++ o ++ " " ++ p ++ ".") >>
              solve_conds a b c l j))
  solve_cond :: (Location_0 -> Location_1) -> Map' (Map' [[String]]) -> String -> Cond_eqtns -> Expression_2 -> Err Expression_2
  solve_cond a b c (Cond_eqtns _ e _ g) h = solve_all a b c e h >>= solve_all a b c g
  solve_conds ::
    (Location_0 -> Location_1) -> Map' (Map' [[String]]) -> String -> [Cond_eqtns] -> Expression_2 -> Err Expression_2
  solve_conds a b c d e =
    case d of
      [] -> Right e
      f : g -> solve_cond a b c f e >>= solve_conds a b c g
  solve_type_eqs :: Location_1 -> [(Kind_1, Kind_1)] -> Err ()
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
          (f, g) -> Left (kind_mism_err a f g)
  solve_type_eqs' :: Bool -> Location_1 -> Integer -> Kind_1 -> [(Kind_1, Kind_1)] -> Err ()
  solve_type_eqs' e a b c d =
    case occ_kind b c of
      False -> solve_type_eqs a (repl_kind_eqs b c d)
      True ->
        Left
          (case e of
            False -> kind_mism_err a c (Var_kind_1 b)
            True -> kind_mism_err a (Var_kind_1 b) c)
  solvesys ::
    (String -> String -> Err ([(String, (Name, Type_1))], Expression_2, Set String, [Cond_eqtns])) ->
    [(Type_1, Type_1)] ->
    ([(String, (Name, Type_1))], Expression_2, Set String, [Cond_eqtns]) ->
    Err ([(String, (Name, Type_1))], Expression_2, Set String, [Cond_eqtns])
  solvesys m b (a', t, u, u1) =
    case b of
      [] -> Right (a', t, u, u1)
      (c, d) : g ->
        case c of
          Application_type_1 e f ->
            case d of
              Application_type_1 h i -> solvesys m ((e, h) : (f, i) : g) (a', t, u, u1)
              Name_type_1 h -> solvesys' m h c g (a', t, u, u1)
          Name_type_1 e ->
            case d of
              Name_type_1 f ->
                if e == f
                  then solvesys m g (a', t, u, u1)
                  else
                    case Data.Set.member e u of
                      False ->
                        case Data.Set.member f u of
                          False -> m e f
                          True -> solvesys_rep m f c g (a', t, u, u1)
                      True -> solvesys_rep m e d g (a', t, u, u1)
              _ -> solvesys' m e d g (a', t, u, u1)
  solvesys' ::
    (String -> String -> Err ([(String, (Name, Type_1))], Expression_2, Set String, [Cond_eqtns])) ->
    String ->
    Type_1 ->
    [(Type_1, Type_1)] ->
    ([(String, (Name, Type_1))], Expression_2, Set String, [Cond_eqtns]) ->
    Err ([(String, (Name, Type_1))], Expression_2, Set String, [Cond_eqtns])
  solvesys' h b c d (x, m, a, w) =
    let
      (y, _) = typestring c []
    in
      case Data.Set.member b a of
        False ->
          h
            b
            (case Data.Set.member y a of
              False -> y
-- todo: see veateade on kahtlane!
              True -> "an application type")
        True -> solvesys_rep h b c d (x, m, a, w)
  solvesys_rep ::
    (String -> String -> Err ([(String, (Name, Type_1))], Expression_2, Set String, [Cond_eqtns])) ->
    String ->
    Type_1 ->
    [(Type_1, Type_1)] ->
    ([(String, (Name, Type_1))], Expression_2, Set String, [Cond_eqtns]) ->
    Err ([(String, (Name, Type_1))], Expression_2, Set String, [Cond_eqtns])
  solvesys_rep a c d e (x, f, k, w) =
    let
      m = sysrep' c d
    in
      solvesys a (bimap m m <$> e) (second (second m) <$> x, sysrep2 c d f, Data.Set.delete c k, sysrep_cond c d <$> w)
  split_pattern :: (Map' Constructor, Map' [String]) -> Pattern_5 -> Alg_pat_3 -> ([(Pattern_5, Bool)], Bool)
  split_pattern (constructors, structs) x y =
    case (x, y) of
      (Blank_pattern_5, Char_alg_pat_3 z) -> primitive_pattern_0 (Char_pattern_5, Char_blank_pattern_5) z
      (Blank_pattern_5, Int_alg_pat_3 z) -> primitive_pattern_0 (Int_pattern_5, Int_blank_pattern_5) z
      (Blank_pattern_5, Modular_alg_pat_3 (Modular' z a)) -> ((\b -> (Modular_pattern_5 b, b == a)) <$> [0 .. z - 1], True)
      (Blank_pattern_5, Struct_alg_pat_3 z a) ->
        let
          Constructor d _ = constructors ! z
        in
          (
            join
              (
                (\b ->
                  let
                    Constructor _ c = constructors ! b
                  in
                    case b == z of
                      False -> [(Struct_pattern_5 b (replicate (length c) Blank_pattern_5), False)]
                      True -> fst (struct_pattern (constructors, structs) z ((,) Blank_pattern_5 <$> a))) <$>
                structs ! d),
            True)
      (Char_blank_pattern_5 z, Char_alg_pat_3 a) -> primitive_pattern_2 (Char_pattern_5, Char_blank_pattern_5) z a
      (Char_pattern_5 z, Char_alg_pat_3 a) -> primitive_pattern_1 Char_pattern_5 z a
      (Int_blank_pattern_5 z, Int_alg_pat_3 a) -> primitive_pattern_2 (Int_pattern_5, Int_blank_pattern_5) z a
      (Int_pattern_5 z, Int_alg_pat_3 a) -> primitive_pattern_1 Int_pattern_5 z a
      (Modular_pattern_5 z, Modular_alg_pat_3 (Modular' _ a)) -> primitive_pattern_1 Modular_pattern_5 z a
      (Struct_pattern_5 z a, Struct_alg_pat_3 b c) ->
        case b == z of
          False -> ([(x, False)], False)
          True -> struct_pattern (constructors, structs) z (zip a c)
      (_, Blank_alg_pat_3) -> ([(x, True)], True)
      _ -> undefined
  standard_naming_typing ::
    (
      String ->
      Tree_0 ->
      (((Set String, Set String), Locations, Locations, Map' (Map' Location')), (File, Map' Op), Map' Expr_2) ->
      Err (((Set String, Set String), Locations, Locations, Map' (Map' Location')), (File, Map' Op), Map' Expr_2))
  standard_naming_typing f a (b, (c, t), g) =
    standard_1 (Location_1 f) t a >>= \(v, n') -> naming f n' b >>= \(d, e) -> (\(h, i) -> (d, (h, v), i)) <$> typing f e (c, g)
  struct_pattern :: (Map' Constructor, Map' [String]) -> String -> [(Pattern_5, Alg_pat_3)] -> ([(Pattern_5, Bool)], Bool)
  struct_pattern context x y =
    first
      (fmap (first (Struct_pattern_5 x)))
      (Prelude.foldr
        (\(z, a) -> \(b, c) -> (compose_patterns <$> z <*> b, a && c))
        ([([], True)], True)
        (uncurry (split_pattern context) <$> y))
  sysrep' :: String -> Type_1 -> Type_1 -> Type_1
  sysrep' a b c =
    let
      f = sysrep' a b
    in
      case c of
        Application_type_1 d e -> Application_type_1 (f d) (f e)
        Name_type_1 d -> if d == a then b else c
  sysrep_cond :: String -> Type_1 -> Cond_eqtns -> Cond_eqtns
  sysrep_cond a b (Cond_eqtns c d e f) =
    let
      g h i = sysrep_eqs a c h b i
    in
      Cond_eqtns c (g (Name_type_1 "Zero") d) e (g (Application_type_1 (Name_type_1 "Next") (Name_type_1 e)) f)
  sysrep_eqs :: String -> String -> Type_1 -> Type_1 -> Eqtns -> Eqtns
  sysrep_eqs a b h i (Eqtns c d e f) =
    let
      g = sysrep' b h i
      j = sysrep' a g
    in
      Eqtns c (bimap j j <$> d) (second (second j) <$> e) (sysrep_cond a g <$> f)
  sysrep2 :: String -> Type_1 -> Expression_2 -> Expression_2
  sysrep2 a b c =
    let
      f = sysrep2 a b
      h = sysrep' a b
    in
      case c of
        Application_expression_2 d e -> Application_expression_2 (f d) (f e)
        Function_expression_2 d e -> Function_expression_2 d (f e)
        Glob_expression_2 d e g -> Glob_expression_2 d (h <$> e) (h <$> g)
        Match_expression_2 d e -> Match_expression_2 (f d) ((\(Case_3 g i) -> Case_3 g (f i)) <$> e)
        _ -> c
  type_br_0 :: Data_br_1 -> [(String, Expression_2)]
  type_br_0 (Data_br_1 a b) = 
    let
      c = fst <$> b
    in
      (
        (
          a,
          Prelude.foldr
            (\d -> Function_expression_2 (Name_pat_1 ('!' : d)))
            (Algebraic_expression_2 a ((\d -> Loc_expression_2 ('!' : d)) <$> c))
            c) :
        type_brs_0 0 c)
  type_branching :: (Location_0 -> Location_1) -> Map' Kind_0 -> Type_1 -> Data_br_1 -> Err [(String, Type_1)]
  type_branching a b c (Data_br_1 d e) =
    (\f -> (d, Prelude.foldr function_type c (snd <$> f)) : (second (function_type c) <$> f)) <$> type_fields a e b
  type_brs_0 :: Integer -> [String] -> [(String, Expression_2)]
  type_brs_0 a b =
    case b of
      [] -> []
      c : d -> (c, Field_expression_2 a) : type_brs_0 (a + 1) d
  type_case ::
    (
      Map' Alg ->
      Map' Constructor ->
      (Location_0 -> Location_1) ->
      Integer ->
      Eqtns ->
      Map' (Type_2, Globloc) ->
      Case_2 ->
      Integer ->
      Type_1 ->
      Map' Kind_0 ->
      Err (Case_3, Eqtns, Integer, [(Location_0, [Alg_pat_3])], Alg_pat_3))
  type_case a b c d (Eqtns e p q r) f (Case_2 g h) i j k =
    (
      get_pattern_type c a b (d, e, p, f) g (Name_type_1 (show i)) >>=
      \((m, n, s, t), (o, y, w')) ->
        (
          (\(u, v, w, r0) -> (Case_3 o u, v, w, r0, y)) <$>
          type_expression
            a
            b
            c
            m
            (Eqtns n s q r)
            t
            (Prelude.foldl
              (\t0 -> \t1 -> Application_expression_1 t0 (Name_expression_1 (Name (Location_0 0 0) t1) Nothing []))
              h
              (case w' of
                Nothing -> []
                Just (_, w0) -> w0))
            j
            k))
  type_cases ::
    (
      Map' Alg ->
      Map' Constructor ->
      (Location_0 -> Location_1) ->
      Integer ->
      Eqtns ->
      Map' (Type_2, Globloc) ->
      [Case_2] ->
      Integer ->
      Type_1 ->
      Map' Kind_0 ->
      Err ([Case_3], Eqtns, Integer, [(Location_0, [Alg_pat_3])], [Alg_pat_3]))
  type_cases a b c d e f g n h i =
    case g of
      [] -> Right ([], e, d, [], [])
      l : m ->
        (
          type_case a b c d e f l n h i >>=
          \(o, p, q, r0, r') -> (\(r, s, t, r1, w) -> (o : r, s, t, r0 ++ r1, r' : w)) <$> type_cases a b c q p f m n h i)
  type_class_0 ::
    (
      (Location_0 -> Location_1) ->
      Map' Kind_0 ->
      Class_2 ->
      (Map' (Kind_0, Status), Map' (Class_5, Status), Map' String) ->
      Err
      (Class_3, (Map' (Kind_0, Status), Map' (Class_5, Status), Map' String)))
  type_class_0 a j (Class_2 b (c, d) g' e) (i', j0, x2) =
    let
      g3 = (\(Name _ t4) -> t4) <$> g'
    in
      (
        type_inh b [b] g3 x2 *>
        (
          (\g ->
            let
              g2 = (\(Method_3 w1 _ _ _) -> w1) <$> g
            in
              (
                Class_3 b (c, d) g' g,
                (
                  ins_new b d i',
                  ins_new b (Class_5 d g3 g2) j0,
                  case g' of
                    Just (Name _ t0) -> Data.Map.insert b t0 x2
                    Nothing -> x2))) <$>
          type_methods_0 a e (Data.Map.insert c d j)))
  type_class_1 ::
    String ->
    Class_3 ->
    Map' Kind_0 ->
    Map' Class_5 ->
    (Map' (Type_2, Status), Map' (Class_4, Status)) ->
    Err (Map' (Type_2, Status), Map' (Class_4, Status))
  type_class_1 a (Class_3 b (c, k) g' e) d f1 (f0, f) =
    let
      x1 = Constraint_1 b c
      l m =
        (
          (\e' ->
            (
              Prelude.foldl
                (\x -> \(Method_4 t s u0 u) -> ins_new t (Basic_type_1 ((c, k) : s) (Just x1) (x1 : u0) u) x)
                f0
                e',
              ins_new b (Class_4 (c, k) m e') f)) <$>
          type_methods_1 a f1 e)
    in
      case g' of
        Just (Name m g) ->
          und_err g d "class" (Location_1 a m) (\h -> if h == k then l (Just g) else kind_err (Location_1 a m))
        Nothing -> l Nothing
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
      String ->
      Map' Kind_0 ->
      [Class_2] ->
      (Map' (Class_4, Status), Map' (Type_2, Status), Map' (Kind_0, Status), Map' (Class_5, Status)) ->
      Err (Map' (Class_4, Status), Map' (Type_2, Status), Map' (Kind_0, Status), Map' (Class_5, Status)))
  type_classes a c d (e, g, o, o') =
    (
      type_classes_0 (Location_1 a) c d (o, o', Data.Map.empty) >>=
      \(r, (p, p', _)) -> (\(k, n) -> (n, k, p, p')) <$> type_classes_1 a r (fst <$> p) (fst <$> p') (g, e))
  type_classes_0 ::
    (
      (Location_0 -> Location_1) ->
      Map' Kind_0 ->
      [Class_2] ->
      (Map' (Kind_0, Status), Map' (Class_5, Status), Map' String) ->
      Err ([Class_3], (Map' (Kind_0, Status), Map' (Class_5, Status), Map' String)))
  type_classes_0 a g b c =
    case b of
      [] -> Right ([], c)
      d : e -> type_class_0 a g d c >>= \(h, i) -> first ((:) h) <$> type_classes_0 a g e i
  type_classes_1 ::
    String ->
    [Class_3] ->
    Map' Kind_0 ->
    Map' Class_5 ->
    (Map' (Type_2, Status), Map' (Class_4, Status)) ->
    Err (Map' (Type_2, Status), Map' (Class_4, Status))
  type_classes_1 a b h i c =
    case b of
      [] -> Right c
      d : e -> type_class_1 a d h i c >>= type_classes_1 a e h i
  type_cls_0 ::
    String ->
    [Method_4] ->
    Type_1 ->
    [(Name, Expression_1)] ->
    (Location_0 -> Location_1) ->
    String ->
    Location_0 ->
    Err [(Name, Expression_1, [(String, Kind_0)], [Constraint_1], Type_1)]
  type_cls_0 a b c d l m n =
    case d of
      [] ->
        case b of
          [] -> Right []
          (Method_4 e _ _ _) : _ -> Left ("Missing definition " ++ e ++ " in " ++ m ++ " " ++ a ++ location' (l n))
      (p' @ (Name h i), j) : k ->
        let
          o p = Left ("Definition " ++ i ++ location (l h) ++ " is not a component of class " ++ m ++ p)
        in
          case b of
            [] -> o "."
            (Method_4 e s y f) : g ->
              if i == e
                then (:) (p', j, s, y, f) <$> type_cls_0 a g c k l m n
                else o " or the definitions are in a wrong order."
  type_constraint_0 ::
    Map' (Map' [String]) ->
    Constraint_0 ->
    (Map' Class_5, Map' Kind_0) ->
    String ->
    Err (Map' (Map' [String]))
  type_constraint_0 k (Constraint_0 (Name b c) (Name d e)) (f, g) j =
    und_err
      c
      f
      "class"
      (Location_1 j b)
      (\(Class_5 h y h') ->
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
                      (Data.Map.insert
                        e
                        ((\t -> t ++ " " ++ e) <$> h')
                        (case Data.Map.lookup c k of
                          Just l -> l
                          Nothing -> Data.Map.empty))
                      k)
                    e)
              else
                Left
                  (
                    "Kind mismatch in constraint" ++
                    location (Location_1 j b) ++
                    " between class " ++
                    c ++
                    " and type variable " ++
                    e ++
                    ".")
          Nothing -> Left ("Undefined type variable " ++ e ++ location' (Location_1 j d)))
  type_constraint_1 :: Constraint_1 -> Map' (Map' [[String]]) -> Map' Class_4 -> Map' (Map' [[String]])
  type_constraint_1 (Constraint_1 c e) a b =
    let
      d =
        Data.Map.insert
          c
          (Data.Map.insert
            e
            []
            (case Data.Map.lookup c a of
              Just l -> l
              Nothing -> Data.Map.empty))
          a
      Class_4 _ f _ = b ! c
    in
      case f of
        Just g -> type_constraint_1 (Constraint_1 g e) d b
        Nothing -> d
  type_constraints_0 ::
    Map' (Map' [String]) -> [Constraint_0] -> (Map' Class_5, Map' Kind_0, Map' Nat) -> String -> Err [Constraint_1]
  type_constraints_0 g a (f, t, u) h =
    case a of
      [] ->
        let
          l y = join (y <$> assocs (keys <$> g))
        in
          Right (l (\(i, j) -> Constraint_1 i <$> j))
      b : c -> type_constraint_0 g b (f, t) h >>= \d -> type_constraints_0 d c (f, t, u) h
  type_constraints_1 :: [Constraint_1] -> Map' (Map' [[String]]) -> Map' Class_4 -> Map' (Map' [[String]])
  type_constraints_1 a e d =
    case a of
      [] -> e
      b : c -> type_constraints_1 c (type_constraint_1 b e d) d
  type_data_1 :: Data_2 -> (Map' (Kind_0, Status), Map' Expr_2) -> (Map' (Kind_0, Status), Map' Expr_2)
  type_data_1 (Data_2 b c d) (e, g) =
    let
      l m o =
        (
          ins_new b (Prelude.foldr Arrow_kind_0 Star_kind_0 (m ++ (snd <$> c))) e,
          Prelude.foldl (\h -> \(p, q) -> Data.Map.insert p q h) g o)
      s o = l [] (second (Expr_2 Nothing (fst <$> c)) <$> o)
    in
      case d of
        Algebraic_data_2 i ->
          s
            (
              (\(Form_1 m n) ->
                let
                  t = show <$> [0 .. length n - 1]
                in
                  (
                    m,
                    Prelude.foldr
                      (\r -> Function_expression_2 (Name_pat_1 r))
                      (Algebraic_expression_2 m (Loc_expression_2 <$> t))
                      t)) <$>
              i)
        Branching_data_2 i _ k ->
          l
            [Nat_kind_0]
            (
              (second (Expr_2 Nothing (fst <$> c)) <$> type_br_0 i) ++
              (second (Expr_2 Nothing ("!" : (fst <$> c))) <$> type_br_0 k))
        Struct_data_2 i -> s (type_br_0 (Data_br_1 b i))
  type_data_2 ::
    (
      (Location_0 -> Location_1) ->
      Data_2 ->
      Map' Kind_0 ->
      (Algebraics, Types, Map' (Constructor, Status)) ->
      Err (Algebraics, Types, Map' (Constructor, Status)))
  type_data_2 a (Data_2 g h b) c (d, e, m3) =
    let
      l = type_kinds h c
      x' = Prelude.foldl (\n -> \n' -> Application_type_1 n n') (Name_type_1 g)
      y = ((\(v, _) -> Name_type_1 v) <$> h)
      x = x' y
      x2 q = x' (q : y)
      t' = Basic_type_1 h Nothing []
    in
      case b of
        Algebraic_data_2 i ->
          (
            (\q ->
              (
                ins_new g (Alg h x ((\(Form_2 u _) -> u) <$> q)) d,
                Prelude.foldl (\w' -> \(Form_2 u m) -> ins_new u (t' (Prelude.foldr function_type x m)) w') e q,
                Prelude.foldl (\w' -> \(Form_2 u m) -> ins_new u (Constructor g m) w') m3 q)) <$>
            type_forms a i l)
        Branching_data_2 (Data_br_1 i i') j (Data_br_1 k k') ->
          let
            s0 = x2 (Name_type_1 "Zero")
            t0 = x2 (Application_type_1 (Name_type_1 "Next") (Name_type_1 j))
          in
            (
              (\s -> \t ->
                case (s, t) of
                  (_ : s1, _ : t1) ->
                    (
                      ins_new (g ++ " Next") (Alg (("!", Nat_kind_0) : h) t0 [k]) (ins_new (g ++ " Zero") (Alg h s0 [i]) d),
                      Prelude.foldl
                        (\w' -> \(m, u) -> ins_new m u w')
                        e
                        ((second t' <$> s) ++ (second (Basic_type_1 ((j, Nat_kind_0) : h) Nothing []) <$> t)),
                      ins_new
                        k
                        (Constructor (g ++ " Next") (snd <$> t1))
                        (ins_new i (Constructor (g ++ " Zero") (snd <$> s1)) m3))
                  _ -> undefined) <$>
              type_branching a l s0 (Data_br_1 i i') <*>
              type_branching a (Data.Map.insert j Nat_kind_0 l) t0 (Data_br_1 k k'))
        Struct_data_2 i ->
          (
            (\u ->
              case u of
                _ : u' ->
                  (
                    ins_new g (Alg h x [g]) d,
                    Prelude.foldl (\w' -> \(k, r) -> ins_new k (t' r) w') e u,
                    ins_new g (Constructor g (snd <$> u')) m3)
                _ -> undefined) <$>
            type_branching a l x (Data_br_1 g i))
  type_datas ::
    (
      (Location_0 -> Location_1) ->
      [Data_2] ->
      (Map' (Kind_0, Status), Algebraics, Map' (Constructor, Status), Types, Map' Expr_2) ->
      Err (Map' (Kind_0, Status), Algebraics, Map' (Constructor, Status), Types, Map' Expr_2))
  type_datas h a (b, i, j, d, c) =
    let
      (u, w) = type_datas_1 a (b, c)
    in
      (\(b', c', v) -> (u, b', v, c', w)) <$> type_datas_2 h a (fst <$> u) (i, d, j)
  type_datas_1 :: [Data_2] -> (Map' (Kind_0, Status), Map' Expr_2) -> (Map' (Kind_0, Status), Map' Expr_2)
  type_datas_1 b c = Prelude.foldl (\a -> \d -> type_data_1 d a) c b
  type_datas_2 ::
    (
      (Location_0 -> Location_1) ->
      [Data_2] ->
      Map' Kind_0 ->
      (Algebraics, Types, Map' (Constructor, Status)) ->
      Err (Algebraics, Types, Map' (Constructor, Status)))
  type_datas_2 f a b c =
    case a of
      [] -> Right c
      d : e -> type_data_2 f d b c >>= type_datas_2 f e b
  type_def_1 ::
    (
      String ->
      Def_3 ->
      Map' Kind_0 ->
      Map' (Type_2, Status) ->
      Map' Class_4 ->
      Map' Class_5 ->
      Map' (Map' ([[String]], Status)) ->
      Err (Def_4, Map' (Type_2, Status), Map' (Map' ([[String]], Status))))
  type_def_1 l a b c k k2 t' =
    case a of
      Basic_def_3 f d e e' g i ->
        let
          j' = type_kinds e Data.Map.empty
        in
          (
            type_constraints_0 Data.Map.empty e' (k2, j', (\_ -> Zr) <$> j') l >>=
            \o1 ->
              (
                (\h -> (Basic_def_4 f d e o1 h i, ins_new d (Basic_type_1 e Nothing o1 h) c, t')) <$>
                type_typ (Location_1 l) g (type_kinds e b) Star_kind_0))
      Instance_3 d (Name e m) (Name f n) k' o' g ->
        und_err
          m
          k
          "class"
          (Location_1 l e)
          (\(Class_4 (o, p) w0 q) ->
            und_err
              n
              b
              "type"
              (Location_1 l f)
              (\s ->
                (
                  type_class_args s k' (kind_err (Location_1 l f)) p 0 (Name_type_1 n) Data.Map.empty Data.Map.empty Zr >>=
                  \(q', p', s', t0, t7) ->
                    (
                      type_constraints_0 Data.Map.empty o' (k2, t0, t7) l >>=
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
                            type_cls_0 n q s' g (Location_1 l) m d)))))
  type_def_2 ::
    (
      (Location_0 -> Location_1) ->
      Def_4 ->
      (Map' Alg, Map' Constructor, Map' Type_2) ->
      Map' Expr_2 ->
      Map' (Map' [[String]]) ->
      Map' Kind_0 ->
      Map' Class_4 ->
      Err (Map' Expr_2))
  type_def_2 j a (d, l, k) c m n u0 =
    case a of
      Basic_def_4 r e b x h i ->
        (
          (\t -> Data.Map.insert e (Expr_2 Nothing (fst <$> b) t) c) <$>
          type_expr
            ("definition " ++ e ++ location' (j r))
            h
            j
            (d, l, k)
            i
            (type_constraints_1 x m u0)
            0
            (Prelude.foldl (\y -> \(z, w) -> Data.Map.insert z w y) n b))
      Instance_4 l' e' w0 w e e0 e1 f f' g' r' ->
        let
          f4 = Prelude.foldl (\x -> \(y, g) -> Data.Map.insert y g x) n e0
          r =
            type_exprs
              (\(Name x g) -> "definition " ++ g ++ " " ++ e ++ location' (j x))
              j
              (d, l, k)
              (type_constraints_1 g' m u0)
              f
              c
              e
              (sysrep' w f')
              e1
              u0
              f4
              (fst <$> e0)
          s' w1 = Left (e' ++ " " ++ e ++ " at" ++ location (j l') ++ " is an illegal instance because " ++ w1 ++ ".")
        in
          case w0 of
            Just q ->
              let
                s = s' (e' ++ " assumes " ++ q)
              in
                case Data.Map.lookup q m of
                  Just t ->
                    case Data.Map.lookup e t of
                      Just u ->
                        case constr_check ((\(Class_4 _ q0 _) -> q0) <$> u0) (fst <$> e0) u r' of
                          Just r0 -> s' ("it requires " ++ r0 ++ " due to constraints on " ++ q ++ " " ++ e)
                          Nothing -> r
                      Nothing -> s
                  Nothing -> s
            Nothing -> r
  type_defs ::
    (
      String ->
      [Def_3] ->
      [Name] ->
      (Map' Kind_0, Map' Alg, Map' Constructor) ->
      (Map' Expr_2, Types) ->
      Map' Class_4 ->
      Map' Class_5 ->
      Map' (Map' ([[String]], Status)) ->
      Err (Map' Expr_2, Types, Map' (Map' ([[String]], Status))))
  type_defs h a a2 (b, i, j) (c, d) y y0 t =
    (
      type_defs_1 h a b d y y0 t >>=
      \(g, e, u) ->
        (
          (\f -> (f, e, u)) <$
          type_ops h (fst <$> e) a2 <*>
          type_defs_2 (Location_1 h) g (i, j, fst <$> e) c ((<$>) fst <$> u) b y))
  type_defs_1 ::
    String ->
    [Def_3] ->
    Map' Kind_0 ->
    Types ->
    Map' Class_4 ->
    Map' Class_5 ->
    Map' (Map' ([[String]], Status)) ->
    Err ([Def_4], Types, Map' (Map' ([[String]], Status)))
  type_defs_1 h a b c y y0 u =
    case a of
      [] -> Right ([], c, u)
      d : e -> type_def_1 h d b c y y0 u >>= \(f, g, u') -> (\(k, l, t') -> (f : k, l, t')) <$> type_defs_1 h e b g y y0 u'
  type_defs_2 ::
    (Location_0 -> Location_1) ->
    [Def_4] ->
    (Map' Alg, Map' Constructor, Map' Type_2) ->
    Map' Expr_2 ->
    Map' (Map' [[String]]) ->
    Map' Kind_0 ->
    Map' Class_4 ->
    Err (Map' Expr_2)
  type_defs_2 f a b c g i u =
    case a of
      [] -> Right c
      d : e -> type_def_2 f d b c g i u >>= \h -> type_defs_2 f e b h g i u
  type_eqs ::
    (
      (Location_0 -> Location_1) ->
      Integer ->
      Type_5 ->
      Map' Kind_0 ->
      Kind_1 ->
      [(Kind_1, Kind_1)] ->
      Err (Integer, [(Kind_1, Kind_1)], Type_1))
  type_eqs m i a d k f =
    case a of
      Application_type_5 b c ->
        (
          type_eqs m (i + 1) b d (Arrow_kind_1 (Var_kind_1 i) k) f >>=
          \(i', f', b') -> (\(i2, f2, c') -> (i2, f2, Application_type_1 b' c')) <$> type_eqs m i' c d (Var_kind_1 i) f')
      Name_type_5 (Name l b) -> und_err b d "type" (m l) (\q -> Right (i, (k, kind_0_to_1 q) : f, Name_type_1 b))
  type_expr ::
    String ->
    Type_1 ->
    (Location_0 -> Location_1) ->
    (Map' Alg, Map' Constructor, Map' Type_2) ->
    Expression_1 ->
    Map' (Map' [[String]]) ->
    Integer ->
    Map' Kind_0 ->
    Err Expression_2
  type_expr k h a (c, d, e) f m w b =
    (
      type_expression c d a w (Eqtns Data.Set.empty [] [] []) ((\e' -> (e', Glob)) <$> e) f h b >>=
      \(g, i, _, z) -> solve_all a m (" in " ++ k) i g <* pats a (d, (\(Alg _ _ e') -> e') <$> c) z)
  type_expr' ::
    (Map' Kind_0, Map' Alg, Map' Constructor, Map' Type_2) -> Expression_1 -> Map' (Map' [[String]]) -> Err Expression_2
  type_expr' (b, c, d, e) f g =
    type_expr
      "input."
      (list_type char_type)
      (Location_1 "input")
      (c, d, e)
      (Application_expression_1
        (Name_expression_1 (Name (Location_0 0 0) "First") Nothing [])
        (Application_expression_1 (Name_expression_1 (Name (Location_0 0 0) "Write_Brackets") Nothing []) f))
      g
      0
      b
  type_expression ::
    Map' Alg ->
    Map' Constructor ->
    (Location_0 -> Location_1) ->
    Integer ->
    Eqtns ->
    Map' (Type_2, Globloc) ->
    Expression_1 ->
    Type_1 ->
    Map' Kind_0 ->
    Err (Expression_2, Eqtns, Integer, [(Location_0, [Alg_pat_3])])
  type_expression v w r o (Eqtns f h c' h9) d b e r7 =
    let
      x' a = location' (r a)
    in
      case b of
        Application_expression_1 c g ->
          (
            type_expression
              v
              w
              r
              (o + 1)
              (Eqtns (Data.Set.insert (show o) f) h c' h9)
              d
              c
              (function_type (Name_type_1 (show o)) e)
              r7 >>=
            \(i, k, p, p2) ->
              (
                (\(l, m, n, n2) -> (Application_expression_2 i l, m, n, p2 ++ n2)) <$>
                type_expression v w r p k d g (Name_type_1 (show o)) r7))
        Branch_expression_1 (Name a j) c g i ->
          let
            r5 = Data.Map.delete j r7
          in
            und_err
              j
              (Data.Map.delete "Zero" r7)
              "type variable"
              (r a)
              (\k ->
                case k of
                  Nat_kind_0 ->
                    (
                      type_expression v w r o (Eqtns Data.Set.empty [] [] []) d c (sysrep' j (Name_type_1 "Zero") e) r5 >>=
                      \(i', j', p, p3) ->
                        (
                          (\(a6, b6, c6, p4) ->
                            (
                              Branch_expression_2 (Name_type_1 j) i' g a6,
                              Eqtns f h c' (Cond_eqtns j j' g b6 : h9),
                              c6,
                              p3 ++ p4)) <$>
                          type_expression
                            v
                            w
                            r
                            p
                            (Eqtns Data.Set.empty [] [] [])
                            d
                            i
                            (sysrep' j (Application_type_1 (Name_type_1 "Next") (Name_type_1 g)) e)
                            (Data.Map.insert g Nat_kind_0 r5)))
                  _ -> Left ("Type " ++ j ++ location (r a) ++ " is not of kind Nat."))
        Char_expression_1 c -> Right (Char_expression_2 c, Eqtns f ((e, char_type) : h) c' h9, o, [])
        Function_expression_1 c g ->
          (
            type_pat r (v, w) c (Name_type_1 (show o)) d (o + 1) (Data.Set.insert (show o) f) h >>=
            \(a6, b6, c6, d6, f6, m8) ->
              (
                (\(a', b', d', d2) -> (Function_expression_2 a6 a', b', d', d2)) <$>
                type_expression
                  v
                  w
                  r
                  (c6 + 1)
                  (Eqtns
                    (Data.Set.insert (show c6) d6)
                    ((e, function_type (Name_type_1 (show o)) (Name_type_1 (show c6))) : f6)
                    c'
                    h9)
                  b6
                  (Prelude.foldl
                    (\t1 -> \t2 -> Application_expression_1 t1 (Name_expression_1 (Name (Location_0 0 0) t2) Nothing []))
                    g
                    (case m8 of
                      Nothing -> []
                      Just (_, m7) -> m7))
                  (Name_type_1 (show c6))
                  r7))
        Int_expression_1 c -> Right (Int_expression_2 c, Eqtns f ((e, int_type) : h) c' h9, o, [])
        Match_expression_1 a7 c g ->
          (
            type_expression v w r (o + 1) (Eqtns (Data.Set.insert (show o) f) h c' h9) d c (Name_type_1 (show o)) r7 >>=
            \(k, m, n, n2) ->
              (
                (\(q, u, x, n3, n4) -> (Match_expression_2 k q, u, x, n2 ++ [(a7, n4)] ++ n3)) <$>
                type_cases v w r n m d g o e r7))
        Modular_expression_1 c ->
          (\(Modular' _ g1, g3) -> (Modular_expression_2 g1, Eqtns f ((e, g3) : h) c' h9, o, [])) <$> check_mod r c
        Name_expression_1 (Name a7 c) g k ->
          let
            e5 a' = Left ("Variable " ++ c ++ x' a7 ++ " has been given too " ++ a' ++ " type arguments.")
          in
            und_err
              c
              d
              "variable"
              (r a7)
              (\(Basic_type_1 i x0 a' j, b3) ->
                let
                  g7 k2 d3 e4 s' r' =
                    (
                      (\((s, p, n), n9) ->
                        (
                          k2 n9,
                          Eqtns
                            n
                            ((e, repl' p j) : h)
                            (((\(Constraint_1 a0 b0) -> (a0, (Name a7 c, p ! b0))) <$> a') ++ c')
                            h9,
                          s,
                          [])) <$>
                      case k of
                        [] -> Right (typevars d3 (s', e4, r'))
                        _ -> first (\f9 -> (s', f9, r')) <$> typevars' r r7 e5 d3 k e4)
                in
                  case g of
                    Just t3 ->
                      case x0 of
                        Just (Constraint_1 _ _) ->
                          case i of
                            [] -> undefined
                            (d5, k3) : d' ->
                              (
                                type_typ r t3 r7 k3 >>=
                                \t7 -> g7 (Glob_expression_2 c (Just t7)) d' (Data.Map.singleton d5 t7) o f)
                        Nothing -> Left ("Invalid instance argument for variable " ++ c ++ x' a7)
                    Nothing ->
                      case x0 of
                        Just (Constraint_1 _ _) ->
                          case i of
                            [] -> undefined
                            (d5, _) : d' ->
                              g7
                                (Glob_expression_2 c (Just (Name_type_1 (show o))))
                                d'
                                (Data.Map.singleton d5 (Name_type_1 (show o)))
                                (o + 1)
                                (Data.Set.insert (show o) f)
                        Nothing ->
                          g7
                            (\t9 ->
                              case b3 of
                                Glob -> Glob_expression_2 c Nothing t9
                                Loc -> Loc_expression_2 c)
                            i
                            Data.Map.empty
                            o
                            f)
  type_exprs ::
    (
      (Name -> String) ->
      (Location_0 -> Location_1) ->
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
      Err (Map' Expr_2))
  type_exprs a b c d h i t z w t0 x2 f' =
    case h of
      [] -> Right i
      (j @ (Name _ y), k, s, t5, l) : m ->
        (
          type_expr
            (a j)
            (z l)
            b
            c
            k
            (type_constraints_1 t5 d t0)
            w
            (Prelude.foldl (\k' -> \(l', m0) -> Data.Map.insert l' m0 k') x2 s) >>=
          \g -> type_exprs a b c d m (Data.Map.insert (y ++ " " ++ t) (Expr_2 (Just f') (fst <$> s) g) i) t z w t0 x2 f')
  type_field :: (Location_0 -> Location_1) -> (String, Type_8) -> Map' Kind_0 -> Err (String, Type_1)
  type_field d (a, b) c  = (,) a <$> type_typ d b c Star_kind_0
  type_fields :: (Location_0 -> Location_1) -> [(String, Type_8)] -> Map' Kind_0 -> Err [(String, Type_1)]
  type_fields f a b =
    case a of
      [] -> Right []
      c : d -> type_field f c b >>= \e -> (:) e <$> type_fields f d b
  type_form :: (Location_0 -> Location_1) -> Form_1 -> Map' Kind_0 -> Err Form_2
  type_form d (Form_1 a b) c = Form_2 a <$> type_types d b c
  type_forms :: (Location_0 -> Location_1) -> [Form_1] -> Map' Kind_0 -> Err [Form_2]
  type_forms f a b =
    case a of
      [] -> Right []
      c : d -> type_form f c b >>= \e -> (:) e <$> type_forms f d b
  type_inh :: String -> [String] -> Maybe String -> Map' String -> Either String ()
  type_inh a b c d =
    case c of
      Just e ->
        if e == a
          then Left ("Circular dependency between classes [" ++ intercalate ", " b ++ "].")
          else type_inh a (e : b) (Data.Map.lookup e d) d
      Nothing -> Right ()
  type_kinds :: [(String, Kind_0)] -> Map' Kind_0 -> Map' Kind_0
  type_kinds c d =
    case c of
      [] -> d
      (e, f) : g -> type_kinds g (Data.Map.insert e f d)
  type_method :: (Location_0 -> Location_1) -> Method_2 -> Map' Kind_0 -> Err Method_3
  type_method a (Method_2 b c i d) e = Method_3 b c i <$> type_typ a d (type_kinds c e) Star_kind_0
  type_method_1 :: String -> Map' Class_5 -> Method_3 -> Err Method_4
  type_method_1 e g (Method_3 a b c d) =
    let
      m = Prelude.foldl (\h -> \(i, j) -> Data.Map.insert i j h) Data.Map.empty b
    in
      (\f -> Method_4 a b f d) <$> type_constraints_0 Data.Map.empty c (g, m, (\_ -> Zr) <$> m) e
  type_methods_0 :: (Location_0 -> Location_1) -> [Method_2] -> Map' Kind_0 -> Err [Method_3]
  type_methods_0 a b c =
    case b of
      [] -> Right []
      e : g -> type_method a e c >>= \h -> (:) h <$> type_methods_0 a g c
  type_methods_1 :: String -> Map' Class_5 -> [Method_3] -> Err [Method_4]
  type_methods_1 e f a =
    case a of
      [] -> Right []
      b : c -> type_method_1 e f b >>= \d -> (:) d <$> type_methods_1 e f c
  type_ops :: String -> Map' Type_2 -> [Name] -> Err ()
  type_ops a b c =
    case c of
      [] -> Right ()
      Name d e : f ->
        und_err
          e
          b
          "function"
          (Location_1 a d)
          (\(Basic_type_1 _ _ _ g) ->
            case g of
              Application_type_1
                (Application_type_1 (Name_type_1 "Function") _)
                (Application_type_1 (Application_type_1 (Name_type_1 "Function") _) _) ->
                  type_ops a b f
              _ -> Left ("Function " ++ e ++ location (Location_1 a d) ++ " takes less than 2 arguments."))
  type_pat ::
    (
      (Location_0 -> Location_1) ->
      (Map' Alg, Map' Constructor) ->
      Pat ->
      Type_1 ->
      Map' (Type_2, Globloc) ->
      Integer ->
      Set String ->
      [(Type_1, Type_1)] ->
      Err (Pat_1, Map' (Type_2, Globloc), Integer, Set String, [(Type_1, Type_1)], Maybe (Name, [String])))
  type_pat k (h, h') (Pat g b) c d l n o =
    case b of
      Application_pat e f ->
        let
          Constructor f' g' = h' ! e
          Alg i j m = h ! f'
          ((p, q, r), _) = typevars i (l, Data.Map.empty, n)
        in
          case m of
            [_] ->
              (
                (\(s, t, u, v, w, x') -> (Application_pat_1 s, t, u, v, w, x')) <$>
                type_pats k (h, h') f (repl' q <$> g') d p r ((c, repl' q j) : o) (Name g e))
            _ -> Left ("Constructor " ++ e ++ location (k g) ++ " is not a struct constructor.")
      Blank_pat -> Right (Blank_pat_1, d, l, n, o, Nothing)
      Name_pat e -> Right (Name_pat_1 e, Data.Map.insert e (Basic_type_1 [] Nothing [] c, Loc) d, l, n, o, Nothing)
  type_pats ::
    (
      (Location_0 -> Location_1) ->
      (Map' Alg, Map' Constructor) ->
      [Pat] ->
      [Type_1] ->
      Map' (Type_2, Globloc) ->
      Integer ->
      Set String ->
      [(Type_1, Type_1)] ->
      Name ->
      Err ([Pat_1], Map' (Type_2, Globloc), Integer, Set String, [(Type_1, Type_1)], Maybe (Name, [String])))
  type_pats a b d e f g h i (Name x y) =
    case d of 
      [] ->
        let
          n = show <$> [0 .. length e - 1]
        in
          Right
            (
              Name_pat_1 <$> n,
              Prelude.foldl (\j -> \(k, m) -> Data.Map.insert k (Basic_type_1 [] Nothing [] m, Loc) j) f (zip n e),
              g,
              h,
              i,
              case e of
                [] -> Nothing
                _ -> Just (Name x y, n))
      j : k ->
        case e of
          [] -> Left ("Constructor " ++ y ++ location (a x) ++ " has been given too many arguments.")
          m : n ->
            (
              type_pat a b j m f g h i >>=
              \(c, o, p, q, r, r') ->
                case r' of
                  Nothing -> (\(s, t, u, v, w, a') -> (c : s, t, u, v, w, a')) <$> type_pats a b k n o p q r (Name x y)
                  Just (Name s t, _) -> Left ("Constructor " ++ t ++ location (a s) ++ " has been given too few arguments."))
  type_typ :: (Location_0 -> Location_1) -> Type_8 -> Map' Kind_0 -> Kind_0 -> Err Type_1
  type_typ a (Type_8 b c) d e = type_eqs a 0 c d (kind_0_to_1 e) [] >>= \(_, h, i) -> i <$ solve_type_eqs (a b) h
  type_types :: (Location_0 -> Location_1) -> [Type_8] -> Map' Kind_0 -> Err [Type_1]
  type_types f a b =
    case a of
      [] -> Right []
      c : d -> type_typ f c b Star_kind_0 >>= \e -> (:) e <$> type_types f d b
  type_types' :: (Location_0 -> Location_1) -> Map' Kind_0 -> [(String, Type_8)] -> Err [(String, Type_1)]
  type_types' a c d =
    case d of
      [] -> Right []
      (e, f) : g -> type_typ a f c Star_kind_0 >>= \h -> (:) (e, h) <$> type_types' a c g
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
                Basic_type_1 c Nothing [] (Prelude.foldr function_type d b)) <$>
          constrs) ++
        [
          (
            "Add",
            Basic_type_1
              [("T", Star_kind_0)]
              (Just (Constraint_1 "Ring" "T"))
              [Constraint_1 "Ring" "T"]
              (function_type (Name_type_1 "T") (function_type (Name_type_1 "T") (Name_type_1 "T")))),
          (
            "Compare",
            Basic_type_1
              [("T", Star_kind_0)]
              (Just (Constraint_1 "Ord" "T"))
              [Constraint_1 "Ord" "T"]
              (function_type (Name_type_1 "T") (function_type (Name_type_1 "T") comparison_type))),
          (
            "Convert",
            Basic_type_1
              [("T", Star_kind_0)]
              (Just (Constraint_1 "Ring" "T"))
              [Constraint_1 "Ring" "T"]
              (function_type int_type (Name_type_1 "T"))),
          ("Crash", Basic_type_1 [("T", Star_kind_0)] Nothing [] (Name_type_1 "T")),
          ("Div", Basic_type_1 [] Nothing [] (function_type int_type (function_type int_type (maybe_type int_type)))),
          (
            "Div'",
            Basic_type_1
              [("N", Nat_kind_0)]
              (Just (Constraint_1 "Nonzero" "N"))
              [Constraint_1 "Nonzero" "N"]
              (function_type int_type int_type)),
          (
            "First",
            Basic_type_1
              [("T", Star_kind_0), ("U", Star_kind_0)]
              Nothing
              []
              (function_type (pair_type (Name_type_1 "T") (Name_type_1 "U")) (Name_type_1 "T"))),
          (
            "Inverse",
            Basic_type_1
              [("T", Star_kind_0)]
              (Just (Constraint_1 "Field" "T"))
              [Constraint_1 "Field" "T", Constraint_1 "Ring" "T"]
              (function_type (Name_type_1 "T") (maybe_type (Name_type_1 "T")))),
          ("Mod", Basic_type_1 [] Nothing [] (function_type int_type (function_type int_type (maybe_type int_type)))),
          (
            "Multiply",
            Basic_type_1
              [("T", Star_kind_0)]
              (Just (Constraint_1 "Ring" "T"))
              [Constraint_1 "Ring" "T"]
              (function_type (Name_type_1 "T") (function_type (Name_type_1 "T") (Name_type_1 "T")))),
          (
            "Negate",
            Basic_type_1
              [("T", Star_kind_0)]
              (Just (Constraint_1 "Ring" "T"))
              [Constraint_1 "Ring" "T"]
              (function_type (Name_type_1 "T") (Name_type_1 "T"))),
          (
            "Second",
            Basic_type_1
              [("T", Star_kind_0), ("U", Star_kind_0)]
              Nothing
              []
              (function_type (pair_type (Name_type_1 "T") (Name_type_1 "U")) (Name_type_1 "U"))),
          (
            "Write_Brackets",
            Basic_type_1
              [("T", Star_kind_0)]
              (Just (Constraint_1 "Writeable" "T"))
              [Constraint_1 "Writeable" "T"]
              (function_type (Name_type_1 "T") (pair_type (list_type char_type) logical_type)))])
  typestring :: Type_1 -> [Type_1] -> (String, [Type_1])
  typestring a d =
    case a of
      Application_type_1 b c -> typestring b (c : d)
      Name_type_1 b -> (b, d)
  typevar :: String -> (Integer, Map' Type_1, Set String) -> ((Integer, Map' Type_1, Set String), Type_1)
  typevar a (c, e, f) =
    let
      d = show c
    in
      ((c + 1, Data.Map.insert a (Name_type_1 d) e, Data.Set.insert d f), Name_type_1 d)
  typevars :: [(String, Kind_0)] -> (Integer, Map' Type_1, Set String) -> ((Integer, Map' Type_1, Set String), [Type_1])
  typevars a b =
    case a of
      [] -> (b, [])
      (c, _) : d ->
        let
          (e, f) = typevar c b
        in
          second ((:) f) (typevars d e)
  typevars' ::
    (
      (Location_0 -> Location_1) ->
      Map' Kind_0 ->
      (String -> Err (Map' Type_1, [Type_1])) ->
      [(String, Kind_0)] ->
      [Type_8] ->
      Map' Type_1 ->
      Err (Map' Type_1, [Type_1]))
  typevars' l j a b c d =
    case b of
      [] ->
        case c of
          [] -> Right (d, [])
          _ -> a "many"
      (e, f) : g ->
        case c of
          [] -> a "few"
          h : i -> type_typ l h j f >>= \m -> second ((:) m) <$> typevars' l j a g i (Data.Map.insert e m d)
  typing :: String -> Tree_5 -> (File, Map' Expr_2) -> Err (File, Map' Expr_2)
  typing k (Tree_5 a a' x7 c) (File d t u v b' c5 x t2, l) =
    (
      type_datas (Location_1 k) a (old d, old t, old u, old v, l) >>=
      \(e, b, h, g, f) ->
        (
          type_classes k (fst <$> e) a' (old b', g, old t2, old c5) >>=
          \(c', g0, x2, t3) ->
            (
              (\(i, j, y) ->
                (
                  File (rem_old e) (rem_old b) (rem_old h) (rem_old j) (rem_old c') (rem_old t3) (rem_old' y) (rem_old x2),
                  i)) <$>
              type_defs
                k
                c
                x7
                (fst <$> e, fst <$> b, fst <$> h)
                (f, g0)
                (fst <$> c')
                (fst <$> t3)
                (old' x))))
  unsafe_left :: Either t u -> t
  unsafe_left a =
    case a of
      Left b -> b
      Right _ -> undefined
--------------------------------------------------------------------------------------------------------------------------------