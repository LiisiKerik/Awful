{-
protection against duplicate file loading - what happens now? if crashes - fix, give a nice error/warning. if nothing - warn?
tests
topelt-esindajate kontroll nimekontrolliga kokku? move duplicate instance control into Naming module?
internal: do something with old/new status tags. check where exactly they're necessary. get rid of them where they're useless
change semantics of missing pattern-match variables from blank to lambda? (Left -> e is not Left _ -> e but Left x -> e x)
internal: make the system of specifying built-in algebraic data types and things better and safer
Allow hiding things to functions outside module - so that helper functions are not exported from the module?
normalising constructors for some data types (polynomial, fraction) which assume a certain normal form of fields?
allow to hide (prevent exporting) constructors and field accessors which can potentially have bad behavior
switch expression that is less strict and more flexible than match?
syntactic sugar for lists, vectors, matrices... allow writing (tiny, limited to expression parsing) language extensions?
boolean function library
implement map and set (AVL trees?)
different ways of folding lists, vectors, sets, maps etc
module system related functions into a separate file?
make command line arguments nicer
remove promotion of primitives? it seems that without GADT-s they are pointless?
liigirakendamise eemaldamine liigituletuse kasuks (igal pool? teatud piiratud juhtudel?)
võimaldada suvalise arvu konstruktoritega algebralisi andmetüüpe. (LISAKS struktide allesjätmisele?)
todo: make a function writing operator/function. For printing stuff like "Complex (Fraction 0 1) (Fraction 1 1)"
checki abil võiks saada tüübikontrollida korraga mitut moodulit, andes ette nimekirja
ühildada Standard ja parser? või vastupidi, süntaktiline suhkur (listide sün.suhk.) standard moodulisse?
operaatorid struktuuride ja algebraliste andmetüüpide patternmatchides
teha midagi et kõrvaldada parserist aegunud keelelaienduse hoiatus
semantics of "Pair -> f" should be "Pair x y -> f x y"
make match expression more flexible (like case in Haskell)?
mis juhtub kui esimeses moodulis on kusagil tüübimuutuja T ja järgmises moodulis sama nimega globaalne tüüp?
Let f = Crash, x = f f In 0 -- tüüpimine läheb lõpmatusse tsüklisse sest puudub occur check
"./Awful eval "List (0)"" without importing Standard.awf - error message about Writeable class looks bad; fix
let expr de-sugaring (and therefore struct name collection) completely to Standard.hs module
all de-sugaring: remove from Tree.hs, put into Standard.hs
simplify parsing of match expression and remove duplicate code from de-sugaring, name checking, typechecking & eval
allow operators in pattern matching?
What happens with unary minus and binary minus during parsing?
allow using operators in class method definitions? Instance Ring{Complex T}<Ring T>(..., Complex x y * Complex z w = ...)
Match expression parsing has a bug. Match Foo{Int} vs Match x {False -> ...
check that struct pattern matching for branching types never occurs in worng place: Branch N {Zero -> Construct_Array x y -> ...
Ensure that <Nonzero N> constraint gets translated to N = Next N'.
syntactic sugar in pattern matching. List (x, y, z) -> ...    and Array (x, y, z) in function argument.
-}
-----------------------------------------------------------------------------------------------------------------------------
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
  data Alg = Alg [(String, Kind_0)] (Map' [Type_1]) Type_1 deriving Show -- TODO: REM STRINGS FROM FST MAP
  type Algebraics = Map' (Alg, Status)
  data Class_3 = Class_3 String (String, Kind_0) (Maybe Name) [Method_3] deriving Show
  data Class_4 = Class_4 (String, Kind_0) (Maybe String) [Method_4] deriving Show
  data Class_5 = Class_5 Kind_0 (Maybe String) [String] deriving Show
  data Cond_eqtns = Cond_eqtns String Eqtns String Eqtns deriving Show
  data Constraint_1 = Constraint_1 String String deriving Show
  type Constrs = Map' (String, Status)
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
    Branch_expression_2 String Expression_2 String Expression_2 |
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
    Field_expression_2 String |
    Function_expression_2 Pat_1 Expression_2 |
    Glob_expression_2 String (Maybe Type_1) [Type_1] |
    Int_expression_2 Integer |
    Inverse_Modular_expression_2 Integer |
    Loc_expression_2 String |
    Match_expression_2 Expression_2 Matches_2 |
    Mod_0_expression_2 |
    Mod_1_expression_2 Integer |
    Modular_expression_2 Integer |
    Multiply_Int_0_expression_2 |
    Multiply_Int_1_expression_2 Integer |
    Multiply_Modular_0_expression_2 Integer |
    Multiply_Modular_1_expression_2 Integer Integer |
    Negate_Int_expression_2 |
    Negate_Modular_expression_2 Integer |
    Struct_expression_2 (Map' Expression_2) |
    Write_Brackets_Int_expression_2 |
    Write_Brackets_Modular_expression_2 Integer
      deriving Show
  data File =
    File
      (Map' Kind_0)
      (Map' Alg)
      (Map' String)
      (Map' Type_2)
      (Map' Class_4)
      (Map' Class_5)
      (Map' (Map' [[String]]))
      (Map' Kind_0)
      (Map' Strct)
        deriving Show
  data Form_2 = Form_2 String [Type_1] deriving Show
  data Globloc = Glob | Loc deriving Show
  data Match_Algebraic_2 = Match_Algebraic_2 [Pat_1] Expression_2 deriving Show
  data Matches_2 =
    Matches_Algebraic_2 (Map' Match_Algebraic_2) (Maybe Expression_2) |
    Matches_char_2 (Map Char Expression_2) Expression_2 |
    Matches_Int_2 (Map Integer Expression_2) Expression_2 |
    Matches_Modular_2 (Map Integer Expression_2) (Maybe Expression_2)
      deriving Show
  data Method_3 = Method_3 String [(String, Kind_0)] [Constraint_0] Type_1 deriving Show
  data Method_4 = Method_4 String [(String, Kind_0)] [Constraint_1] Type_1 deriving Show
  data Modular' = Modular' Integer Integer deriving Show
  data Nat = Nxt Nat | Zr deriving (Eq, Ord, Show)
  data Pat_1 = Application_pat_1 [(String, Pat_1)] | Blank_pat_1 | Name_pat_1 String deriving Show
  data Strct = Strct [(String, Kind_0)] [(String, Type_1)] Type_1 deriving Show
  data Type_1 = Application_type_1 Type_1 Type_1 | Name_type_1 String deriving (Eq, Show)
  data Type_2 = Basic_type_1 [(String, Kind_0)] (Maybe Constraint_1) [Constraint_1] Type_1 deriving Show
  type Types = Map' (Type_2, Status)
  algebraics :: Map' Alg
  algebraics =
    Data.Map.fromList
      [
        ("Comparison", Alg [] (Data.Map.fromList [("EQ", []), ("GT", []), ("LT", [])]) comparison_type),
        (
          "Either",
          Alg
            [("T", Star_kind_0), ("U", Star_kind_0)]
            (Data.Map.fromList [("Left", [Name_type_1 "T"]), ("Right", [Name_type_1 "U"])])
            (either_type (Name_type_1 "T") (Name_type_1 "U"))),
        (
          "List",
          Alg
            [("T", Star_kind_0)]
            (Data.Map.fromList [("Construct_List", [Name_type_1 "T", list_type (Name_type_1 "T")]), ("Empty_List", [])])
            (list_type (Name_type_1 "T"))),
        ("Logical", Alg [] (Data.Map.fromList [("False", []), ("True", [])]) logical_type),
        (
          "Maybe",
          Alg
            [("T", Star_kind_0)]
            (Data.Map.fromList [("Nothing", []), ("Wrap", [Name_type_1 "T"])])
            (maybe_type (Name_type_1 "T")))]
  chain_constraints :: Maybe String -> Map' Class_5 -> Map' (Map' [String]) -> String -> Map' (Map' [String])
  chain_constraints a b c e =
    case a of
      Just d ->
        let
          Class_5 _ f o = unsafe_lookup d b
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
                  case unsafe_lookup d a of
                    Left e -> e
                    Right e -> e)
  check_mod :: (Location_0 -> Location_1) -> Modular -> Err Modular'
  check_mod a (Modular d b c) =
    if c < b then Right (Modular' b c) else Left ("Invalid Modular " ++ show_mod b c ++ location' (a d))
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
        case unsafe_lookup y m of
          Just y' -> constr_check_3 m x y'
          Nothing -> False
  constrs :: Map' String
  constrs =
    Data.Map.fromList
      [
        ("Construct_List", "List"),
        ("EQ", "Comparison"),
        ("Empty_List", "List"),
        ("False", "Logical"),
        ("GT", "Comparison"),
        ("LT", "Comparison"),
        ("Left", "Either"),
        ("Nothing", "Maybe"),
        ("Right", "Either"),
        ("True", "Logical"),
        ("Wrap", "Maybe")]
  context_union :: (File, Map' Op) -> (File, Map' Op) -> (File, Map' Op)
  context_union (File b i j d e q t g d', t0) (File f k l h m r u n m', t2) =
    (
      File
        (Data.Map.union b f)
        (Data.Map.union i k)
        (Data.Map.union j l)
        (Data.Map.union d h)
        (Data.Map.union e m)
        (Data.Map.union q r)
        (unionWith Data.Map.union t u)
        (Data.Map.union g n)
        (Data.Map.union d' m'),
      Data.Map.union t0 t2)
  defs :: Map' Expr_2
  defs =
    Data.Map.fromList
      [
        ("Add Int", Expr_2 (Just []) [] Add_Int_0_expression_2),
        ("Compare Char", Expr_2 (Just []) [] Compare_Char_0_expression_2),
        ("Compare Int", Expr_2 (Just []) [] Compare_Int_0_expression_2),
        (
          "Construct_List",
          Expr_2
            Nothing
            ["T"]
            (Function_expression_2
              (Name_pat_1 "x")
              (Function_expression_2
                (Name_pat_1 "y")
                (Algebraic_expression_2 "Construct_List" [Loc_expression_2 "x", Loc_expression_2 "y"])))),
        ("Convert Int", Expr_2 (Just []) [] Convert_Int_expression_2),
        ("Div", Expr_2 Nothing [] Div_0_expression_2),
        ("EQ", Expr_2 Nothing [] (Algebraic_expression_2 "EQ" [])),
        ("Empty_List", Expr_2 Nothing ["T"] (Algebraic_expression_2 "Empty_List" [])),
        ("False", Expr_2 Nothing [] (Algebraic_expression_2 "False" [])),
        ("First", Expr_2 Nothing ["T", "U"] (Field_expression_2 "First")),
        ("GT", Expr_2 Nothing [] (Algebraic_expression_2 "GT" [])),
        ("LT", Expr_2 Nothing [] (Algebraic_expression_2 "LT" [])),
        (
          "Left",
          Expr_2
            Nothing
            ["T", "U"]
            (Function_expression_2 (Name_pat_1 "x") (Algebraic_expression_2 "Left" [Loc_expression_2 "x"]))),
        ("Mod", Expr_2 Nothing [] Mod_0_expression_2),
        ("Multiply Int", Expr_2 (Just []) [] Multiply_Int_0_expression_2),
        ("Negate Int", Expr_2 (Just []) [] Negate_Int_expression_2),
        ("Nothing", Expr_2 Nothing ["T"] (Algebraic_expression_2 "Nothing" [])),
        (
          "Pair",
          Expr_2
            Nothing
            ["T", "U"]
            (Function_expression_2
              (Name_pat_1 "x")
              (Function_expression_2
                (Name_pat_1 "y")
                (Struct_expression_2
                  (Data.Map.fromList [("First", Loc_expression_2 "x"), ("Second", Loc_expression_2 "y")]))))),
        (
          "Right",
          Expr_2
            Nothing
            ["T", "U"]
            (Function_expression_2 (Name_pat_1 "x") (Algebraic_expression_2 "Right" [Loc_expression_2 "x"]))),
        ("Second", Expr_2 Nothing ["T", "U"] (Field_expression_2 "Second")),
        ("True", Expr_2 Nothing [] (Algebraic_expression_2 "True" [])),
        (
          "Wrap",
          Expr_2
            Nothing
            ["T"]
            (Function_expression_2 (Name_pat_1 "x") (Algebraic_expression_2 "Wrap" [Loc_expression_2 "x"]))),
        ("Write_Brackets Int", Expr_2 (Just []) [] Write_Brackets_Int_expression_2)]
  either_type :: Type_1 -> Type_1 -> Type_1
  either_type x = Application_type_1 (Application_type_1 (Name_type_1 "Either") x)
  find_and_delete :: Ord t => Map t u -> t -> Maybe (u, Map t u)
  find_and_delete a b = (\c -> (c, Data.Map.delete b a)) <$> Data.Map.lookup b a
  function_type :: Type_1 -> Type_1 -> Type_1
  function_type a = Application_type_1 (Application_type_1 (Name_type_1 "Function") a)
  init_type_context :: (File, Map' Op)
  init_type_context =
    (
      File
        kinds
        algebraics
        constrs
        types
        classes_0
        classes_1
        instances
        classes_2
        (Data.Map.singleton
          "Pair"
          (Strct
            [("T", Star_kind_0), ("U", Star_kind_0)]
            [("First", Name_type_1 "T"), ("Second", Name_type_1 "U")]
            (pair_type (Name_type_1 "T") (Name_type_1 "U")))),
      Data.Map.empty)
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
  kind_err :: Location_1 -> Err t
  kind_err a = Left ("Kind mismatch" ++ location' a)
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
        [
          "Add",
          "Char",
          "Compare",
          "Comparison",
          "Construct_List",
          "Convert",
          "Crash",
          "Div",
          "Div'",
          "EQ",
          "Either",
          "Empty_List",
          "False",
          "Field",
          "First",
          "Function",
          "GT",
          "Int",
          "Inverse",
          "LT",
          "Left",
          "List",
          "Logical",
          "Maybe",
          "Mod",
          "Modular",
          "Multiply",
          "Negate",
          "Next",
          "Nonzero",
          "Nothing",
          "Ord",
          "Pair",
          "Right",
          "Ring",
          "Second",
          "True",
          "Wrap",
          "Write_Brackets",
          "Writeable",
          "Zero"])
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
  old' :: Map' (Map' t) -> Map' (Map' (t, Status))
  old' = (<$>) old
  pair_type :: Type_1 -> Type_1 -> Type_1
  pair_type x = Application_type_1 (Application_type_1 (Name_type_1 "Pair") x)
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
  show_char :: Char -> String
  show_char c = show [c]
  show_mod :: Integer -> Integer -> String
  show_mod a b = show b ++ " # " ++ show a
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
  standard_naming_typing ::
    (
      String ->
      Tree_0 ->
      ((Set String, Locations, Locations), (File, Map' Op), Map' Expr_2, Map' (Map' Location')) ->
      Err ((Set String, Locations, Locations), (File, Map' Op), Map' Expr_2, Map' (Map' Location')))
  standard_naming_typing f a (b, (c, t), g, m) =
    (
      standard_1 (Location_1 f) t a >>=
      \(v, n') -> naming f n' b >>= \(d, e) -> (\(h, i, n) -> (d, (h, v), i, n)) <$> typing f e (c, g, m))
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
        Match_expression_2 d e ->
          Match_expression_2
            (f d)
            (case e of
              Matches_Algebraic_2 i j ->
                Matches_Algebraic_2 ((\(Match_Algebraic_2 k l) -> Match_Algebraic_2 k (f l)) <$> i) (f <$> j)
              Matches_char_2 i j -> Matches_char_2 (f <$> i) (f j)
              Matches_Int_2 i j -> Matches_Int_2 (f <$> i) (f j)
              Matches_Modular_2 i j -> Matches_Modular_2 (f <$> i) (f <$> j))
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
            (Struct_expression_2 (Data.Map.fromList ((\d -> (d, Loc_expression_2 ('!' : d))) <$> c)))
            c) :
        ((\d -> (d, Field_expression_2 d)) <$> c))
  type_branching :: (Location_0 -> Location_1) -> Map' Kind_0 -> Type_1 -> Data_br_1 -> Err [(String, Type_1)]
  type_branching a b c (Data_br_1 d e) =
    (\f -> (d, Prelude.foldr function_type c (snd <$> f)) : (second (function_type c) <$> f)) <$> type_fields a e b
  type_case ::
    (
      (Location_0 -> Location_1) ->
      Name ->
      Map' Type_1 ->
      [Pat] ->
      [Type_1] ->
      Map' (Type_2, Globloc) ->
      Map' Strct ->
      Integer ->
      Set String ->
      [(Type_1, Type_1)] ->
      Err ([Pat_1], Map' (Type_2, Globloc), Integer, Set String, [(Type_1, Type_1)]))
  type_case j (m @ (Name k l)) a b c d o p u x =
    case b of
      [] -> Right ([], d, p, u, x)
      e : f ->
        case c of
          [] -> Left ("Constructor " ++ l ++ location (j k) ++ " has been given too many arguments.")
          g : h ->
            (
              type_pat j o e (repl' a g) d p u x >>=
              \(i, n, q, v, y) -> (\(r, s, t, u1, m2) -> (i : r, s, t, u1, m2)) <$> type_case j m a f h n o q v y)
  type_class_0 ::
    (
      (Location_0 -> Location_1) ->
      Map' Kind_0 ->
      Class_2 ->
      (Map' (Map' Location'), Map' (Kind_0, Status), Map' (Class_5, Status), Map' String) ->
      Err
      (Class_3, (Map' (Map' Location'), Map' (Kind_0, Status), Map' (Class_5, Status), Map' String)))
  type_class_0 a j (Class_2 b (c, d) g' e) (m, i', j0, x2) =
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
                  Data.Map.insert b Data.Map.empty m,
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
      (
        Map' (Class_4, Status),
        Map' (Map' Location'),
        Map' (Type_2, Status),
        Map' (Kind_0, Status),
        Map' (Class_5, Status)) ->
      Err
        (
          Map' (Class_4, Status),
          Map' (Map' Location'),
          Map' (Type_2, Status),
          Map' (Kind_0, Status),
          Map' (Class_5, Status)))
  type_classes a c d (e, f, g, o, o') =
    (
      type_classes_0 (Location_1 a) c d (f, o, o', Data.Map.empty) >>=
      \(r, (j, p, p', _)) -> (\(k, n) -> (n, j, k, p, p')) <$> type_classes_1 a r (fst <$> p) (fst <$> p') (g, e))
  type_classes_0 ::
    (
      (Location_0 -> Location_1) ->
      Map' Kind_0 ->
      [Class_2] ->
      (Map' (Map' Location'), Map' (Kind_0, Status), Map' (Class_5, Status), Map' String) ->
      Err ([Class_3], (Map' (Map' Location'), Map' (Kind_0, Status), Map' (Class_5, Status), Map' String)))
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
-- todo: distinguish between these two error messages
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
      Class_4 _ f _ = unsafe_lookup c b
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
  type_data_1 :: Data_2 -> (Map' (Kind_0, Status), Constrs, Map' Expr_2) -> (Map' (Kind_0, Status), Constrs, Map' Expr_2)
  type_data_1 (Data_2 b c d) (e, f, g) =
    let
      l m n o =
        (
          ins_new b (Prelude.foldr Arrow_kind_0 Star_kind_0 (m ++ (snd <$> c))) e,
          Prelude.foldl (\h -> \p -> ins_new p b h) f n,
          Prelude.foldl (\h -> \(p, q) -> Data.Map.insert p q h) g o)
      s n o = l [] n (second (Expr_2 Nothing (fst <$> c)) <$> o)
    in
      case d of
        Algebraic_data_2 i ->
          s
            ((\(Form_1 m _) -> m) <$> i)
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
            []
            (
              (second (Expr_2 Nothing (fst <$> c)) <$> type_br_0 i) ++
              (second (Expr_2 Nothing ("!" : (fst <$> c))) <$> type_br_0 k))
        Struct_data_2 i -> s [] (type_br_0 (Data_br_1 b i))
  type_data_2 ::
    (
      (Location_0 -> Location_1) ->
      Data_2 ->
      Map' Kind_0 ->
      (Algebraics, Types, Map' (Strct, Status)) ->
      Err (Algebraics, Types, Map' (Strct, Status)))
  type_data_2 a (Data_2 g h b) c (d, e, f) =
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
                ins_new g (Alg h (Data.Map.fromList ((\(Form_2 r s) -> (r, s)) <$> q)) x) d,
                Prelude.foldl (\w' -> \(Form_2 u m) -> ins_new u (t' (Prelude.foldr function_type x m)) w') e q,
                f)) <$>
            type_forms a i l)
        Branching_data_2 i j k ->
          (
            (\s -> \t ->
              (
                d,
                Prelude.foldl
                  (\w' -> \(m, u) -> ins_new m u w')
                  e
                  ((second t' <$> s) ++ (second (Basic_type_1 ((j, Nat_kind_0) : h) Nothing []) <$> t)),
                f)) <$>
            type_branching a l (x2 (Name_type_1 "Zero")) i <*>
            type_branching
              a
              (Data.Map.insert j Nat_kind_0 l)
              (x2 (Application_type_1 (Name_type_1 "Next") (Name_type_1 j)))
              k)
        Struct_data_2 i ->
          (
            (\u -> (d, Prelude.foldl (\w' -> \(k, r) -> ins_new k (t' r) w') e u, ins_new g (Strct h u x) f)) <$>
            type_branching a l x (Data_br_1 g i))
  type_datas ::
    (Location_0 -> Location_1) ->
    [Data_2] ->
    (Map' (Kind_0, Status), Algebraics, Constrs, Types, Map' Expr_2, Map' (Strct, Status)) ->
    Err (Map' (Kind_0, Status), Algebraics, Constrs, Types, Map' Expr_2, Map' (Strct, Status))
  type_datas h a (b, i, j, d, c, a7) =
    let
      (u, v, w) = type_datas_1 a (b, j, c)
    in
      (\(b', c', t2) -> (u, b', v, c', w, t2)) <$> type_datas_2 h a (fst <$> u) (i, d, a7)
  type_datas_1 :: [Data_2] -> (Map' (Kind_0, Status), Constrs, Map' Expr_2) -> (Map' (Kind_0, Status), Constrs, Map' Expr_2)
  type_datas_1 b c = Prelude.foldl (\a -> \d -> type_data_1 d a) c b
  type_datas_2 ::
    (
      (Location_0 -> Location_1) ->
      [Data_2] ->
      Map' Kind_0 ->
      (Algebraics, Types, Map' (Strct, Status)) ->
      Err (Algebraics, Types, Map' (Strct, Status)))
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
      Map' (Map' Location') ->
      Map' (Map' ([[String]], Status)) ->
      Err (Def_4, Map' (Type_2, Status), Map' (Map' Location'), Map' (Map' ([[String]], Status))))
  type_def_1 l a b c k k2 t t' =
    case a of
      Basic_def_3 f d e e' g i ->
        let
          j' = type_kinds e Data.Map.empty
        in
          (
            type_constraints_0 Data.Map.empty e' (k2, j', (\_ -> Zr) <$> j') l >>=
            \o1 ->
              (
                (\h -> (Basic_def_4 f d e o1 h i, ins_new d (Basic_type_1 e Nothing o1 h) c, t, t')) <$>
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
                            type_cls_0 n q s' g (Location_1 l) m d >>=
                            \w ->
                              case Data.Map.lookup n (unsafe_lookup m t) of
                                Just u -> Left (location_err ("instances of " ++ m ++ " " ++ n) u (Location_1 l d))
                                Nothing ->
                                  Right
                                    (
                                      Instance_4 d m w0 o n q' p' w s' o1 r',
                                      c,
                                      adjust (Data.Map.insert n (Library (Location_1 l d))) m t,
                                      (case Data.Map.lookup m t' of
                                        Just _ -> adjust (ins_new n r') m
                                        Nothing -> Data.Map.insert m (Data.Map.singleton n (r', New))) t'))))))
  type_def_2 ::
    (Location_0 -> Location_1) ->
    Def_4 ->
    (Map' Alg, Map' String, Map' Type_2) ->
    Map' Expr_2 ->
    Map' (Map' [[String]]) ->
    Map' Kind_0 ->
    Map' Class_4 ->
    Map' Strct ->
    Err (Map' Expr_2)
  type_def_2 j a (d, l, k) c m n u0 w3 =
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
            (Prelude.foldl (\y -> \(z, w) -> Data.Map.insert z w y) n b)
            w3)
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
              w3
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
      (Map' Kind_0, Map' Alg, Map' String) ->
      (Map' Expr_2, Types) ->
      Map' Class_4 ->
      Map' Class_5 ->
      Map' (Map' Location') ->
      Map' (Map' ([[String]], Status)) ->
      Map' Strct ->
      Err (Map' Expr_2, Types, Map' (Map' Location'), Map' (Map' ([[String]], Status))))
  type_defs h a a2 (b, i, j) (c, d) y y0 z t k' =
    (
      type_defs_1 h a b d y y0 z t >>=
      \(g, e, k, u) ->
        (
          (\f -> (f, e, k, u)) <$
          type_ops h (fst <$> e) a2 <*>
          type_defs_2 (Location_1 h) g (i, j, fst <$> e) c ((<$>) fst <$> u) b y k'))
  type_defs_1 ::
    String ->
    [Def_3] ->
    Map' Kind_0 ->
    Types ->
    Map' Class_4 ->
    Map' Class_5 ->
    Map' (Map' Location') ->
    Map' (Map' ([[String]], Status)) ->
    Err ([Def_4], Types, Map' (Map' Location'), Map' (Map' ([[String]], Status)))
  type_defs_1 h a b c y y0 z u =
    case a of
      [] -> Right ([], c, z, u)
      d : e ->
        (
          type_def_1 h d b c y y0 z u >>=
          \(f, g, t, u') -> (\(k, l, m, t') -> (f : k, l, m, t')) <$> type_defs_1 h e b g y y0 t u')
  type_defs_2 ::
    (Location_0 -> Location_1) ->
    [Def_4] ->
    (Map' Alg, Map' String, Map' Type_2) ->
    Map' Expr_2 ->
    Map' (Map' [[String]]) ->
    Map' Kind_0 ->
    Map' Class_4 ->
    Map' Strct ->
    Err (Map' Expr_2)
  type_defs_2 f a b c g i u w =
    case a of
      [] -> Right c
      d : e -> type_def_2 f d b c g i u w >>= \h -> type_defs_2 f e b h g i u w
  type_expr ::
    String ->
    Type_1 ->
    (Location_0 -> Location_1) ->
    (Map' Alg, Map' String, Map' Type_2) ->
    Expression_1 ->
    Map' (Map' [[String]]) ->
    Integer ->
    Map' Kind_0 ->
    Map' Strct ->
    Err Expression_2
  type_expr k h a (c, d, e) f m w b t3 =
    (
      type_expression c d a w (Eqtns Data.Set.empty [] [] []) ((\e' -> (e', Glob)) <$> e) f h b t3 >>=
{-
        \(g, i, j, _, x) ->
          (
            solvesys (\y -> \p -> Left ("Type mismatch between " ++ min y p ++ " and " ++ max y p ++ n)) j (x, g, i) >>=
            \(y, p, k') ->
              case Data.Set.null k' of
                False -> Left ("Unresolved type variables" ++ n)
                True ->
                  (
                    p <$
                    slv
                      m
                      y
                      (\(Name p' q) -> \t -> \u ->
                        (
                          "Function " ++
                          q ++
                          location (a p') ++
                          " requires instance or constraint " ++
                          t ++
                          " " ++
                          u ++
                          ".")))))
-}
      \(g, i, _) -> solve_all a m (" in " ++ k) i g)
  type_expr' ::
    (Map' Kind_0, Map' Alg, Map' String, Map' Type_2) ->
    Expression_1 ->
    Map' (Map' [[String]]) ->
    Map' Strct ->
    Err Expression_2
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
    Map' String ->
    (Location_0 -> Location_1) ->
    Integer ->
    Eqtns ->
    Map' (Type_2, Globloc) ->
    Expression_1 ->
    Type_1 ->
    Map' Kind_0 ->
    Map' Strct ->
    Err (Expression_2, Eqtns, Integer)
  type_expression v w r o (Eqtns f h c' h9) d b e r7 z8 =
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
              r7
              z8 >>=
            \(i, k, p) ->
              (
                (\(l, m, n) -> (Application_expression_2 i l, m, n)) <$>
                type_expression v w r p k d g (Name_type_1 (show o)) r7 z8))
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
                      type_expression v w r o (Eqtns Data.Set.empty [] [] []) d c (sysrep' j (Name_type_1 "Zero") e) r5 z8 >>=
                      \(i', j', p) ->
                        (
                          (\(a6, b6, c6) -> (Branch_expression_2 j i' g a6, Eqtns f h c' (Cond_eqtns j j' g b6 : h9), c6)) <$>
                          type_expression
                            v
                            w
                            r
                            p
                            (Eqtns Data.Set.empty [] [] [])
                            d
                            i
                            (sysrep' j (Application_type_1 (Name_type_1 "Next") (Name_type_1 g)) e)
                            (Data.Map.insert g Nat_kind_0 r5)
                            z8))
                  _ -> Left ("Type " ++ j ++ location (r a) ++ " is not of kind Nat."))
        Char_expression_1 c -> Right (Char_expression_2 c, Eqtns f ((e, char_type) : h) c' h9, o)
        Function_expression_1 c g ->
          (
            type_pat r z8 c (Name_type_1 (show o)) d (o + 1) (Data.Set.insert (show o) f) h >>=
            \(a6, b6, c6, d6, f6) ->
              (
                (\(a', b', d') -> (Function_expression_2 a6 a', b', d')) <$>
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
                  g
                  (Name_type_1 (show c6))
                  r7
                  z8))
        Int_expression_1 c -> Right (Int_expression_2 c, Eqtns f ((e, int_type) : h) c' h9, o)
        Match_expression_1 a7 c g ->
          case g of
            Matches_Algebraic_1 i j ->
              case i of
                [] -> undefined
                Match_Algebraic_1 (Name l2 l) _ _ : _ ->
                  case Data.Map.lookup l w of
                    Just m ->
                      let
                        Alg n p q = unsafe_lookup m v
                        ((o', t, u), _) = typevars n (o, Data.Map.empty, f)
                      in
                        (
                          type_expression v w r o' (Eqtns u h c' h9) d c (repl' t q) r7 z8 >>=
                          \(x, a0, b0) ->
                            (
                              type_matches_algebraic
                                v
                                w
                                r
                                b0
                                a0
                                d
                                Data.Map.empty
                                i
                                e
                                (Right <$> p)
                                (l2, l)
                                t
                                r7
                                z8 >>=
                              \(d0, f0, g0, i0) ->
                                let
                                  k0 k1 = Match_expression_2 x (Matches_Algebraic_2 d0 k1)
                                in
                                  if all isLeft i0
                                    then
                                      case j of
                                        Just (l3, _) -> Left ("Unnecessary default case" ++ location' (r l3))
                                        Nothing -> Right (k0 Nothing, f0, g0)
                                    else
                                      case j of
                                        Just (_, j0) ->
                                          (\(a', b', c2) -> (k0 (Just a'), b', c2)) <$> type_expression v w r g0 f0 d j0 e r7 z8
                                        Nothing -> Left ("Incomplete match" ++ x' a7)))
                    Nothing -> Left ("Undefined algebraic constructor " ++ l ++ x' l2)
            Matches_char_1 i j ->
              (
                type_expression v w r o (Eqtns f h c' h9) d c char_type r7 z8 >>=
                \(k, m, n) ->
                  (
                    type_matches_char v w r n m d Data.Map.empty i e Data.Map.empty r7 z8 >>=
                    \(q, u, x) ->
                      (
                        (\(a0, b0, c0) -> (Match_expression_2 k (Matches_char_2 q a0), b0, c0)) <$>
                        type_expression v w r x u d j e r7 z8)))
            Matches_Int_1 i j ->
              (
                type_expression v w r o (Eqtns f h c' h9) d c int_type r7 z8 >>=
                \(k, m, n) ->
                  (
                    type_matches_int v w r n m d Data.Map.empty i e Data.Map.empty r7 z8 >>=
                    \(q, u, x) ->
                      (
                        (\(a0, b0, c0) -> (Match_expression_2 k (Matches_Int_2 q a0), b0, c0)) <$>
                        type_expression v w r x u d j e r7 z8)))
            Matches_Modular_1 i j ->
              case i of
                [] -> undefined
                Match_Modular_1 q3 (Modular _ m2 _) _ : _ ->
                  if m2 < 2
                    then Left ("Match expression over Modular " ++ show m2 ++ location (r a7))
                    else
                      (
                        type_expression v w r o (Eqtns f h c' h9) d c (mod_type (int_to_nat' m2)) r7 z8 >>=
                        \(k, m, n) ->
                          (
                            type_matches_modular
                              v
                              w
                              r
                              n
                              m
                              d
                              Data.Map.empty
                              i
                              e
                              (Data.Map.fromList ((\u4 -> (u4, Nothing)) <$> [0 .. m2 - 1]))
                              (q3, m2)
                              r7
                              z8 >>=
                            \(d0, f0, g0, i0) ->
                                let
                                  k0 k1 = Match_expression_2 k (Matches_Modular_2 d0 k1)
                                in
                                  if all isJust i0
                                    then
                                      case j of
                                        Just (l3, _) -> Left ("Unnecessary default case" ++ x' l3)
                                        Nothing -> Right (k0 Nothing, f0, g0)
                                    else
                                      case j of
                                        Just (_, j0) ->
                                          (\(a', b', c2) -> (k0 (Just a'), b', c2)) <$> type_expression v w r g0 f0 d j0 e r7 z8
                                        Nothing -> Left ("Incomplete match" ++ x' a7)))
        Modular_expression_1 c ->
          (
            (\(Modular' g g1) -> (Modular_expression_2 g1, Eqtns f ((e, mod_type (int_to_nat' g)) : h) c' h9, o)) <$>
            check_mod r c)
        Name_expression_1 (Name a7 c) g k ->
          let
            e5 a' = Left ("Too " ++ a' ++ " type arguments for variable " ++ c ++ x' a7)
          in
            und_err
              c
              d
              "variable"
              (r a7)
              (\(Basic_type_1 i x0 a' j, b3) ->
                let
                  g7 k2 d3 e4 s' r' d5 e8 =
                    (
                      (\((s, p, n), n9) ->
                        (
                          k2 n9,
                          Eqtns
                            n
                            ((e, repl' p j) : h)
                            (((\(Constraint_1 a0 b0) -> (a0, (Name a7 c, unsafe_lookup b0 p))) <$> a') ++ c')
                            h9,
                          s)) <$>
                      case k of
                        [] -> Right (typevars d3 (s', e4, f))
                        _ -> first (\f9 -> (s', f9, r')) <$> typevars' r r7 e5 d5 k e8)
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
                                \t7 ->
                                  g7
                                    (Glob_expression_2 c (Just t7))
                                    d'
                                    (Data.Map.singleton d5 t7)
                                    o
                                    f
                                    d'
                                    (Data.Map.singleton d5 t7))
                        Nothing -> Left ("Invalid class argument for variable " ++ c ++ x' a7)
                    Nothing ->
                      case x0 of
                        Just (Constraint_1 _ _) ->
                          case i of
                            [] -> undefined
                            (d5, _) : d' ->
                              g7
                                (Glob_expression_2 c (Just (Name_type_1 (show o))))
                                i
                                Data.Map.empty
                                (o + 1)
                                (Data.Set.insert (show o) f)
                                d'
                                (Data.Map.singleton d5 (Name_type_1 (show o)))
                        Nothing ->
                          g7
                            (\t9 ->
                              case b3 of
                                Glob -> Glob_expression_2 c Nothing t9
                                Loc -> Loc_expression_2 c)
                            i
                            Data.Map.empty
                            o
                            f
                            i
                            Data.Map.empty)
  type_exprs ::
    (
      (Name -> String) ->
      (Location_0 -> Location_1) ->
      (Map' Alg, Map' String, Map' Type_2) ->
      Map' (Map' [[String]]) ->
      [(Name, Expression_1, [(String, Kind_0)], [Constraint_1], Type_1)] ->
      (Map' Expr_2) ->
      String ->
      (Type_1 -> Type_1) ->
      Integer ->
      Map' Class_4 ->
      Map' Kind_0 ->
      Map' Strct ->
      [String] ->
      Err (Map' Expr_2))
  type_exprs a b c d h i t z w t0 x2 f5 f' =
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
            (Prelude.foldl (\k' -> \(l', m0) -> Data.Map.insert l' m0 k') x2 s)
            f5 >>=
          \g -> type_exprs a b c d m (Data.Map.insert (y ++ " " ++ t) (Expr_2 (Just f') (fst <$> s) g) i) t z w t0 x2 f5 f')
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
  type_match_algebraic ::
    (
      Map' Alg ->
      Map' String ->
      (Location_0 -> Location_1) ->
      Integer ->
      Eqtns ->
      Map' (Type_2, Globloc) ->
      Map' Match_Algebraic_2 ->
      Match_Algebraic_1 ->
      Type_1 ->
      Map' (Either Location_0 [Type_1]) ->
      (Location_0, String) ->
      Map' Type_1 ->
      Map' Kind_0 ->
      Map' Strct ->
      Err (Map' Match_Algebraic_2, Eqtns, Integer, Map' (Either Location_0 [Type_1])))
  type_match_algebraic a b c d (Eqtns g t0 t1 t2) h i (Match_Algebraic_1 (Name j k) l m) n o (q1, q) r m2 x5 =
    case Data.Map.lookup k o of
      Just p' ->
        case p' of
          Left e' -> Left (location_err' ("cases for " ++ k) (c e') (c j))
          Right p ->
            (
              type_case c (Name j k) r l p h x5 d g t0 >>=
              \(s0, s, w2, g2, t4) ->
                (
                  (\(t, u, v) -> (Data.Map.insert k (Match_Algebraic_2 s0 t) i, u, v, Data.Map.insert k (Left j) o)) <$>
                  type_expression a b c w2 (Eqtns g2 t4 t1 t2) s m n m2 x5))
      Nothing ->
        Left
          (
            case Data.Map.lookup k b of
              Just _ -> "Incompatible constructors " ++ q ++ " and " ++ k ++ location (c q1) ++ " and" ++ location' (c j)
              Nothing -> "Undefined algebraic constructor " ++ k ++ location' (c j))
  type_match_char ::
    (
      Map' Alg ->
      Map' String ->
      (Location_0 -> Location_1) ->
      Integer ->
      Eqtns ->
      Map' (Type_2, Globloc) ->
      Map Char Expression_2 ->
      Match_char_1 ->
      Type_1 ->
      Map Char Location_0 ->
      Map' Kind_0 ->
      Map' Strct ->
      Err (Map Char Expression_2, Eqtns, Integer, Map Char Location_0))
  type_match_char a b c d g h i (Match_char_1 y2 j k) l x1 w w7 =
    case Data.Map.lookup j x1 of
      Just y0 -> Left (location_err' ("cases for " ++ show_char j) (c y0) (c y2))
      Nothing -> (\(m, n, o) -> (Data.Map.insert j m i, n, o, Data.Map.insert j y2 x1)) <$> type_expression a b c d g h k l w w7
  type_match_int ::
    (
      Map' Alg ->
      Map' String ->
      (Location_0 -> Location_1) ->
      Integer ->
      Eqtns ->
      Map' (Type_2, Globloc) ->
      Map Integer Expression_2 ->
      Match_Int_1 ->
      Type_1 ->
      Map Integer Location_0 ->
      Map' Kind_0 ->
      Map' Strct ->
      Err (Map Integer Expression_2, Eqtns, Integer, Map Integer Location_0))
  type_match_int a b c d g h i (Match_Int_1 y2 j k) l x1 x3 t8 =
    case Data.Map.lookup j x1 of
      Just y0 -> Left (location_err' ("cases for " ++ show j) (c y0) (c y2))
      Nothing ->
        (\(m, n, o) -> (Data.Map.insert j m i, n, o, Data.Map.insert j y2 x1)) <$> type_expression a b c d g h k l x3 t8
  type_match_modular ::
    (
      Map' Alg ->
      Map' String ->
      (Location_0 -> Location_1) ->
      Integer ->
      Eqtns ->
      Map' (Type_2, Globloc) ->
      Map Integer Expression_2 ->
      Match_Modular_1 ->
      Type_1 ->
      Map Integer (Maybe Location_0) ->
      (Location_0, Integer) ->
      Map' Kind_0 ->
      Map' Strct ->
      Err (Map Integer Expression_2, Eqtns, Integer, Map Integer (Maybe Location_0)))
  type_match_modular a b c d g h i (Match_Modular_1 t r l) m n (p, s) x4 t8 =
    (
      check_mod c r >>=
      \(Modular' j k) ->
        if j == s
          then
            case unsafe_lookup k n of
              Just q -> Left (location_err' ("cases for " ++ show_mod j k) (c q) (c t))
              Nothing ->
                (
                  (\(u, v, x) -> (Data.Map.insert k u i, v, x, Data.Map.insert k (Just t) n)) <$>
                  type_expression a b c d g h l m x4 t8)
          else
            Left ("Incompatible modular types " ++ show s ++ " and " ++ show j ++ location (c p) ++ " and" ++ location' (c t)))
  type_matches_algebraic ::
    (
      Map' Alg ->
      Map' String ->
      (Location_0 -> Location_1) ->
      Integer ->
      Eqtns ->
      Map' (Type_2, Globloc) ->
      Map' Match_Algebraic_2 ->
      [Match_Algebraic_1] ->
      Type_1 ->
      Map' (Either Location_0 [Type_1]) ->
      (Location_0, String) ->
      Map' Type_1 ->
      Map' Kind_0 ->
      Map' Strct ->
      Err (Map' Match_Algebraic_2, Eqtns, Integer, Map' (Either Location_0 [Type_1])))
  type_matches_algebraic a b c d g h i j k s u v m0 z1 =
    case j of
      [] -> Right (i, g, d, s)
      l : m ->
        (
          type_match_algebraic a b c d g h i l k s u v m0 z1 >>=
          \(n, p, q, t) -> type_matches_algebraic a b c q p h n m k t u v m0 z1)
  type_matches_char ::
    (
      Map' Alg ->
      Map' String ->
      (Location_0 -> Location_1) ->
      Integer ->
      Eqtns ->
      Map' (Type_2, Globloc) ->
      Map Char Expression_2 ->
      [Match_char_1] ->
      Type_1 ->
      Map Char Location_0 ->
      Map' Kind_0 ->
      Map' Strct ->
      Err (Map Char Expression_2, Eqtns, Integer))
  type_matches_char a b c d g h i j k x1 w1 w2 =
    case j of
      [] -> Right (i, g, d)
      l : m -> type_match_char a b c d g h i l k x1 w1 w2 >>= \(n, p, q, x2) -> type_matches_char a b c q p h n m k x2 w1 w2
  type_matches_int ::
    (
      Map' Alg ->
      Map' String ->
      (Location_0 -> Location_1) ->
      Integer ->
      Eqtns ->
      Map' (Type_2, Globloc) ->
      Map Integer Expression_2 ->
      [Match_Int_1] ->
      Type_1 ->
      Map Integer Location_0 ->
      Map' Kind_0 ->
      Map' Strct ->
      Err (Map Integer Expression_2, Eqtns, Integer))
  type_matches_int a b c d g h i j k x1 m' w2 =
    case j of
      [] -> Right (i, g, d)
      l : m -> type_match_int a b c d g h i l k x1 m' w2 >>= \(n, p, q, x2) -> type_matches_int a b c q p h n m k x2 m' w2
  type_matches_modular ::
    (
      Map' Alg ->
      Map' String ->
      (Location_0 -> Location_1) ->
      Integer ->
      Eqtns ->
      Map' (Type_2, Globloc) ->
      Map Integer Expression_2 ->
      [Match_Modular_1] ->
      Type_1 ->
      Map Integer (Maybe Location_0) ->
      (Location_0, Integer) ->
      Map' Kind_0 ->
      Map' Strct ->
      Err (Map Integer Expression_2, Eqtns, Integer, Map Integer (Maybe Location_0)))
  type_matches_modular a b c d g h i j k u w x' w1 =
    case j of
      [] -> Right (i, g, d, u)
      m : n ->
        (
          type_match_modular a b c d g h i m k u w x' w1 >>=
          \(o, q, r, v) -> type_matches_modular a b c r q h o n k v w x' w1)
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
      Map' Strct ->
      Pat ->
      Type_1 ->
      Map' (Type_2, Globloc) ->
      Integer ->
      Set String ->
      [(Type_1, Type_1)] ->
      Err (Pat_1, Map' (Type_2, Globloc), Integer, Set String, [(Type_1, Type_1)]))
  type_pat k h (Pat g b) c d l n o =
    case b of
      Application_pat e f ->
        und_err
        e
        h
        "struct constructor"
        (k g)
        (\(Strct i j m) ->
          let
            ((p, q, r), _) = typevars i (l, Data.Map.empty, n)
          in
            (
              (\(s, t, u, v, w) -> (Application_pat_1 s, t, u, v, w)) <$>
              type_pats k h f (second (repl' q) <$> j) d p r ((c, repl' q m) : o) (Name g e)))
      Blank_pat -> Right (Blank_pat_1, d, l, n, o)
      Name_pat e -> Right (Name_pat_1 e, Data.Map.insert e (Basic_type_1 [] Nothing [] c, Loc) d, l, n, o)
  type_pats ::
    (
      (Location_0 -> Location_1) ->
      Map' Strct ->
      [Pat] ->
      [(String, Type_1)] ->
      Map' (Type_2, Globloc) ->
      Integer ->
      Set String ->
      [(Type_1, Type_1)] ->
      Name ->
      Err ([(String, Pat_1)], Map' (Type_2, Globloc), Integer, Set String, [(Type_1, Type_1)]))
  type_pats a b d e f g h i (Name x y) =
    let
      z a' = Left ("Constructor " ++ y ++ location (a x) ++ " has been given too " ++ a' ++ " arguments.")
    in
      case d of 
        [] ->
          case e of
            [] -> Right ([], f, g, h, i)
            _ -> z "few"
        j : k ->
          case e of
            [] -> z "many"
            (l, m) : n ->
              (
                type_pat a b j m f g h i >>=
                \(c, o, p, q, r) -> (\(s, t, u, v, w) -> ((l, c) : s, t, u, v, w)) <$> type_pats a b k n o p q r (Name x y))
{-
TODO:
Do it with 1 function that assembles a system of equations and one function that solves the system
Make error messages similar to those for type errors ("Kind mismatch between x and y ...")
-}
  type_typ :: (Location_0 -> Location_1) -> Type_8 -> Map' Kind_0 -> Kind_0 -> Err Type_1
  type_typ a (Type_8 b c) = type_type a b c
  type_type :: (Location_0 -> Location_1) -> Location_0 -> Type_5 -> Map' Kind_0 -> Kind_0 -> Err Type_1
  type_type l a c d e =
    let
      x = kind_err (l a)
    in
      case c of
        Application_type_5 f g ->
          (
            type_type' l a f d >>=
            \(h, i) ->
              case i of
                Arrow_kind_0 j k -> if k == e then Application_type_1 h <$> type_type l a g d j else x
                _ -> x)
        Name_type_5 (Name a' f) -> und_err f d "type" (l a') (\g -> if g == e then Right (Name_type_1 f) else x)
  type_type' :: (Location_0 -> Location_1) -> Location_0 -> Type_5 -> Map' Kind_0 -> Err (Type_1, Kind_0)
  type_type' l a c d =
    case c of
      Application_type_5 e f ->
        (
          type_type' l a e d >>=
          \(g, h) ->
            case h of
              Arrow_kind_0 i j -> (\k -> (Application_type_1 g k, j)) <$> type_type l a f d i
              _ -> kind_err (l a))
      Name_type_5 (Name a' e) -> und_err e d "type" (l a') (\f -> Right (Name_type_1 e, f))
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
          "Construct_List",
          Basic_type_1
            [("T", Star_kind_0)]
            Nothing
            []
            (function_type (Name_type_1 "T") (function_type (list_type (Name_type_1 "T")) (list_type (Name_type_1 "T"))))),
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
        ("EQ", Basic_type_1 [] Nothing [] comparison_type),
        ("Empty_List", Basic_type_1 [("T", Star_kind_0)] Nothing [] (list_type (Name_type_1 "T"))),
        ("False", Basic_type_1 [] Nothing [] logical_type),
        (
          "First",
          Basic_type_1
            [("T", Star_kind_0), ("U", Star_kind_0)]
            Nothing
            []
            (function_type (pair_type (Name_type_1 "T") (Name_type_1 "U")) (Name_type_1 "T"))),
        ("GT", Basic_type_1 [] Nothing [] comparison_type),
        (
          "Inverse",
          Basic_type_1
            [("T", Star_kind_0)]
            (Just (Constraint_1 "Field" "T"))
            [Constraint_1 "Field" "T", Constraint_1 "Ring" "T"]
            (function_type (Name_type_1 "T") (maybe_type (Name_type_1 "T")))),
        (
          "Left",
          Basic_type_1
            [("T", Star_kind_0), ("U", Star_kind_0)]
            Nothing
            []
            (function_type (Name_type_1 "T") (either_type (Name_type_1 "T") (Name_type_1 "U")))),
        ("LT", Basic_type_1 [] Nothing [] comparison_type),
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
        ("Nothing", Basic_type_1 [("T", Star_kind_0)] Nothing [] (maybe_type (Name_type_1 "T"))),
        (
          "Pair",
          Basic_type_1
            [("T", Star_kind_0), ("U", Star_kind_0)]
            Nothing
            []
            (function_type
              (Name_type_1 "T")
              (function_type (Name_type_1 "U") (pair_type (Name_type_1 "T") (Name_type_1 "U"))))),
        (
          "Right",
          Basic_type_1
            [("T", Star_kind_0), ("U", Star_kind_0)]
            Nothing
            []
            (function_type (Name_type_1 "U") (either_type (Name_type_1 "T") (Name_type_1 "U")))),
        (
          "Second",
          Basic_type_1
            [("T", Star_kind_0), ("U", Star_kind_0)]
            Nothing
            []
            (function_type (pair_type (Name_type_1 "T") (Name_type_1 "U")) (Name_type_1 "U"))),
        ("True", Basic_type_1 [] Nothing [] logical_type),
        (
          "Wrap",
          Basic_type_1 [("T", Star_kind_0)] Nothing [] (function_type (Name_type_1 "T") (maybe_type (Name_type_1 "T")))),
        (
          "Write_Brackets",
          Basic_type_1
            [("T", Star_kind_0)]
            (Just (Constraint_1 "Writeable" "T"))
            [Constraint_1 "Writeable" "T"]
            (function_type (Name_type_1 "T") (pair_type (list_type char_type) logical_type)))]
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
  typing :: String -> Tree_5 -> (File, Map' Expr_2, Map' (Map' Location')) -> Err (File, Map' Expr_2, Map' (Map' Location'))
  typing k (Tree_5 a a' x7 c) (File d t u v b' c5 x t2 k7, l, m') =
    (
      type_datas (Location_1 k) a (old d, old t, old u, old v, l, old k7) >>=
      \(e, b, h, g, f, k8) ->
        (
          type_classes k (fst <$> e) a' (old b', m', g, old t2, old c5) >>=
          \(c', m2, g0, x2, t3) ->
            (
              (\(i, j, n', y) ->
                (
                  File
                    (rem_old e)
                    (rem_old b)
                    (rem_old h)
                    (rem_old j)
                    (rem_old c')
                    (rem_old t3)
                    (rem_old' y)
                    (rem_old x2)
                    (rem_old k8),
                  i,
                  n')) <$>
              type_defs
                k
                c
                x7
                (fst <$> e, fst <$> b, fst <$> h)
                (f, g0)
                (fst <$> c')
                (fst <$> t3)
                m2
                (old' x)
                (fst <$> k8))))
  unsafe_left :: Either t u -> t
  unsafe_left a =
    case a of
      Left b -> b
      Right _ -> undefined
  unsafe_lookup :: Ord t => t -> Map t u -> u
  unsafe_lookup a b =
    case Data.Map.lookup a b of
      Just c -> c
      Nothing -> undefined
-----------------------------------------------------------------------------------------------------------------------------