{-
protection against duplicate file loading - what happens now? if crashes - fix, give a nice error/warning. if nothing - warn?
tests
type synonyms?
operators
topelt-esindajate kontroll nimekontrolliga kokku? move duplicate instance control into Naming module?
internal: do something with old/new status tags. check where exactly they're necessary. get rid of them where they're useless
change semantics of missing pattern-match variables from blank to lambda? (Left -> e is not Left _ -> e but Left x -> e x)
internal: make the system of specifying built-in algebraic data types and things better and safer
Allow hiding things to functions outside module - so that helper functions are not exported from the module?
normalising constructors for some data types (polynomial, fraction) which assume a certain normal form of fields?
allow to hide (prevent exporting) constructors and field accessors which can potentially have bad behavior
switch expression that is less strict and more flexible than match?
some limited pattern matching in function arguments (and maybe also variables introduced through algebraic matching?)
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
semantics of "Pair -> f" should be "Pair x y -> f x y"
make match expression more flexible (like case in Haskell)?
mis juhtub kui esimeses moodulis on kusagil tüübimuutuja T ja järgmises moodulis sama nimega globaalne tüüp?
Let f = Crash, x = f f In 0 -- tüüpimine läheb lõpmatusse tsüklisse sest puudub occur check
can Data.Set and Data.Map imports be removed if the file uses both and disambiguates all function calls anyways?
let expr de-sugaring (and therefore struct name collection) completely to Standard.hs module
all de-sugaring: remove from Tree.hs, put into Standard.hs
simplify parsing of match expression and remove duplicate code from de-sugaring, name checking, typechecking & eval
allow operators in pattern matching
What happens with unary minus and binary minus during parsing?
allow using operators in class method definitions? Instance Ring{Complex T}<Ring T>(..., Complex x y * Complex z w = ...)
-}
module Awful.Typechecker (
  Alg,
  Expression_2 (..),
  File (..),
  Kind_1,
  Match_Algebraic_2 (..),
  Matches_2 (..),
  Nat (..),
  Pat_1 (..),
  Strct,
  Type_2,
  context_union,
  defs,
  init_type_context,
  kinds,
  locations,
  show_mod,
  standard_naming_typing,
  type_expr',
  unsafe_lookup) where
  import Awful.Namechecker
  import Awful.Operators
  import Awful.Parser
  import Awful.Tokeniser
  import Control.Monad
  import Data.Bifunctor
  import Data.List
  import Data.Map
  import Data.Maybe
  import Data.Set
  import Parser.Locations
  data Alg = Alg [(String, Kind_1)] (Map' [Type_1]) Type_1 deriving Show -- TODO: REM STRINGS FROM FST MAP
  type Algebraics = Map' (Alg, Status)
  data Brnch_3 = Brnch_3 String [(String, Kind_1)] String [(String, Type_8)] deriving Show
  data Class_3 = Class_3 String (String, Kind_1) (Maybe Name) [Method_3] deriving Show
  data Class_4 = Class_4 (String, Kind_1) (Maybe String) [Method_4] deriving Show
  data Class_5 = Class_5 Kind_1 (Maybe String) [String] deriving Show
  data Constraint_1 = Constraint_1 String String deriving Show
  type Constrs = Map' (String, Status)
  data Data_3 = Data_3 String Data_br_3 deriving Show
  data Data_br_3 = Branching_data_3 [(String, Kind_1)] [Brnch_3] | Plain_data_3 [(String, Kind_1)] Data_branch_1 deriving Show
  data Def_4 =
    Basic_def_4 Location String [(String, Kind_1)] [Constraint_1] Type_1 Expression_1 [String] |
    Instance_4
      Location
      String
      (Maybe String)
      String
      String
      [(String, Kind_1)]
      Integer
      [(Name, Expression_1, [(String, Kind_1)], [Constraint_1], Type_1)]
      Type_1
      [Constraint_1]
      [String]
      [[String]]
        deriving Show
  data Expression_2 =
    Add_Int_0_expression_2 |
    Add_Int_1_expression_2 Integer |
    Add_Modular_0_expression_2 Integer |
    Add_Modular_1_expression_2 Integer Integer |
    Algebraic_expression_2 String [Expression_2] |
    Application_expression_2 Expression_2 Expression_2 |
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
    Int_expression_2 Integer |
    Inverse_Modular_expression_2 Integer |
    Match_expression_2 Expression_2 Matches_2 |
    Mod_0_expression_2 |
    Mod_1_expression_2 Integer |
    Modular_expression_2 Integer |
    Multiply_Int_0_expression_2 |
    Multiply_Int_1_expression_2 Integer |
    Multiply_Modular_0_expression_2 Integer |
    Multiply_Modular_1_expression_2 Integer Integer |
    Name_expression_2 String |
    Negate_Int_expression_2 |
    Negate_Modular_expression_2 Integer |
    Struct_expression_2 String (Map' Expression_2)
      deriving Show
  data File =
    File
      (Map' Kind_1)
      (Map' Alg)
      (Map' String)
      (Map' Type_2)
      (Map' Class_4)
      (Map' Class_5)
      (Map' (Map' [[String]]))
      (Map' Kind_1)
      (Map' Strct)
        deriving Show
  data Form_2 = Form_2 String [Type_1] deriving Show
  data Kind_1 = Function_kind_1 Kind_1 Kind_1 | Name_kind_1 String deriving (Eq, Show)
  data Match_Algebraic_2 = Match_Algebraic_2 [Pat_1] Expression_2 deriving Show
  data Matches_2 =
    Matches_Algebraic_2 (Map' Match_Algebraic_2) (Maybe Expression_2) |
    Matches_Int_2 (Map Integer Expression_2) Expression_2 |
    Matches_Modular_2 (Map Integer Expression_2) (Maybe Expression_2)
      deriving Show
  data Method_3 = Method_3 String [(String, Kind_1)] [Constraint_0] Type_1 deriving Show
  data Method_4 = Method_4 String [(String, Kind_1)] [Constraint_1] Type_1 deriving Show
  data Modular' = Modular' Integer Integer deriving Show
  data Nat = Nxt Nat | Zr deriving (Eq, Ord, Show)
  data Pat_1 = Application_pat_1 [(String, Pat_1)] | Blank_pat_1 | Name_pat_1 String deriving Show
  data Plain_dat = Plain_dat String Data_branch_1 deriving Show
  data Prom_alg = Prom_alg (Map' [Kind_1]) deriving Show
  data Strct = Strct [(String, Kind_1)] [(String, Type_1)] Type_1 deriving Show
  data Type_1 = Application_type_1 Type_1 Type_1 | Name_type_1 String deriving (Eq, Show)
  data Type_2 = Basic_type_1 [(String, Kind_1)] (Maybe Constraint_1) [Constraint_1] Type_1 deriving Show
  data Tmatch' = Tmatch' [Pat_1] Typedexpr deriving Show
  data Typedexpr =
    Application_texpr Typedexpr Typedexpr |
    Function_texpr Pat_1 Typedexpr |
    Int_texpr Integer |
    Match_texpr Typedexpr Typedmatches |
    Modular_texpr Integer |
    Name_texpr_0 String String Type_1 |
    Name_texpr_1 String [(String, Type_1)]
      deriving Show
  data Typedmatches =
    Tmatch_algebraic (Map' Tmatch') (Maybe Typedexpr) |
    Tmatch_int (Map Integer Typedexpr) Typedexpr |
    Tmatch_Modular (Map Integer Typedexpr) (Maybe Typedexpr)
      deriving Show
  type Types = Map' (Type_2, Status)
  addargs :: Map' ([String], Map' [(String, Nat)]) -> Typedexpr -> Expression_2
  addargs b c =
    let
      h = addargs b
    in
      case c of
        Application_texpr d e -> Application_expression_2 (h d) (h e)
        Function_texpr d e -> Function_expression_2 d (h e)
        Int_texpr d -> Int_expression_2 d
        Match_texpr d e ->
          Match_expression_2
            (h d)
            (case e of
              Tmatch_algebraic f g -> Matches_Algebraic_2 ((\(Tmatch' i j) -> Match_Algebraic_2 i (h j)) <$> f) (h <$> g)
              Tmatch_int f g -> Matches_Int_2 (h <$> f) (h g)
              Tmatch_Modular f g -> Matches_Modular_2 (h <$> f) (h <$> g))
        Modular_texpr d -> Modular_expression_2 d
        Name_texpr_0 a "Nonzero" d ->
          (case a of
            "Add_Modular" -> Add_Modular_0_expression_2
            "Convert_Modular" -> Convert_Modular_expression_2
            "Div'" -> Div'_expression_2
            "Inverse_Modular" -> Inverse_Modular_expression_2
            "Multiply_Modular" -> Multiply_Modular_0_expression_2
            "Negate_Modular" -> Negate_Modular_expression_2
            _ -> undefined)
              (nat_to_int d)
        Name_texpr_0 d e f ->
          let
            (g, i) = typestring f []
          in
            addargs_2
              b
              (
                second (getarg i) <$>
                case Data.Map.lookup g (snd (unsafe_lookup e b)) of
                  Just k -> k
                  Nothing -> [])
              (Name_expression_2 (d ++ " " ++ g))
        Name_texpr_1 d e -> addargs_2 b e (Name_expression_2 d)
  addargs_1 :: Map' ([String], Map' [(String, Nat)]) -> String -> String -> [Type_1] -> Expression_2 -> Expression_2
  addargs_1 _ "Nonzero" "!Next" [c] d =
    Prelude.foldl
      (\e -> \f -> Application_expression_2 e (f (nat_to_int (Application_type_1 (Name_type_1 "!Next") c))))
      d
      [
        Add_Modular_0_expression_2,
        Convert_Modular_expression_2,
        Div'_expression_2,
        Inverse_Modular_expression_2,
        Multiply_Modular_0_expression_2,
        Negate_Modular_expression_2]
  addargs_1 c d e f g =
    let
      (h, i) = unsafe_lookup d c
    in
      Prelude.foldl
        (\j -> \k ->
          Application_expression_2
            j
            (addargs_2
              c
              (
                second (getarg f) <$>
                case Data.Map.lookup e i of
                  Just l -> l
                  Nothing -> [])
              (Name_expression_2 (k ++ " " ++ e))))
        g
        h
  addargs_2 :: Map' ([String], Map' [(String, Nat)]) -> [(String, Type_1)] -> Expression_2 -> Expression_2
  addargs_2 b d e =
    case d of
      [] -> e
      (f, g) : h ->
        let
          (c, i) = typestring g []
        in
          addargs_2 b h (addargs_1 b f c i e)
  algebraics :: Map' Alg
  algebraics =
    Data.Map.fromList
      [
        ("Comparison", Alg [] (Data.Map.fromList [("EQ", []), ("GT", []), ("LT", [])]) comparison_type),
        (
          "Maybe",
          Alg
            [("T", star_kind)]
            (Data.Map.fromList [("Nothing", []), ("Wrap", [Name_type_1 "T"])]) (maybe_type (Name_type_1 "T"))),
        ("Nat", Alg [] (Data.Map.fromList [("Next", [Name_type_1 "Nat"]), ("Zr", [])]) (Name_type_1 "Nat"))]
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
  check_mod :: (Location -> Location_1) -> Modular -> Err Modular'
  check_mod a (Modular d b c) =
    if c < b then Right (Modular' b c) else Left ("Invalid Modular " ++ show_mod b c ++ location' (a d))
  classes_0 :: Map' Class_4
  classes_0 =
    Data.Map.fromList
      [
        (
          "Nonzero",
          Class_4
            ("N", nat_kind)
            Nothing
            [
              Method_4
                "Add_Modular"
                []
                []
                (function_type
                  (mod_type (Name_type_1 "N"))
                  (function_type (mod_type (Name_type_1 "N")) (mod_type (Name_type_1 "N")))),
              Method_4 "Convert_Modular" [] [] (function_type int_type (mod_type (Name_type_1 "N"))),
              Method_4 "Div'" [] [] (function_type int_type int_type),
              Method_4
                "Inverse_Modular"
                []
                []
                (function_type (mod_type (Name_type_1 "N")) (maybe_type (mod_type (Name_type_1 "N")))),
              Method_4
                "Multiply_Modular"
                []
                []
                (function_type
                  (mod_type (Name_type_1 "N"))
                  (function_type (mod_type (Name_type_1 "N")) (mod_type (Name_type_1 "N")))),
              Method_4 "Negate_Modular" [] [] (function_type (mod_type (Name_type_1 "N")) (mod_type (Name_type_1 "N")))]),
        (
          "Ord",
          Class_4
            ("T", star_kind)
            Nothing
            [Method_4 "Compare" [] [] (function_type (Name_type_1 "T") (function_type (Name_type_1 "T") comparison_type))]),
        (
          "Ring",
          Class_4
            ("T", star_kind)
            Nothing
            [
              Method_4 "Add" [] [] (function_type (Name_type_1 "T") (function_type (Name_type_1 "T") (Name_type_1 "T"))),
              Method_4 "Convert" [] [] (function_type int_type (Name_type_1 "T")),
              Method_4 "Multiply" [] [] (function_type (Name_type_1 "T") (function_type (Name_type_1 "T") (Name_type_1 "T"))),
              Method_4 "Negate" [] [] (function_type (Name_type_1 "T") (Name_type_1 "T"))])]
  classes_1 :: Map' Class_5
  classes_1 = (\(Class_4 (_, a) b c) -> Class_5 a b ((\(Method_4 d _ _ _) -> d) <$> c)) <$> classes_0
  classes_2 :: Map' Kind_1
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
        ("EQ", "Comparison"),
        ("GT", "Comparison"),
        ("LT", "Comparison"),
        ("Next", "Nat"),
        ("Nothing", "Maybe"),
        ("Wrap", "Maybe"),
        ("Zr", "Nat")]
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
  defs :: Map' Expression_2
  defs =
    Data.Map.fromList
      [
        ("Add Int", Add_Int_0_expression_2),
        (
          "Add Modular",
          Function_expression_2
            (Name_pat_1 "x")
            (Function_expression_2
              Blank_pat_1
              (Function_expression_2
                Blank_pat_1
                (Function_expression_2
                  Blank_pat_1
                  (Function_expression_2
                    Blank_pat_1
                    (Function_expression_2 Blank_pat_1 (Function_expression_2 Blank_pat_1 (Name_expression_2 "x")))))))),
        ("Compare Int", Compare_Int_0_expression_2),
        ("Compare Modular", Compare_Modular_0_expression_2),
        ("Convert Int", Convert_Int_expression_2),
        (
          "Convert Modular",
          Function_expression_2
            Blank_pat_1
            (Function_expression_2
              (Name_pat_1 "x")
              (Function_expression_2
                Blank_pat_1
                (Function_expression_2
                  Blank_pat_1
                  (Function_expression_2
                    Blank_pat_1
                    (Function_expression_2 Blank_pat_1 (Function_expression_2 Blank_pat_1 (Name_expression_2 "x")))))))),
        ("Div", Div_0_expression_2),
        ("EQ", Algebraic_expression_2 "EQ" []),
        ("GT", Algebraic_expression_2 "GT" []),
        ("LT", Algebraic_expression_2 "LT" []),
        ("Mod", Mod_0_expression_2),
        ("Multiply Int", Multiply_Int_0_expression_2),
        (
          "Multiply Modular",
          Function_expression_2
            Blank_pat_1
            (Function_expression_2
              Blank_pat_1
              (Function_expression_2
                Blank_pat_1
                (Function_expression_2
                  Blank_pat_1
                  (Function_expression_2
                    (Name_pat_1 "x")
                    (Function_expression_2 Blank_pat_1 (Function_expression_2 Blank_pat_1 (Name_expression_2 "x")))))))),
        ("Negate Int", Negate_Int_expression_2),
        (
          "Negate Modular",
          Function_expression_2
            Blank_pat_1
            (Function_expression_2
              Blank_pat_1
              (Function_expression_2
                Blank_pat_1
                (Function_expression_2
                  Blank_pat_1
                  (Function_expression_2
                    Blank_pat_1
                    (Function_expression_2
                      (Name_pat_1 "x")
                      (Function_expression_2 Blank_pat_1 (Name_expression_2 "x")))))))),
        ("Next", Function_expression_2 (Name_pat_1 "x") (Algebraic_expression_2 "Next" [Name_expression_2 "x"])),
        ("Nothing", Algebraic_expression_2 "Nothing" []),
        ("Wrap", Function_expression_2 (Name_pat_1 "x") (Algebraic_expression_2 "Wrap" [Name_expression_2 "x"])),
        ("Zr", Algebraic_expression_2 "Zr" [])]
  function_type :: Type_1 -> Type_1 -> Type_1
  function_type a = Application_type_1 (Application_type_1 (Name_type_1 "Function") a)
  getarg :: [t] -> Nat -> t
  getarg a b =
    case a of
      [] -> undefined
      c : d ->
        case b of
          Nxt e -> getarg d e
          Zr -> c
  hkinds :: Set String
  hkinds = Data.Set.fromList ["!Nat", "Star"]
  init_type_context :: (File, Map' Op)
  init_type_context =
    (File kinds algebraics constrs types classes_0 classes_1 instances classes_2 Data.Map.empty, Data.Map.empty)
  instances :: Map' (Map' [[String]])
  instances =
    Data.Map.fromList
      [
        ("Nonzero", Data.Map.fromList []),
        ("Ord", Data.Map.fromList [("Int", []), ("Modular", [[]])]),
        ("Ring", Data.Map.fromList [("Int", []), ("Modular", [["Nonzero"]])])]
  int_to_nat_type :: Integer -> Type_1
  int_to_nat_type x =
    case x of
      0 -> Name_type_1 "!Zr"
      _ -> next_type (int_to_nat_type (x - 1))
  int_type :: Type_1
  int_type = Name_type_1 "Int"
  isLeft :: Either t u -> Bool
  isLeft a =
    case a of
      Left _ -> True
      Right _ -> False
  kind_err :: Location_1 -> Err t
  kind_err a = Left ("Kind mismatch" ++ location' a)
  kinds :: Map' Kind_1
  kinds =
    Data.Map.fromList
      [
        ("!Next", Function_kind_1 nat_kind nat_kind),
        ("!Zr", nat_kind),
        ("Comparison", star_kind),
        ("Function", Function_kind_1 star_kind (Function_kind_1 star_kind star_kind)),
        ("Int", star_kind),
        ("Maybe", Function_kind_1 star_kind star_kind),
        ("Modular", Function_kind_1 nat_kind star_kind),
        ("Nat", star_kind)]
  location_err' :: String -> Location_1 -> Location_1 -> String
  location_err' a b = location_err a (Library b)
  locations :: Locations
  locations =
    Data.Map.fromList
      (
        (\x -> (x, Language)) <$>
        [
          "Add",
          "Add_Modular",
          "Compare",
          "Comparison",
          "Convert",
          "Convert_Modular",
          "Crash",
          "Div",
          "Div'",
          "EQ",
          "Function",
          "GT",
          "Int",
          "Inverse_Modular",
          "LT",
          "Maybe",
          "Mod",
          "Modular",
          "Multiply",
          "Multiply_Modular",
          "Nat",
          "Negate",
          "Negate_Modular",
          "Next",
          "Nonzero",
          "Nothing",
          "Ord",
          "Ring",
          "Wrap",
          "Zr"])
  maybe_type :: Type_1 -> Type_1
  maybe_type = Application_type_1 (Name_type_1 "Maybe")
  mod_type :: Type_1 -> Type_1
  mod_type = Application_type_1 (Name_type_1 "Modular")
  nat_kind :: Kind_1
  nat_kind = Name_kind_1 "!Nat"
  nat_to_int :: Type_1 -> Integer
  nat_to_int x =
    case x of
      Name_type_1 "!Zr" -> 0
      Application_type_1 (Name_type_1 "!Next") y -> nat_to_int y + 1
      _ -> undefined
  nat_to_int' :: Type_1 -> Bool
  nat_to_int' x =
    case x of
      Name_type_1 "!Zr" -> True
      Application_type_1 (Name_type_1 "!Next") y -> nat_to_int' y
      _ -> False
  nat_type :: Type_1
  nat_type = Name_type_1 "Nat"
  next_type :: Type_1 -> Type_1
  next_type = Application_type_1 (Name_type_1 "!Next")
  old' :: Map' (Map' t) -> Map' (Map' (t, Status))
  old' = (<$>) old
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
          case (c, d) of
            ("Nonzero", Application_type_1 (Name_type_1 "!Next") c') -> if nat_to_int' c' then slv a e h else i
            _ ->
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
  solvesys ::
    (String -> String -> Err ([(String, (Name, Type_1))], Typedexpr, Set String)) ->
    [(Type_1, Type_1)] ->
    ([(String, (Name, Type_1))], Typedexpr, Set String) ->
    Err ([(String, (Name, Type_1))], Typedexpr, Set String)
  solvesys m b (a', t, u) =
    case b of
      [] -> Right (a', t, u)
      (c, d) : g ->
        case c of
          Application_type_1 e f ->
            case d of
              Application_type_1 h i -> solvesys m ((e, h) : (f, i) : g) (a', t, u)
              Name_type_1 h -> solvesys' m h c g (a', t, u)
          Name_type_1 e ->
            case d of
              Name_type_1 f ->
                if e == f
                  then solvesys m g (a', t, u)
                  else
                    case Data.Set.member e u of
                      False ->
                        case Data.Set.member f u of
                          False -> m e f
                          True -> solvesys_rep m f c g (a', t, u)
                      True -> solvesys_rep m e d g (a', t, u)
              _ -> solvesys' m e d g (a', t, u)
  solvesys' ::
    (String -> String -> Err ([(String, (Name, Type_1))], Typedexpr, Set String)) ->
    String ->
    Type_1 ->
    [(Type_1, Type_1)] ->
    ([(String, (Name, Type_1))], Typedexpr, Set String) ->
    Err ([(String, (Name, Type_1))], Typedexpr, Set String)
  solvesys' h b c d (x, m, a) =
    let
      (y, _) = typestring c []
    in
      case Data.Set.member b a of
        False ->
          h
            b
            (case Data.Set.member y a of
              False -> y
              True -> "an application type")
        True -> solvesys_rep h b c d (x, m, a)
  solvesys_rep ::
    (String -> String -> Err ([(String, (Name, Type_1))], Typedexpr, Set String)) ->
    String ->
    Type_1 ->
    [(Type_1, Type_1)] ->
    ([(String, (Name, Type_1))], Typedexpr, Set String) ->
    Err ([(String, (Name, Type_1))], Typedexpr, Set String)
  solvesys_rep a c d e (x, f, k) =
    let
      m = sysrep' c d
    in
      solvesys a ((<$>) (bimap m m) e) (second (second m) <$> x, sysrep2 c d f, Data.Set.delete c k)
  standard_naming_typing ::
    (
      String ->
      Tree_0 ->
      (
        (Set String, Locations, Locations),
        (File, Map' Op),
        Map' Expression_2,
        Map' Kind_1,
        Map' (Map' Location'),
        Map' ([String], Map' [(String, Nat)])) ->
      Err
        (
          (Set String, Locations, Locations),
          (File, Map' Op),
          Map' Expression_2,
          Map' Kind_1,
          Map' (Map' Location'),
          Map' ([String], Map' [(String, Nat)])))
  standard_naming_typing f a (b, (c, t), g, j, m, w) =
    (
      standard_1 (Location_1 f) t a >>=
      \(v, n') -> naming f n' b >>= \(d, e) -> (\(h, i, k, n, u) -> (d, (h, v), i, k, n, u)) <$> typing f e (c, g, j, m, w))
  star_kind :: Kind_1
  star_kind = Name_kind_1 "Star"
  sysrep' :: String -> Type_1 -> Type_1 -> Type_1
  sysrep' a b c =
    let
      f = sysrep' a b
    in
      case c of
        Application_type_1 d e -> Application_type_1 (f d) (f e)
        Name_type_1 d -> if d == a then b else c
  sysrep2 :: String -> Type_1 -> Typedexpr -> Typedexpr
  sysrep2 a b c =
    let
      f = sysrep2 a b
    in
      case c of
        Application_texpr d e -> Application_texpr (f d) (f e)
        Function_texpr d e -> Function_texpr d (f e)
        Match_texpr d e ->
          Match_texpr
            (f d)
            (case e of
              Tmatch_algebraic g h -> Tmatch_algebraic ((\(Tmatch' i j) -> Tmatch' i (f j)) <$> g) (f <$> h)
              Tmatch_int g h -> Tmatch_int (f <$> g) (f h)
              Tmatch_Modular g h -> Tmatch_Modular (f <$> g) (f <$> h))
        Name_texpr_0 d g e -> Name_texpr_0 d g (sysrep' a b e)
        Name_texpr_1 d e -> Name_texpr_1 d (second (sysrep' a b) <$> e)
        _ -> c
  type_alg :: [t] -> String -> Expression_2
  type_alg a b =
    let
      c = show <$> [0 .. length a - 1]
    in
      Prelude.foldr Function_expression_2 (Algebraic_expression_2 b (Name_expression_2 <$> c)) (Name_pat_1 <$> c)
  type_branching ::
    (
      (Location -> Location_1) ->
      String ->
      Map' (Either Location [Kind_1]) ->
      Brnch_2 ->
      Err (Map' (Either Location [Kind_1]), Brnch_3))
  type_branching e j f (Brnch_2 (Name a g) b c d) =
    case Data.Map.lookup g f of
      Just h ->
        case h of
          Left i -> Left ("Conflicting data cases for " ++ g ++ location (e i) ++ " and" ++ location' (e a))
          Right i ->
            if length b == length i
              then Right (Data.Map.insert g (Left a) f, Brnch_3 g (zip b i) c d)
              else Left ("Kind constructor " ++ g ++ location (e a) ++ " has a wrong number of arguments.")
      Nothing -> Left ("Undefined constructor for " ++ j ++ " " ++ g ++ location' (e a))
  type_branching_1 :: (Location -> Location_1) -> Map' Kind_1 -> Types -> String -> [(String, Kind_1)] -> Brnch_3 -> Err Types
  type_branching_1 f q g n o (Brnch_3 b c d e) =
    let
      m = Basic_type_1 (o ++ c) Nothing []
      l =
        Prelude.foldl
          (\i -> \(j, _) -> Application_type_1 i (Name_type_1 j))
          (Application_type_1
            (Name_type_1 n)
            (Prelude.foldl (\i -> \(j, _) -> Application_type_1 i (Name_type_1 j)) (Name_type_1 b) c))
          o
    in
      (
        (\h ->
          Prelude.foldl
            (\i -> \(j, k) -> ins_new j (m (function_type l k)) i)
            (ins_new d (m (Prelude.foldr (\(_, i) -> function_type i) l h)) g)
            h) <$>
        type_types' f (Prelude.foldl (\i -> \(j, k) -> Data.Map.insert j k i) q c) e)
  type_branchings ::
    (Location -> Location_1) -> String -> Location -> String -> Map' (Either Location [Kind_1]) -> [Brnch_2] -> Err [Brnch_3]
  type_branchings e h i j f a =
    case a of
      [] -> if all isLeft f then Right [] else Left ("Incomplete match in branching data type " ++ h ++ location' (e i))
      b : c -> type_branching e j f b >>= \(g, d) -> (:) d <$> type_branchings e h i j g c
  type_branchings_1 ::
    (Location -> Location_1) -> Map' Kind_1 -> Types -> String -> [(String, Kind_1)] -> [Brnch_3] -> Err Types
  type_branchings_1 g f a i h b =
    case b of
      [] -> Right a
      c : d -> type_branching_1 g f a i h c >>= \e -> type_branchings_1 g f e i h d
  type_case ::
    (
      (Location -> Location_1) ->
      Name ->
      Map' Type_1 ->
      [Pat] ->
      [Type_1] ->
      Map' Type_2 ->
      Map' Strct ->
      Integer ->
      Set String ->
      [(Type_1, Type_1)] ->
      Err ([Pat_1], Map' Type_2, Integer, Set String, [(Type_1, Type_1)]))
  type_case j (m@(Name k l)) a b c d o p u x =
    case b of
      [] -> Right ([], d, p, u, x)
      e : f ->
        case c of
          [] -> Left ("Constructor " ++ l ++ location (j k) ++ " has been given too many arguments.")
          g : h ->
            (
              type_pat j o e (repl' a g) d p u x >>=
              \(i, n, q, v, y) -> (\(r, s, t, w, z) -> (i : r, s, t, w, z)) <$> type_case j m a f h n o q v y)
  type_class_0 ::
    (Location -> Location_1) ->
    Map' Kind_1 ->
    Class_2 ->
    (
      Map' (Map' Location'),
      Map' ([String], Map' [(String, Nat)]),
      Map' (Kind_1, Status),
      Map' (Class_5, Status),
      Map' String) ->
    Err
      (
        Class_3,
        (
          Map' (Map' Location'),
          Map' ([String], Map' [(String, Nat)]),
          Map' (Kind_1, Status),
          Map' (Class_5, Status),
          Map' String))
  type_class_0 a j (Class_2 b (c, d) g' e) (m, w0, i', j0, x2) =
    (
      type_kind_7 a d >>=
      \h ->
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
                    Class_3 b (c, h) g' g,
                    (
                      Data.Map.insert b Data.Map.empty m,
                      Data.Map.insert b (g2, Data.Map.empty) w0,
                      ins_new b h i',
                      ins_new b (Class_5 h g3 g2) j0,
                      case g' of
                        Just (Name _ t0) -> Data.Map.insert b t0 x2
                        Nothing -> x2))) <$>
              type_methods_0 a e (Data.Map.insert c h j))))
  type_class_1 ::
    String ->
    Class_3 ->
    Map' Kind_1 ->
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
    Kind_1 ->
    [Pattern_0] ->
    Err ([(String, Kind_1)], Integer, Type_1, Map' Kind_1, Map' Nat) ->
    Kind_1 ->
    Integer ->
    Type_1 ->
    Map' Kind_1 ->
    Map' Nat ->
    Nat ->
    Err ([(String, Kind_1)], Integer, Type_1, Map' Kind_1, Map' Nat)
  type_class_args a b e g c x c0 c' n =
    case b of
      [] -> if a == g then Right ([], c, x, c0, c') else e
      h : d ->
        case a of
          Function_kind_1 l f ->
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
    String ->
    Map' Kind_1 ->
    [Class_2] ->
    (
      Map' (Class_4, Status),
      Map' (Map' Location'),
      Map' (Type_2, Status),
      Map' ([String], Map' [(String, Nat)]),
      Map' (Kind_1, Status),
      Map' (Class_5, Status)) ->
    Err
      (
        Map' (Class_4, Status),
        Map' (Map' Location'),
        Map' (Type_2, Status),
        Map' ([String], Map' [(String, Nat)]),
        Map' (Kind_1, Status),
        Map' (Class_5, Status))
  type_classes a c d (e, f, g, i, o, o') =
    (
      type_classes_0 (Location_1 a) c d (f, i, o, o', Data.Map.empty) >>=
      \(r, (j, m, p, p', _)) -> (\(k, n) -> (n, j, k, m, p, p')) <$> type_classes_1 a r (fst <$> p) (fst <$> p') (g, e))
  type_classes_0 ::
    (Location -> Location_1) ->
    Map' Kind_1 ->
    [Class_2] ->
    (
      Map' (Map' Location'),
      Map' ([String], Map' [(String, Nat)]),
      Map' (Kind_1, Status),
      Map' (Class_5, Status),
      Map' String) ->
    Err
      (
        [Class_3],
        (
          Map' (Map' Location'),
          Map' ([String], Map' [(String, Nat)]),
          Map' (Kind_1, Status),
          Map' (Class_5, Status),
          Map' String))
  type_classes_0 a g b c =
    case b of
      [] -> Right ([], c)
      d : e -> type_class_0 a g d c >>= \(h, i) -> first ((:) h) <$> type_classes_0 a g e i
  type_classes_1 ::
    String ->
    [Class_3] ->
    Map' Kind_1 ->
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
    (Location -> Location_1) ->
    String ->
    Location ->
    Err [(Name, Expression_1, [(String, Kind_1)], [Constraint_1], Type_1)]
  type_cls_0 a b c d l m n =
    case d of
      [] ->
        case b of
          [] -> Right []
          (Method_4 e _ _ _) : _ -> Left ("Missing definition " ++ e ++ " in " ++ m ++ " " ++ a ++ location' (l n))
      (p'@(Name h i), j) : k ->
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
    (Map' Class_5, Map' Kind_1) ->
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
    Map' (Map' [String]) ->
    [Constraint_0] ->
    (Map' Class_5, Map' Kind_1, Map' Nat) ->
    String ->
    Err ([Constraint_1], [String], [(String, Nat)])
  type_constraints_0 g a (f, t, u) h =
    case a of
      [] ->
        let
          l y = join (y <$> assocs (keys <$> g))
        in
          Right
            (
              l (\(i, j) -> Constraint_1 i <$> j),
              join (Data.Map.elems (join <$> Data.Map.elems <$> g)),
              l (\(i, j) -> (,) i <$> ((\k -> unsafe_lookup k u) <$> j)))
      b : c -> type_constraint_0 g b (f, t) h >>= \d -> type_constraints_0 d c (f, t, u) h
  type_constraints_1 :: [Constraint_1] -> Map' (Map' [[String]]) -> Map' Class_4 -> Map' (Map' [[String]])
  type_constraints_1 a e d =
    case a of
      [] -> e
      b : c -> type_constraints_1 c (type_constraint_1 b e d) d
  type_data_1 ::
    (Location -> Location_1) ->
    Data_2 ->
    (Map' (Kind_1, Status), Constrs, Map' Expression_2, Map' Kind_1) ->
    Err ((Map' (Kind_1, Status), Constrs, Map' Expression_2, Map' Kind_1), Data_3)
  type_data_1 q (Data_2 (Name a2 a) b') (i, j, k, x) =
    case b' of
      Branching_data_2 e f ->
        (
          type_kinds_5 q e >>=
          \m ->
            let
              p = Prelude.foldr Function_kind_1 star_kind (Name_kind_1 "!Nat" : (snd <$> m))
            in
              (
                (\n ->
                  (
                    (
                      ins_new a p i,
                      j,
                      Prelude.foldl
                        (\o1 -> \(Brnch_3 f9 _ u s) ->
                          let
                            w = fst <$> s
                          in
                            Prelude.foldl
                              (\t -> \v -> Data.Map.insert v (Field_expression_2 v) t)
                              (Data.Map.insert
                                u
                                (Prelude.foldr
                                  (\t -> Function_expression_2 (Name_pat_1 ('#' : t)))
                                  (Struct_expression_2
                                    f9
                                    (Data.Map.fromList ((\t -> (t, Name_expression_2 ('#' : t))) <$> w)))
                                  w)
                                o1)
                              w)
                        k
                        n,
                      Data.Map.insert a p x),
                    Data_3 a (Branching_data_3 m n))) <$>
                type_branchings q a a2 "!Nat" (Right <$> (Data.Map.fromList [("!Next", [nat_kind]), ("!Zr", [])])) f))
      Plain_data_2 b c ->
        let
          (l, m) =
            case c of
              Algebraic_data_1 e ->
                (
                  Prelude.foldl (\d -> \(Form_1 n _) -> ins_new n a d) j e,
                  Prelude.foldl (\f -> \(Form_1 g h) -> Data.Map.insert g (type_alg h g) f) k e)
              Struct_data_1 e ->
                let
                  e' = fst <$> e
                in
                  (
                    j,
                    Prelude.foldl
                      (flip (\g -> Data.Map.insert g (Field_expression_2 g)))
                      (Data.Map.insert
                        a
                        (Prelude.foldr
                          (\f -> Function_expression_2 (Name_pat_1 ('#' : f)))
                          (Struct_expression_2 a (Data.Map.fromList ((\f -> (f, Name_expression_2 ('#' : f))) <$> e')))
                          e')
                        k)
                      e')
        in
          (
            (\p ->
              let
                y = Prelude.foldr Function_kind_1 star_kind (snd <$> p)
              in
                ((ins_new a y i, l, m, Data.Map.insert a y x), Data_3 a (Plain_data_3 p c))) <$>
            type_kinds_5 q b)
  type_data_2 ::
    (
      (Location -> Location_1) ->
      Data_3 ->
      Map' Kind_1 ->
      (Algebraics, Types, Map' (Strct, Status)) ->
      Err (Algebraics, Types, Map' (Strct, Status)))
  type_data_2 f (Data_3 a b') d (p, e, q') =
    case b' of
      Branching_data_3 g h -> (\t -> (p, t, q')) <$> type_branchings_1 f (type_kinds g d) e a g h
      Plain_data_3 b c ->
        let
          g = type_kinds b d
          x = Prelude.foldl (\n -> \n' -> (Application_type_1 n (Name_type_1 n'))) (Name_type_1 a) (fst <$> b)
          t' = Basic_type_1 b Nothing []
        in
          case c of
            Algebraic_data_1 h ->
              (
                (\q ->
                  (
                    ins_new a (Alg b (Data.Map.fromList ((\(Form_2 r s) -> (r, s)) <$> q)) x) p,
                    Prelude.foldl (flip (\(Form_2 l m) -> ins_new l (t' (Prelude.foldr function_type x m)))) e q,
                    q')) <$>
                type_forms f h g)
            Struct_data_1 h ->
              (
                (\i ->
                  (
                    p,
                    Prelude.foldl
                      (flip (\(k, l) -> ins_new k (t' (function_type x l))))
                      (ins_new a (t' (Prelude.foldr (function_type <$> snd) x i)) e)
                      i,
                    ins_new a (Strct b i x) q')) <$>
                type_fields f h g)
  type_datas ::
    (Location -> Location_1) ->
    [Data_2] ->
    (Map' (Kind_1, Status), Algebraics, Constrs, Types, Map' Expression_2, Map' Kind_1, Map' (Strct, Status)) ->
    Err (Map' (Kind_1, Status), Algebraics, Constrs, Types, Map' Expression_2, Map' Kind_1, Map' (Strct, Status))
  type_datas h a (b, i, j, d, c, m, a7) =
    (
      type_datas_1 h a (b, j, c, m) >>=
      \((u, v, w, a'), y) -> (\(b', c', t2) -> (u, b', v, c', w, a', t2)) <$> type_datas_2 h y (fst <$> u) (i, d, a7))
  type_datas_1 ::
    (
      (Location -> Location_1) ->
      [Data_2] ->
      (Map' (Kind_1, Status), Constrs, Map' Expression_2, Map' Kind_1) ->
      Err ((Map' (Kind_1, Status), Constrs, Map' Expression_2, Map' Kind_1), [Data_3]))
  type_datas_1 f b c =
    case b of
      [] -> Right (c, [])
      d : e -> type_data_1 f d c >>= \(g, h) -> second ((:) h) <$> type_datas_1 f e g
  type_datas_2 ::
    (
      (Location -> Location_1) ->
      [Data_3] ->
      Map' Kind_1 ->
      (Algebraics, Types, Map' (Strct, Status)) ->
      Err (Algebraics, Types, Map' (Strct, Status)))
  type_datas_2 f a b c =
    case a of
      [] -> Right c
      d : e -> type_data_2 f d b c >>= type_datas_2 f e b
  type_def_1 ::
    String ->
    Def_3 ->
    Map' Kind_1 ->
    Map' (Type_2, Status) ->
    Map' Class_4 ->
    Map' Class_5 ->
    Map' (Map' Location') ->
    Map' (Map' ([[String]], Status)) ->
    Map' ([String], Map' [(String, Nat)]) ->
    Err
      (
        Def_4,
        Map' (Type_2, Status),
        Map' (Map' Location'),
        Map' (Map' ([[String]], Status)),
        Map' ([String], Map' [(String, Nat)]))
  type_def_1 l a b c k k2 t t' u3 =
    case a of
      Basic_def_3 f d e e' g i ->
        (
          type_kinds_1 (Location_1 l) e b Data.Map.empty >>=
          \(y, j, j') ->
            (
              type_constraints_0 Data.Map.empty e' (k2, j', (\_ -> Zr) <$> j') l >>=
                \(o1, o2, _) ->
                (
                  (\h -> (Basic_def_4 f d y o1 h i o2, ins_new d (Basic_type_1 y Nothing o1 h) c, t, t', u3)) <$>
                  type_typ (Location_1 l) g j star_kind)))
      Instance_3 d (Name e m) (Name f n) w2 k' o' g ->
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
              (\ s ->
                (
                  ziphelp (Location_1 l) n f w2 >>=
                    \ () ->
                      (
                        type_class_args
                          s
                          k'
                          (kind_err (Location_1 l f))
                          p
                          0
                          (Name_type_1 n)
                          Data.Map.empty
                          Data.Map.empty
                          Zr >>=
                        \(q', p', s', t0, t7) ->
                          (
                            type_constraints_0 Data.Map.empty o' (k2, t0, t7) l >>=
                            \(o1, o2, o3) ->
                              let
                                r' =
                                  (
                                    (\(x', _) ->
                                      (
                                        (\(Constraint_1 y' _) -> y') <$>
                                        Data.List.filter (\(Constraint_1 _ y') -> y' == x') o1)) <$>
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
                                            Instance_4 d m w0 o n q' p' w s' o1 o2 r',
                                            c,
                                            adjust (Data.Map.insert n (Library (Location_1 l d))) m t,
                                            (case Data.Map.lookup m t' of
                                              Just _ -> adjust (ins_new n r') m
                                              Nothing -> Data.Map.insert m (Data.Map.singleton n (r', New))) t',
                                            adjust (second (Data.Map.insert n o3)) m u3)))))))
  type_def_2 ::
    (Location -> Location_1) ->
    Def_4 ->
    (Map' Alg, Map' String, Map' Type_2) ->
    Map' Expression_2 ->
    Map' (Map' [[String]]) ->
    Map' Kind_1 ->
    Map' ([String], Map' [(String, Nat)]) ->
    Map' Class_4 ->
    Map' Strct ->
    Err (Map' Expression_2)
  type_def_2 j a (d, l, k) c m n t' u0 w3 =
    case a of
      Basic_def_4 r e b x h i y' ->
        (
          (\t -> Data.Map.insert e (Prelude.foldr (\x' -> Function_expression_2 (Name_pat_1 x')) t y') c) <$>
          type_expr
            ("definition " ++ e ++ location' (j r))
            h
            j
            (d, l, k)
            i
            (type_constraints_1 x m u0)
            0
            t'
            (Prelude.foldl (\y -> \(z, w) -> Data.Map.insert z w y) n b)
            w3
            Data.Set.empty)
      Instance_4 l' e' w0 w e e0 e1 f f' g' c2 r' ->
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
              (\g -> Prelude.foldr (\b -> Function_expression_2 (Name_pat_1 b)) g c2)
              t'
              u0
              f4
              w3
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
    String ->
    [Def_3] ->
    [Name] ->
    (Map' Kind_1, Map' Alg, Map' String) ->
    (Map' Expression_2, Types) ->
    Map' Class_4 ->
    Map' Class_5 ->
    Map' (Map' Location') ->
    Map' (Map' ([[String]], Status)) ->
    Map' ([String], Map' [(String, Nat)]) ->
    Map' Strct ->
    Err
      (
        Map' Expression_2,
        Types,
        Map' (Map' Location'),
        Map' (Map' ([[String]], Status)),
        Map' ([String], Map' [(String, Nat)]))
  type_defs h a a2 (b, i, j) (c, d) y y0 z t u' k' =
    (
      type_defs_1 h a b d y y0 z t u' >>=
      \(g, e, k, u, f') ->
        (
          (\f -> (f, e, k, u, f')) <$
          type_ops h (fst <$> e) a2 <*>
          type_defs_2 (Location_1 h) g (i, j, fst <$> e) c ((<$>) fst <$> u) b f' y k'))
  type_defs_1 ::
    String ->
    [Def_3] ->
    Map' Kind_1 ->
    Types ->
    Map' Class_4 ->
    Map' Class_5 ->
    Map' (Map' Location') ->
    Map' (Map' ([[String]], Status)) ->
    Map' ([String], Map' [(String, Nat)]) ->
    Err ([Def_4], Types, Map' (Map' Location'), Map' (Map' ([[String]], Status)), Map' ([String], Map' [(String, Nat)]))
  type_defs_1 h a b c y y0 z u v =
    case a of
      [] -> Right ([], c, z, u, v)
      d : e ->
        (
          type_def_1 h d b c y y0 z u v >>=
          \(f, g, t, u', v') -> (\(k, l, m, t', k') -> (f : k, l, m, t', k')) <$> type_defs_1 h e b g y y0 t u' v')
  type_defs_2 ::
    (Location -> Location_1) ->
    [Def_4] ->
    (Map' Alg, Map' String, Map' Type_2) ->
    Map' Expression_2 ->
    Map' (Map' [[String]]) ->
    Map' Kind_1 ->
    Map' ([String], Map' [(String, Nat)]) ->
    Map' Class_4 ->
    Map' Strct ->
    Err (Map' Expression_2)
  type_defs_2 f a b c g i j u w =
    case a of
      [] -> Right c
      d : e -> type_def_2 f d b c g i j u w >>= \h -> type_defs_2 f e b h g i j u w
  type_expr ::
    String ->
    Type_1 ->
    (Location -> Location_1) ->
    (Map' Alg, Map' String, Map' Type_2) ->
    Expression_1 ->
    Map' (Map' [[String]]) ->
    Integer ->
    Map' ([String], Map' [(String, Nat)]) ->
    Map' Kind_1 ->
    Map' Strct ->
    Set String ->
    Err Expression_2
  type_expr k h a (c, d, e) f m w w' b t3 x8 =
    let
      n = " in " ++ k
    in
      (
        type_expression c d a w x8 [] e f h [] b t3 >>=
        \(g, i, j, _, x) ->
          (
            solvesys (\y -> \p -> Left ("Type mismatch between " ++ min y p ++ " and " ++ max y p ++ n)) j (x, g, i) >>=
            \(y, p, k') ->
              case Data.Set.null k' of
                False -> Left ("Unresolved type variables" ++ n)
                True ->
                  (
                    addargs w' p <$
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
  type_expr' ::
    (Map' Kind_1, Map' Alg, Map' String, Map' Type_2) ->
    Expression_1 ->
    Map' (Map' [[String]]) ->
    Map' ([String], Map' [(String, Nat)]) ->
    Map' Strct ->
    Err Expression_2
  type_expr' (b, c, d, e) f g h t6 =
    type_expr "input." (Name_type_1 "0") (Location_1 "input") (c, d, e) f g 1 h b t6 (Data.Set.singleton "0")
  type_expression ::
    Map' Alg ->
    Map' String ->
    (Location -> Location_1) ->
    Integer ->
    Set String ->
    [(Type_1, Type_1)] ->
    Map' Type_2 ->
    Expression_1 ->
    Type_1 ->
    [(String, (Name, Type_1))] ->
    Map' Kind_1 ->
    Map' Strct ->
    Err (Typedexpr, Set String, [(Type_1, Type_1)], Integer, [(String, (Name, Type_1))])
  type_expression v w r o f h d b e c' r7 z8 =
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
              (Data.Set.insert (show o) f)
              h
              d
              c
              (function_type (Name_type_1 (show o)) e)
              c'
              r7
              z8 >>=
            \(i, j, k, p, d') ->
              (
                (\(l, m, n, q, e') -> (Application_texpr i l, m, n, q, e')) <$>
                type_expression v w r p j k d g (Name_type_1 (show o)) d' r7 z8))
        Function_expression_1 c g ->
          (
            type_pat r z8 c (Name_type_1 (show o)) d (o + 1) (Data.Set.insert (show o) f) h >>=
            \(a6, b6, c6, d6, f6) ->
              (
                (\(a', b', c3, d', f') -> (Function_texpr a6 a', b', c3, d', f')) <$>
                type_expression
                  v
                  w
                  r
                  (c6 + 1)
                  (Data.Set.insert (show c6) d6)
                  ((e, function_type (Name_type_1 (show o)) (Name_type_1 (show c6))) : f6)
                  b6
                  g
                  (Name_type_1 (show c6))
                  c'
                  r7
                  z8))
        Int_expression_1 c -> Right (Int_texpr c, f, (e, int_type) : h, o, c')
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
                        (o', t, u) = typevars n (o, Data.Map.empty, f)
                      in
                        (
                          type_expression v w r o' u h d c (repl' t q) c' r7 z8 >>=
                          \(x, y, a0, b0, a2) ->
                            (
                              type_matches_algebraic
                                v
                                w
                                r
                                b0
                                y
                                a0
                                d
                                Data.Map.empty
                                i
                                e
                                (Right <$> p)
                                (l2, l)
                                t
                                a2
                                r7
                                z8 >>=
                              \(d0, e0, f0, g0, i0, a3) ->
                                let
                                  k0 k1 = Match_texpr x (Tmatch_algebraic d0 k1)
                                in
                                  if all isLeft i0
                                    then
                                      case j of
                                        Just (l3, _) -> Left ("Unnecessary default case" ++ location' (r l3))
                                        Nothing -> Right (k0 Nothing, e0, f0, g0, a3)
                                    else
                                      case j of
                                        Just (_, j0) ->
                                          (
                                            (\(a', b', c2, d', a4) -> (k0 (Just a'), b', c2, d', a4)) <$>
                                            type_expression v w r g0 e0 f0 d j0 e a3 r7 z8)
                                        Nothing -> Left ("Incomplete match" ++ x' a7)))
                    Nothing -> Left ("Undefined algebraic constructor " ++ l ++ x' l2)
            Matches_Int_1 i j ->
              (
                type_expression v w r o f h d c int_type c' r7 z8 >>=
                \(k, l, m, n, d') ->
                  (
                    type_matches_int v w r n l m d Data.Map.empty i e Data.Map.empty d' r7 z8 >>=
                    \(q, t, u, x, e') ->
                      (
                        (\(a0, b0, c0, d0, a2) -> (Match_texpr k (Tmatch_int q a0), b0, c0, d0, a2)) <$>
                        type_expression v w r x t u d j e e' r7 z8)))
            Matches_Modular_1 i j ->
              case i of
                [] -> undefined
                Match_Modular_1 q3 (Modular _ m2 _) _ : _ ->
                  if m2 < 2
                    then Left ("Match expression over Modular " ++ show m2 ++ location (r a7))
                    else
                      (
                        type_expression v w r o f h d c (mod_type (int_to_nat_type m2)) c' r7 z8 >>=
                        \(k, l, m, n, d') ->
                          (
                            type_matches_modular
                              v
                              w
                              r
                              n
                              l
                              m
                              d
                              Data.Map.empty
                              i
                              e
                              (Data.Map.fromList ((\u4 -> (u4, Nothing)) <$> [0 .. m2 - 1]))
                              d'
                              (q3, m2)
                              r7
                              z8 >>=
                            \(d0, e0, f0, g0, i0, a3) ->
                                let
                                  k0 k1 = Match_texpr k (Tmatch_Modular d0 k1)
                                in
                                  if all isJust i0
                                    then
                                      case j of
                                        Just (l3, _) -> Left ("Unnecessary default case" ++ x' l3)
                                        Nothing -> Right (k0 Nothing, e0, f0, g0, a3)
                                    else
                                      case j of
                                        Just (_, j0) ->
                                          (
                                            (\(a', b', c2, d7, a4) -> (k0 (Just a'), b', c2, d7, a4)) <$>
                                            type_expression v w r g0 e0 f0 d j0 e a3 r7 z8)
                                        Nothing -> Left ("Incomplete match" ++ x' a7)))
        Modular_expression_1 c ->
          (\(Modular' g g1) -> (Modular_texpr g1, f, (e, mod_type (int_to_nat_type g)) : h, o, c')) <$> check_mod r c
        Name_expression_1 (Name a7 c) g k ->
          let
            e5 a' = Left ("Too " ++ a' ++ " type arguments for variable " ++ c ++ x' a7)
          in
            und_err
              c
              d
              "variable"
              (r a7)
              (\(Basic_type_1 i x0 a' j) ->
                let
                  g7 k2 d3 e4 s' r' d5 e8 =
                    (
                      (\(s, p, n) ->
                        let
                          x7 = (\(Constraint_1 a0 b0) -> (a0, (Name a7 c, unsafe_lookup b0 p))) <$> a'
                        in
                          (k2 x7, n, (e, repl' p j) : h, s, x7 ++ c')) <$>
                      case k of
                        [] -> Right (typevars d3 (o, e4, f))
                        _ -> (\f9 -> (s', f9, r')) <$> typevars' r r7 e5 d5 k e8)
                in
                  case g of
                    Just t3 ->
                      case x0 of
                        Just (Constraint_1 y0 _) ->
                          case i of
                            [] -> undefined
                            (d5, k3) : d' ->
                              (
                                type_typ r t3 r7 k3 >>=
                                \t7 ->
                                  g7
                                    (\_ -> Name_texpr_0 c y0 t7)
                                    d'
                                    (Data.Map.singleton d5 t7)
                                    o
                                    f
                                    d'
                                    (Data.Map.singleton d5 t7))
                        Nothing -> Left ("Invalid class argument for variable " ++ c ++ x' a7)
                    Nothing ->
                      case x0 of
                        Just (Constraint_1 y0 _) ->
                          case i of
                            [] -> undefined
                            (d5, _) : d' ->
                              g7
                                (\_ -> Name_texpr_0 c y0 (Name_type_1 (show o)))
                                i
                                Data.Map.empty
                                (o + 1)
                                (Data.Set.insert (show o) f)
                                d'
                                (Data.Map.singleton d5 (Name_type_1 (show o)))
                        Nothing -> g7 (\t9 -> Name_texpr_1 c (second snd <$> t9)) i Data.Map.empty o f i Data.Map.empty)
  type_exprs ::
    (
      (Name -> String) ->
      (Location -> Location_1) ->
      (Map' Alg, Map' String, Map' Type_2) ->
      Map' (Map' [[String]]) ->
      [(Name, Expression_1, [(String, Kind_1)], [Constraint_1], Type_1)] ->
      (Map' Expression_2) ->
      String ->
      (Type_1 -> Type_1) ->
      Integer ->
      (Expression_2 -> Expression_2) ->
      Map' ([String], Map' [(String, Nat)]) ->
      Map' Class_4 ->
      Map' Kind_1 ->
      Map' Strct ->
      Err (Map' Expression_2))
  type_exprs a b c d h i t z w f' t' t0 x2 f5 =
    case h of
      [] -> Right i
      (j@(Name _ y), k, s, t5, l) : m ->
        (
          type_expr
            (a j)
            (z l)
            b
            c
            k
            (type_constraints_1 t5 d t0)
            w
            t'
            (Prelude.foldl (\k' -> \(l', m0) -> Data.Map.insert l' m0 k') x2 s)
            f5
            Data.Set.empty >>=
          \g -> type_exprs a b c d m (Data.Map.insert (y ++ " " ++ t) (f' g) i) t z w f' t' t0 x2 f5)
  type_field :: (Location -> Location_1) -> (String, Type_8) -> Map' Kind_1 -> Err (String, Type_1)
  type_field d (a, b) c  = (,) a <$> type_typ d b c star_kind
  type_fields :: (Location -> Location_1) -> [(String, Type_8)] -> Map' Kind_1 -> Err [(String, Type_1)]
  type_fields f a b =
    case a of
      [] -> Right []
      c : d -> type_field f c b >>= \e -> (:) e <$> type_fields f d b
  type_form :: (Location -> Location_1) -> Form_1 -> Map' Kind_1 -> Err Form_2
  type_form d (Form_1 a b) c = Form_2 a <$> type_types d b c
  type_forms :: (Location -> Location_1) -> [Form_1] -> Map' Kind_1 -> Err [Form_2]
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
  type_kind :: (String, Kind_1) -> Map' Kind_1 -> Map' Kind_1
  type_kind (a, b) = Data.Map.insert a b
  type_kind_7 :: (Location -> Location_1) -> Kind_0 -> Err Kind_1
  type_kind_7 a (Kind_0 d e) =
    case e of
      Function_kind_0 f g -> Function_kind_1 <$> type_kind_7 a f <*> type_kind_7 a g
      Name_kind_0 f ->
        case Data.Set.member f hkinds of
          False -> Left ("Undefined kind " ++ f ++ location' (a d))
          True -> Right (Name_kind_1 f)
  type_kinds :: [(String, Kind_1)] -> Map' Kind_1 -> Map' Kind_1
  type_kinds a b =
    case a of
      [] -> b
      c : d -> type_kinds d (type_kind c b)
  type_kinds_0 :: (Location -> Location_1) -> [(String, Kind_0)] -> Map' Kind_1 -> Err ([(String, Kind_1)], Map' Kind_1)
  type_kinds_0 a c d =
    case c of
      [] -> Right ([], d)
      (e, f) : g -> type_kind_7 a f >>= \h -> first ((:) (e, h)) <$> type_kinds_0 a g (Data.Map.insert e h d)
  type_kinds_1 ::
    (Location -> Location_1) ->
    [(String, Kind_0)] ->
    Map' Kind_1 ->
    Map' Kind_1 ->
    Err ([(String, Kind_1)], Map' Kind_1, Map' Kind_1)
  type_kinds_1 a c d i =
    case c of
      [] -> Right ([], d, i)
      (e, f) : g ->
        (
          type_kind_7 a f >>=
          \h -> (\(j, k, l) -> ((e, h) : j, k, l)) <$> type_kinds_1 a g (Data.Map.insert e h d) (Data.Map.insert e h i))
  type_kinds_5 :: (Location -> Location_1) -> [(String, Kind_0)] -> Err [(String, Kind_1)]
  type_kinds_5 f b =
    case b of
      [] -> Right []
      (g, c) : d -> type_kind_7 f c >>= \e -> (:) (g, e) <$> type_kinds_5 f d
  type_match_algebraic ::
    (
      Map' Alg ->
      Map' String ->
      (Location -> Location_1) ->
      Integer ->
      Set String ->
      [(Type_1, Type_1)] ->
      Map' Type_2 ->
      Map' Tmatch' ->
      Match_Algebraic_1 ->
      Type_1 ->
      Map' (Either Location [Type_1]) ->
      (Location, String) ->
      Map' Type_1 ->
      [(String, (Name, Type_1))] ->
      Map' Kind_1 ->
      Map' Strct ->
      Err (Map' Tmatch', Set String, [(Type_1, Type_1)], Integer, Map' (Either Location [Type_1]), [(String, (Name, Type_1))]))
  type_match_algebraic a b c d f g h i (Match_Algebraic_1 (Name j k) l m) n o (q1, q) r a' m2 x5 =
    case Data.Map.lookup k o of
      Just p' ->
        case p' of
          Left e' -> Left (location_err' ("cases for " ++ k) (c e') (c j))
          Right p ->
            (
              type_case c (Name j k) r l p h x5 d f g >>=
              \(s0, s, w2, f4, g2) ->
                (
                  (\(t, u, v, w, b') -> (Data.Map.insert k (Tmatch' s0 t) i, u, v, w, Data.Map.insert k (Left j) o, b')) <$>
                  type_expression a b c w2 f4 g2 s m n a' m2 x5))
      Nothing ->
        Left
          (
            case Data.Map.lookup k b of
              Just _ -> "Incompatible constructors " ++ q ++ " and " ++ k ++ location (c q1) ++ " and" ++ location' (c j)
              Nothing -> "Undefined algebraic constructor " ++ k ++ location' (c j))
  type_match_int ::
    (
      Map' Alg ->
      Map' String ->
      (Location -> Location_1) ->
      Integer ->
      Set String ->
      [(Type_1, Type_1)] ->
      Map' Type_2 ->
      Map Integer Typedexpr ->
      Match_Int_1 ->
      Type_1 ->
      Map Integer Location ->
      [(String, (Name, Type_1))] ->
      Map' Kind_1 ->
      Map' Strct ->
      Err (Map Integer Typedexpr, Set String, [(Type_1, Type_1)], Integer, Map Integer Location, [(String, (Name, Type_1))]))
  type_match_int a b c d f g h i (Match_Int_1 y2 j k) l x1 a' x3 t8 =
    case Data.Map.lookup j x1 of
      Just y0 -> Left (location_err' ("cases for " ++ show j) (c y0) (c y2))
      Nothing ->
        (
          (\(m, n, o, p, b') -> (Data.Map.insert j m i, n, o, p, Data.Map.insert j y2 x1, b')) <$>
          type_expression a b c d f g h k l a' x3 t8)
  type_match_modular ::
    (
      Map' Alg ->
      Map' String ->
      (Location -> Location_1) ->
      Integer ->
      Set String ->
      [(Type_1, Type_1)] ->
      Map' Type_2 ->
      Map Integer Typedexpr ->
      Match_Modular_1 ->
      Type_1 ->
      Map Integer (Maybe Location) ->
      [(String, (Name, Type_1))] ->
      (Location, Integer) ->
      Map' Kind_1 ->
      Map' Strct ->
      Err
        (
          Map Integer Typedexpr,
          Set String,
          [(Type_1, Type_1)],
          Integer,
          Map Integer (Maybe Location),
          [(String, (Name, Type_1))]))
  type_match_modular a b c d f g h i (Match_Modular_1 t r l) m n o (p, s) x4 t8 =
    (
      check_mod c r >>=
      \(Modular' j k) ->
        if j == s
          then
            case unsafe_lookup k n of
              Just q -> Left (location_err' ("cases for " ++ show_mod j k) (c q) (c t))
              Nothing ->
                (
                  (\(u, v, w, x, z) -> (Data.Map.insert k u i, v, w, x, Data.Map.insert k (Just t) n, z)) <$>
                  type_expression a b c d f g h l m o x4 t8)
          else
            Left
              (
                "Incompatible modular types " ++
                show s ++
                " and " ++
                show j ++
                location (c p) ++
                " and" ++
                location' (c t)))
  type_matches_algebraic ::
    (
      Map' Alg ->
      Map' String ->
      (Location -> Location_1) ->
      Integer ->
      Set String ->
      [(Type_1, Type_1)] ->
      Map' Type_2 ->
      Map' Tmatch' ->
      [Match_Algebraic_1] ->
      Type_1 ->
      Map' (Either Location [Type_1]) ->
      (Location, String) ->
      Map' Type_1 ->
      [(String, (Name, Type_1))] ->
      Map' Kind_1 ->
      Map' Strct ->
      Err (Map' Tmatch', Set String, [(Type_1, Type_1)], Integer, Map' (Either Location [Type_1]), [(String, (Name, Type_1))]))
  type_matches_algebraic a b c d f g h i j k s u v a' m0 z1 =
    case j of
      [] -> Right (i, f, g, d, s, a')
      l : m ->
        (
          type_match_algebraic a b c d f g h i l k s u v a' m0 z1 >>=
          \(n, o, p, q, t, b') -> type_matches_algebraic a b c q o p h n m k t u v b' m0 z1)
  type_matches_int ::
    (
      Map' Alg ->
      Map' String ->
      (Location -> Location_1) ->
      Integer ->
      Set String ->
      [(Type_1, Type_1)] ->
      Map' Type_2 ->
      Map Integer Typedexpr ->
      [Match_Int_1] ->
      Type_1 ->
      Map Integer Location ->
      [(String, (Name, Type_1))] ->
      Map' Kind_1 ->
      Map' Strct ->
      Err (Map Integer Typedexpr, Set String, [(Type_1, Type_1)], Integer, [(String, (Name, Type_1))]))
  type_matches_int a b c d f g h i j k x1 a' m' w2 =
    case j of
      [] -> Right (i, f, g, d, a')
      l : m ->
        (
          type_match_int a b c d f g h i l k x1 a' m' w2 >>=
          \(n, o, p, q, x2, b') -> type_matches_int a b c q o p h n m k x2 b' m' w2)
  type_matches_modular ::
    (
      Map' Alg ->
      Map' String ->
      (Location -> Location_1) ->
      Integer ->
      Set String ->
      [(Type_1, Type_1)] ->
      Map' Type_2 ->
      Map Integer Typedexpr ->
      [Match_Modular_1] ->
      Type_1 ->
      Map Integer (Maybe Location) ->
      [(String, (Name, Type_1))] ->
      (Location, Integer) ->
      Map' Kind_1 ->
      Map' Strct ->
      Err
        (
          Map Integer Typedexpr,
          Set String,
          [(Type_1, Type_1)],
          Integer,
          Map Integer (Maybe Location),
          [(String, (Name, Type_1))]))
  type_matches_modular a b c d f g h i j k u l w x' w1 =
    case j of
      [] -> Right (i, f, g, d, u, l)
      m : n ->
        (
          type_match_modular a b c d f g h i m k u l w x' w1 >>=
          \(o, p, q, r, v, t) -> type_matches_modular a b c r p q h o n k v t w x' w1)
  type_method :: (Location -> Location_1) -> Method_2 -> Map' Kind_1 -> Err Method_3
  type_method a (Method_2 b c i d) e = type_kinds_0 a c e >>= \(g, h) -> Method_3 b g i <$> type_typ a d h star_kind
  type_method_1 :: String -> Map' Class_5 -> Method_3 -> Err Method_4
  type_method_1 e g (Method_3 a b c d) =
    let
      m = Prelude.foldl (\h -> \(i, j) -> Data.Map.insert i j h) Data.Map.empty b
    in
      (\(f, _, _) -> Method_4 a b f d) <$> type_constraints_0 Data.Map.empty c (g, m, (\_ -> Zr) <$> m) e
  type_methods_0 :: (Location -> Location_1) -> [Method_2] -> Map' Kind_1 -> Err [Method_3]
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
      (Location -> Location_1) ->
      Map' Strct ->
      Pat ->
      Type_1 ->
      Map' Type_2 ->
      Integer ->
      Set String ->
      [(Type_1, Type_1)] ->
      Err (Pat_1, Map' Type_2, Integer, Set String, [(Type_1, Type_1)]))
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
            (p, q, r) = typevars i (l, Data.Map.empty, n)
          in
            (
              (\(s, t, u, v, w) -> (Application_pat_1 s, t, u, v, w)) <$>
              type_pats k h f (second (repl' q) <$> j) d p r ((c, repl' q m) : o) (Name g e)))
      Blank_pat -> Right (Blank_pat_1, d, l, n, o)
      Name_pat e -> Right (Name_pat_1 e, Data.Map.insert e (Basic_type_1 [] Nothing [] c) d, l, n, o)
  type_pats ::
    (
      (Location -> Location_1) ->
      Map' Strct ->
      [Pat] ->
      [(String, Type_1)] ->
      Map' Type_2 ->
      Integer ->
      Set String ->
      [(Type_1, Type_1)] ->
      Name ->
      Err ([(String, Pat_1)], Map' Type_2, Integer, Set String, [(Type_1, Type_1)]))
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
  type_typ :: (Location -> Location_1) -> Type_8 -> Map' Kind_1 -> Kind_1 -> Err Type_1
  type_typ a (Type_8 b c) = type_type a b c
  type_type :: (Location -> Location_1) -> Location -> Type_5 -> Map' Kind_1 -> Kind_1 -> Err Type_1
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
                Function_kind_1 j k -> if k == e then Application_type_1 h <$> type_type l a g d j else x
                _ -> x)
        Name_type_5 (Name a' f) -> und_err f d "type" (l a') (\ g -> if g == e then Right (Name_type_1 f) else x)
  type_type' :: (Location -> Location_1) -> Location -> Type_5 -> Map' Kind_1 -> Err (Type_1, Kind_1)
  type_type' l a c d =
    case c of
      Application_type_5 e f ->
        (
          type_type' l a e d >>=
          \(g, h) ->
            case h of
              Function_kind_1 i j -> (\k -> (Application_type_1 g k, j)) <$> type_type l a f d i
              _ -> kind_err (l a))
      Name_type_5 (Name a' e) -> und_err e d "type" (l a') (\ f -> Right (Name_type_1 e, f))
  type_types :: (Location -> Location_1) -> [Type_8] -> Map' Kind_1 -> Err [Type_1]
  type_types f a b =
    case a of
      [] -> Right []
      c : d -> type_typ f c b star_kind >>= \e -> (:) e <$> type_types f d b
  type_types' :: (Location -> Location_1) -> Map' Kind_1 -> [(String, Type_8)] -> Err [(String, Type_1)]
  type_types' a c d =
    case d of
      [] -> Right []
      (e, f) : g -> type_typ a f c star_kind >>= \h -> (:) (e, h) <$> type_types' a c g
  types :: Map' Type_2
  types =
    Data.Map.fromList
      [
        (
          "Add",
          Basic_type_1
            [("T", star_kind)]
            (Just (Constraint_1 "Ring" "T"))
            [Constraint_1 "Ring" "T"]
            (function_type (Name_type_1 "T") (function_type (Name_type_1 "T") (Name_type_1 "T")))),
        (
          "Add_Modular",
          Basic_type_1
            [("N", nat_kind)]
            (Just (Constraint_1 "Nonzero" "N"))
            [Constraint_1 "Nonzero" "N"]
            (function_type
              (mod_type (Name_type_1 "N"))
              (function_type (mod_type (Name_type_1 "N")) (mod_type (Name_type_1 "N"))))),
        (
          "Compare",
          Basic_type_1
            [("T", star_kind)]
            (Just (Constraint_1 "Ord" "T"))
            [Constraint_1 "Ord" "T"]
            (function_type (Name_type_1 "T") (function_type (Name_type_1 "T") comparison_type))),
        (
          "Convert",
          Basic_type_1
            [("T", star_kind)]
            (Just (Constraint_1 "Ring" "T"))
            [Constraint_1 "Ring" "T"]
            (function_type int_type (Name_type_1 "T"))),
        (
          "Convert_Modular",
          Basic_type_1
            [("N", nat_kind)]
            (Just (Constraint_1 "Nonzero" "N"))
            [Constraint_1 "Nonzero" "N"]
            (function_type int_type (mod_type (Name_type_1 "N")))),
        ("Crash", Basic_type_1 [("T", star_kind)] Nothing [] (Name_type_1 "T")),
        ("Div", Basic_type_1 [] Nothing [] (function_type int_type (function_type int_type (maybe_type int_type)))),
        (
          "Div'",
          Basic_type_1
            [("N", nat_kind)]
            (Just (Constraint_1 "Nonzero" "N"))
            [Constraint_1 "Nonzero" "N"]
            (function_type int_type int_type)),
        ("EQ", Basic_type_1 [] Nothing [] comparison_type),
        ("GT", Basic_type_1 [] Nothing [] comparison_type),
        (
          "Inverse_Modular",
          Basic_type_1
            [("N", nat_kind)]
            (Just (Constraint_1 "Nonzero" "N"))
            [Constraint_1 "Nonzero" "N"]
            (function_type (mod_type (Name_type_1 "N")) (maybe_type (mod_type (Name_type_1 "N"))))),
        ("LT", Basic_type_1 [] Nothing [] comparison_type),
        ("Mod", Basic_type_1 [] Nothing [] (function_type int_type (function_type int_type int_type))),
        (
          "Multiply",
          Basic_type_1
            [("T", star_kind)]
            (Just (Constraint_1 "Ring" "T"))
            [Constraint_1 "Ring" "T"]
            (function_type (Name_type_1 "T") (function_type (Name_type_1 "T") (Name_type_1 "T")))),
        (
          "Multiply_Modular",
          Basic_type_1
            [("N", nat_kind)]
            (Just (Constraint_1 "Nonzero" "N"))
            [Constraint_1 "Nonzero" "N"]
            (function_type
              (mod_type (Name_type_1 "N"))
              (function_type (mod_type (Name_type_1 "N")) (mod_type (Name_type_1 "N"))))),
        (
          "Negate",
          Basic_type_1
            [("T", star_kind)]
            (Just (Constraint_1 "Ring" "T"))
            [Constraint_1 "Ring" "T"]
            (function_type (Name_type_1 "T") (Name_type_1 "T"))),
        (
          "Negate_Modular",
          Basic_type_1
            [("N", nat_kind)]
            (Just (Constraint_1 "Nonzero" "N"))
            [Constraint_1 "Nonzero" "N"]
            (function_type (mod_type (Name_type_1 "N")) (mod_type (Name_type_1 "N")))),
        ("Next", Basic_type_1 [] Nothing [] (function_type nat_type nat_type)),
        ("Nothing", Basic_type_1 [("T", star_kind)] Nothing [] (maybe_type (Name_type_1 "T"))),
        ("Wrap", Basic_type_1 [("T", star_kind)] Nothing [] (function_type (Name_type_1 "T") (maybe_type (Name_type_1 "T")))),
        ("Zr", Basic_type_1 [] Nothing [] nat_type)]
  typestring :: Type_1 -> [Type_1] -> (String, [Type_1])
  typestring a d =
    case a of
      Application_type_1 b c -> typestring b (c : d)
      Name_type_1 b -> (b, d)
  typevar :: String -> (Integer, Map' Type_1, Set String) -> (Integer, Map' Type_1, Set String)
  typevar a (c, e, f) =
    let
      d = show c
    in
      (c + 1, Data.Map.insert a (Name_type_1 d) e, Data.Set.insert d f)
  typevars :: [(String, Kind_1)] -> (Integer, Map' Type_1, Set String) -> (Integer, Map' Type_1, Set String)
  typevars a b =
    case a of
      [] -> b
      (c, _) : d -> typevars d (typevar c b)
  typevars' ::
    (
      (Location -> Location_1) ->
      Map' Kind_1 ->
      (String -> Err (Map' Type_1)) ->
      [(String, Kind_1)] ->
      [Type_8] ->
      Map' Type_1 ->
      Err (Map' Type_1))
  typevars' l j a b c d =
    case b of
      [] ->
        case c of
          [] -> Right d
          _ -> a "many"
      (e, f) : g ->
        case c of
          [] -> a "few"
          h : i -> type_typ l h j f >>= \m -> typevars' l j a g i (Data.Map.insert e m d)
  typing ::
    String ->
    Tree_5 ->
    (File, Map' Expression_2, Map' Kind_1, Map' (Map' Location'), Map' ([String], Map' [(String, Nat)])) ->
    Err (File, Map' Expression_2, Map' Kind_1, Map' (Map' Location'), Map' ([String], Map' [(String, Nat)]))
  typing k (Tree_5 a a' x7 c) (File d t u v b' c5 x t2 k7, l, m, m', n4) =
    (
      type_datas (Location_1 k) a (old d, old t, old u, old v, l, m, old k7) >>=
      \(e, b, h, g, f, n, k8) ->
        (
          type_classes k (fst <$> e) a' (old b', m', g, n4, old t2, old c5) >>=
          \(c', m2, g0, x1, x2, t3) ->
            (
              (\(i, j, n', y, y2) ->
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
                  n,
                  n',
                  y2)) <$>
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
                x1
                (fst <$> k8))))
  unsafe_lookup :: Ord t => t -> Map t u -> u
  unsafe_lookup a b =
    case Data.Map.lookup a b of
      Just c -> c
      Nothing -> undefined
  ziphelp :: (Location -> Location_1) -> String -> Location -> [Kind_0] -> Err ()
  ziphelp l f a c =
    case c of
      [] -> Right ()
      _ -> Left ("Wrong number of kind arguments for " ++ f ++ location' (l a))