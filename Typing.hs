{-
let expression (sequential? or more powerful, like in Haskell?)
protection against duplicate file loading - what happens now? if crashes - fix, give a nice error/warning. if nothing - warn?
implement all necessary operations for ints and bools
tests
type synonyms?
operators
type operators (minimally for functions, either, pair).
abstract methods
move duplicate instance control into Naming module?
if-elif-else?
eta reduction warnings
unused type variable warnings
unused local variable warnings
add something for easily changing fields of structs?
internal: do something with old/new status tags. check where exactly they're necessary. get rid of them where they're useless
change semantics of missing pattern-match variables from blank to lambda? (Left -> e is not Left _ -> e but Left x -> e x)
internal: make the system of specifying built-in algebraic data types and things better and safer
Allow hiding things to functions outside module - so that helper functions are not exported from the module
normalising constructors for some data types (polynomial, fraction) which assume a certain normal form of fields?
allow to hide (prevent exporting) constructors and field accessors which can potentially have bad behavior
internal: remove locations from expressions except from lowest-level things where some checks are necessary (name)?
switch expression that is less strict and more flexible than match?
some limited pattern matching in function arguments (and maybe also variables introduced through algebraic matching?)
syntactic sugar for lists, vectors, matrices... allow writing (tiny, limited to expression parsing) language extensions?
show and read
basic IO operations (output to console, read file, write file, append to file)
finite n
compilation
nice human-readable output for interpreter through show
boolean function library
parsing library
implement map and set (AVL trees?)
make match work with finite and char
different ways of folding lists, vectors, sets, maps etc
gather naming and type errors and give a list instead of returning only the first one?
enrich kind system via promotion
make match work with chars
make promotion for built-in ADT-s automatic
modify parser: make promotion of ints and chars to type level explicit (with !)
simplify hyperkind system because there's no need to keep anything but the number of arguments
modify flexible type variable name generation. use just numbers for everything? ("T0" - maybe name conflict with userdefined)
module system related functions into a separate file?
more detailed type errors (write which two types clashed)
type clash location?
write in error why exactly you need a class constraint (which name of a function caused it)?
write in error where the need for class constraint occurred?
make command line arguments nicer
check for free type variables after typechecking an expression. throw an error (they are a problem with type classes!)
remove promotion of primitives? it seems that without GADT-s they are pointless?
prettyprint evaluation results
case reduction warning (case x of c1 -> f y, c2 -> g y on sama mis (case x of c1 -> f, c2 -> g) y)
mis juhtub kui esimeses moodulis on kusagil tüübimuutuja T ja järgmises moodulis sama nimega globaalne tüüp?
-}
{-
    error("Internal compiler error. Free type variable after type application when trying to derive type.")
-}
-----------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Typing where
  import Control.Monad
  import Data.Bifunctor
  import Data.List
  import Data.Map
  import Data.Set
  import Naming
  import Standard
  import Tokenise
  import Tree
  data Alg = Alg [(String, Kind_1)] (Map' [Type_1]) Type_1 deriving Show -- TODO: REM STRINGS FROM FST MAP
  type Algebraics = Map' (Alg, Status)
  data Brnch_3 = Brnch_3 String [(String, Kind_1)] String [(String, Type_0)] deriving Show
  data Class_3 = Class_3 (String, Kind_1) (Maybe String) [Method_3] deriving Show
  data Class_4 = Class_4 String (String, Kind_1) (Maybe Name) [Method_3] deriving Show
  data Constraint_1 = Constraint_1 String String deriving Show
  type Constrs = Map' (String, Status)
  data Data_3 = Data_3 String Data_br_3 deriving Show
  data Data_br_3 =
    Branching_data_3 String [Kind_1] [(String, Kind_1)] [Brnch_3] | Plain_data_3 [(String, Kind_1)] Data_branch_1
      deriving Show
  data Def_4 =
    Basic_def_4 Location_0 String [(String, Kind_1)] [Constraint_1] Type_1 Expression_1 [String] |
    Instance_4
      String
      String
      [(String, Kind_1)]
      Integer
      [(Name, Expression_1, [(String, Kind_1)], Type_1)]
      Type_1
      [Constraint_1]
      [String]
        deriving Show
  data Expression_2 =
    Add_Int_expression_2 |
    Add_Int'_expression_2 Integer |
    Algebraic_expression_2 String [Expression_2] |
    Application_expression_2 Expression_2 Expression_2 |
    Char_expression_2 Char |
    Compare_Int_expression_2 |
    Compare_Int'_expression_2 Integer |
    Crash_expression_2 |
    Field_expression_2 String |
    Function_expression_2 Pattern_0 Expression_2 |
    Int_expression_2 Integer |
    Match_expression_2 Expression_2 Matches_2 |
    Mod_Int_expression_2 |
    Mod_Int'_expression_2 Integer |
    Multiply_Int_expression_2 |
    Multiply_Int'_expression_2 Integer |
    Name_expression_2 String |
    Negate_Int_expression_2 |
    Struct_expression_2 (Map' Expression_2)
      deriving Show
  data File =
    File
      (Map' Polykind)
      (Map' Alg)
      (Map' String)
      (Map' Type_2)
      (Map' Kind)
      (Map' Bool)
      (Map' Class_3)
      (Map' (Map' [[String]]))
      (Map' Kind_1)
      (Map' Prom_alg)
        deriving Show
  data Form_2 = Form_2 String [Type_1] deriving Show
  data Kind = Arrow_kind Kind Kind | Star_kind deriving (Eq, Show)
  data Kind_1 = Application_kind_1 Kind_1 Kind_1 | Name_kind_1 String deriving (Eq, Show)
  data Match_Algebraic_2 = Match_Algebraic_2 [Pattern_0] Expression_2 deriving Show
  data Matches_2 =
    Matches_Algebraic_2 (Map' Match_Algebraic_2) (Maybe Expression_2) |
    Matches_char_2 (Map Char Expression_2) Expression_2 |
    Matches_Int_2 (Map Integer Expression_2) Expression_2
      deriving Show
  data Method_3 = Method_3 String [(String, Kind_1)] Type_1 deriving Show
  data Nat = Nxt Nat | Zr deriving Show
  data Plain_dat = Plain_dat String [String] Data_branch_1 deriving Show
  data Polykind = Polykind [String] Kind_1 deriving Show
  data Prom_alg = Prom_alg [String] (Map' [Kind_1]) deriving Show
  data Status = New | Old deriving (Eq, Show)
  data Type_1 = Application_type_1 Type_1 Type_1 | Char_type_1 Char | Int_type_1 Integer | Name_type_1 String [Kind_1]
    deriving (Eq, Show)
  data Type_2 = Basic_type_1 [(String, Kind_1)] (Maybe Constraint_1) [Constraint_1] Type_1 deriving Show
  data Tmatch' = Tmatch' [Pattern_0] Typedexpr deriving Show
  data Typedexpr =
    Application_texpr Typedexpr Typedexpr |
    Char_texpr Char |
    Function_texpr Pattern_0 Typedexpr |
    Int_texpr Integer |
    Match_texpr Typedexpr Typedmatches |
    Name_texpr_0 String String Type_1 |
    Name_texpr_1 String [(String, Type_1)]
      deriving Show
  data Typedmatches =
    Tmatch_algebraic (Map' Tmatch') (Maybe Typedexpr) |
    Tmatch_char (Map Char Typedexpr) Typedexpr |
    Tmatch_int (Map Integer Typedexpr) Typedexpr
      deriving Show
  type Types = Map' (Type_2, Status)
  addargs :: Map' Class_3 -> Map' ([String], Map' [(String, Nat)]) -> Typedexpr -> Expression_2
  addargs a b c =
    let
      h = addargs a b
    in
      case c of
        Application_texpr d e -> Application_expression_2 (h d) (h e)
        Char_texpr d -> Char_expression_2 d
        Function_texpr d e -> Function_expression_2 d (h e)
        Int_texpr d -> Int_expression_2 d
        Match_texpr d e ->
          Match_expression_2
            (h d)
            (case e of
              Tmatch_algebraic f g -> Matches_Algebraic_2 ((\(Tmatch' i j) -> Match_Algebraic_2 i (h j)) <$> f) (h <$> g)
              Tmatch_char f g -> Matches_char_2 (h <$> f) (h g)
              Tmatch_int f g -> Matches_Int_2 (h <$> f) (h g))
        Name_texpr_0 d e f ->
          let
            (g, i) = typestring f []
          in
            addargs_2
              a
              b
              (
                second (getarg i) <$>
                case Data.Map.lookup g (snd (unsafe_lookup e b)) of
                  Just k -> k
                  Nothing -> [])
              (Name_expression_2 (d ++ " " ++ g))
        Name_texpr_1 d e -> addargs_2 a b e (Name_expression_2 d)
  addargs_0 ::
    Map' Class_3 -> Map' ([String], Map' [(String, Nat)]) -> String -> String -> [Type_1] -> Expression_2 -> Expression_2
  addargs_0 a b d e f g =
    let
      Class_3 _ h _ = unsafe_lookup d a
    in
      addargs_1
        a
        b
        d
        e
        f
        (case h of
          Just i -> addargs_0 a b i e f g
          Nothing -> g)
  addargs_1 ::
    Map' Class_3 -> Map' ([String], Map' [(String, Nat)]) -> String -> String -> [Type_1] -> Expression_2 -> Expression_2
  addargs_1 a c d e f g =
    let
      (h, i) = unsafe_lookup d c
    in
      Prelude.foldl
        (\j -> \k ->
          Application_expression_2
            j
            (addargs_2
              a
              c
              (
                second (getarg f) <$>
                case Data.Map.lookup e i of
                  Just l -> l
                  Nothing -> [])
              (Name_expression_2 (k ++ " " ++ e))))
        g
        h
  addargs_2 :: Map' Class_3 -> Map' ([String], Map' [(String, Nat)]) -> [(String, Type_1)] -> Expression_2 -> Expression_2
  addargs_2 a b d e =
    case d of
      [] -> e
      (f, g) : h ->
        let
          (c, i) = typestring g []
        in
          addargs_2 a b h (addargs_0 a b f c i e)
  algebraics :: Map' Alg
  algebraics = Data.Map.fromList (second (\(a, b, c) -> (Alg a (Data.Map.fromList b) c)) <$> algebraics')
  algebraics' :: [(String, ([(String, Kind_1)], [(String, [Type_1])], Type_1))]
  algebraics' =
    (
      (\(a, (b, c)) ->
        (a, ((\d -> (d, star_kind)) <$> b, c, Prelude.foldl (\d -> \e -> Application_type_1 d (ntype e)) (ntype a) b))) <$>
      algebraics'')
  algebraics'' :: [(String, ([String], [(String, [Type_1])]))]
  algebraics'' =
    [
      ("Comparison", ([], [("EQ", []), ("GT", []), ("LT", [])])),
      ("Maybe", (["T"], [("Nothing", []), ("Wrap", [Name_type_1 "T" []])])),
      ("Nat", ([], [("Next", [Name_type_1 "Nat" []]), ("Zr", [])]))]
  arrow_kind :: Kind_1 -> Kind_1 -> Kind_1
  arrow_kind a = Application_kind_1 (Application_kind_1 (Name_kind_1 "Arrow") a)
  chain_constraints ::
    Maybe String ->
    Map' Class_3 ->
    Map' (Map' (Maybe String, Location_0)) ->
    String ->
    String ->
    Location_0 ->
    String ->
    [String] ->
    Err (Map' (Map' (Maybe String, Location_0)), [String])
  chain_constraints a b c e h i j l =
    case a of
      Just d ->
        let
          Class_3 _ f o = unsafe_lookup d b
          m n = chain_constraints f b (Data.Map.insert d n c) e h i j (((\(Method_3 u _ _) -> u ++ " " ++ e) <$> o) ++ l)
        in
          case Data.Map.lookup d c of
            Just g -> case Data.Map.lookup e g of
              Just (_, k) ->
                Left (
                  "Constraint " ++
                  d ++
                  " " ++
                  e ++
                  " in " ++
                  j ++
                  " at " ++
                  location0 k ++
                  " overlaps with constraint " ++
                  h ++
                  " " ++
                  e ++
                  " at " ++
                  location0 i ++
                  ".")
              Nothing -> m (Data.Map.insert e (Just h, i) g)
            Nothing -> m (Data.Map.singleton e (Just h, i))
      Nothing -> Right (c, l)
  char_kind :: Kind_1
  char_kind = Name_kind_1 "!Char"
  char_type :: Type_1
  char_type = ntype "Char"
  check_kind :: String -> String -> Map' (Either Polykind Kind_1) -> Type_1 -> Err Kind_1
  check_kind j c a b =
    let
      x = Left j
    in case b of
      Application_type_1 d e -> check_kind j c a d >>= \f -> case f of
        Application_kind_1 (Application_kind_1 (Name_kind_1 "Arrow") g) h ->
          check_kind j c a e >>= \i -> if i == g then Right h else x
        _ -> x
      Char_type_1 _ -> Right char_kind
      Int_type_1 _ -> Right int_kind
-- TODO: are more checks necessary? is it possible that there are problems and potential crash in Name_type_1 case?
      Name_type_1 d e -> if d == c then x else
        let
          (f, g) = check_kind' (unsafe_lookup d a)
        in Right (repkinds (Data.Map.fromList (zip f e)) g)
  check_kind' :: Either Polykind Kind_1 -> ([String], Kind_1)
  check_kind' a = case a of
    Left (Polykind c d) -> (c, d)
    Right b -> ([], b)
  comparison_kind :: Kind_1
  comparison_kind = Name_kind_1 "!Comparison"
  comparison_type :: Type_1
  comparison_type = ntype "Comparison"
  constrs :: Map' String
  constrs = Data.Map.fromList (join ((\(a, (Alg _ b _)) -> (\c -> (c, a)) <$> keys b) <$> assocs algebraics))
  context_union :: File -> File -> File
  context_union (File b i j d a x e t g o) (File f k l h c y m u n p) =
    File
      (Data.Map.union b f)
      (Data.Map.union i k)
      (Data.Map.union j l)
      (Data.Map.union d h)
      (Data.Map.union a c)
      (Data.Map.union x y)
      (Data.Map.union e m)
      (unionWith Data.Map.union t u)
      (Data.Map.union g n)
      (Data.Map.union o p)
  defs :: Map' Expression_2
  defs = fst <$> defs_and_types
  defs_and_types :: Map' (Expression_2, Type_2)
  defs_and_types =
    Data.Map.fromList
      (
        defs_and_types' ++
        join
          (
            (\(_, (b, c, d)) ->
              (\(e, f) -> (e, (type_alg f e, Basic_type_1 b Nothing [] (Prelude.foldr function_type d f)))) <$> c) <$>
            algebraics'))
  defs_and_types' :: [(String, (Expression_2, Type_2))]
  defs_and_types' =
    [
      (
        "Add_Int",
        (Add_Int_expression_2, Basic_type_1 [] Nothing [] (function_type int_type (function_type int_type int_type)))),
      (
        "Compare_Int",
        (
          Compare_Int_expression_2,
          Basic_type_1 [] Nothing [] (function_type int_type (function_type int_type comparison_type)))),
      ("Crash", (Crash_expression_2, Basic_type_1 [("T", star_kind)] Nothing [] (ntype "T"))),
      (
        "Mod_Int",
        (Mod_Int_expression_2, Basic_type_1 [] Nothing [] (function_type int_type (function_type int_type int_type)))),
      (
        "Multiply_Int",
        (Multiply_Int_expression_2, Basic_type_1 [] Nothing [] (function_type int_type (function_type int_type int_type)))),
      ("Negate_Int", (Negate_Int_expression_2, Basic_type_1 [] Nothing [] (function_type int_type int_type)))]
  find_and_delete :: Ord t => Map t u -> t -> Maybe (u, Map t u)
  find_and_delete a b = (\c -> (c, Data.Map.delete b a)) <$> Data.Map.lookup b a
  function_type :: Type_1 -> Type_1 -> Type_1
  function_type a = Application_type_1 (Application_type_1 (ntype "Function") a)
  gather_all_types :: (Ord u, Monad f) => (t -> Map u v -> f (Map u v)) -> [t] -> Map u v -> f (Map u v)
  gather_all_types a b c = case b of
    [] -> return c
    d : e -> gather_all_types a e c >>= a d
  gather_fields :: Set String -> [(String, Type_0)] -> Map' Location_0 -> Maybe (Map' Location_0)
  gather_fields b a = gather_types b (snd <$> a)
  gather_form :: Set String -> Form_1 -> Map' Location_0 -> Maybe (Map' Location_0)
  gather_form b (Form_1 _ a) = gather_types b a
  gather_forms :: Set String -> [Form_1] -> Map' Location_0 -> Maybe (Map' Location_0)
  gather_forms a = gather_all_types (gather_form a)
  gather_type :: Set String -> Type_0 -> Map' Location_0 -> Maybe (Map' Location_0)
  gather_type f (Type_0 a b) c = case b of
    Application_type_0 d e -> gather_type f e c >>= gather_type f d
    Name_type_0 d e -> case e of
      [] -> Just (if Data.Set.member d f then c else Data.Map.insert d a c)
      _ -> Nothing
    _ -> Nothing
  gather_types :: Set String -> [Type_0] -> Map' Location_0 -> Maybe (Map' Location_0)
  gather_types a = gather_all_types (gather_type a)
  get_args :: Err (String, [Type_1]) -> Type_1 -> [Type_1] -> Err (String, [Type_1])
  get_args e a b = case a of
    Application_type_1 c d -> get_args e c (d : b)
    Name_type_1 c d ->
      case d of
        [] -> Right (c, b)
        _ -> e
    _ -> e
  getarg :: [t] -> Nat -> t
  getarg a b = case a of
    [] -> undefined
    c : d -> case b of
      Nxt e -> getarg d e
      Zr -> c
  hkinds :: Map' Kind
  hkinds =
    Data.Map.fromList
      [
        ("Arrow", Arrow_kind Star_kind (Arrow_kind Star_kind Star_kind)),
        ("!Char", Star_kind),
        ("!Comparison", Star_kind),
        ("!Int", Star_kind),
        ("Star", Star_kind)]
  init_type_context :: File
  init_type_context =
    File
      kinds
      algebraics
      constrs
      (snd <$> defs_and_types)
      hkinds
      promotables
      Data.Map.empty
      Data.Map.empty
      Data.Map.empty
      prom_algs
  ins_new :: Ord t => t -> u -> Map t (u, Status) -> Map t (u, Status)
  ins_new a b = Data.Map.insert a (b, New)
  int_kind :: Kind_1
  int_kind = Name_kind_1 "!Int"
  int_type :: Type_1
  int_type = ntype "Int"
  isLeft :: Either t u -> Bool
  isLeft a = case a of
    Left _ -> True
    Right _ -> False
  kind_err :: Location_1 -> Err t
  kind_err a = Left ("Kind mismatch" ++ location' a)
  kinds :: Map' Polykind
  kinds =
    (
      pkind <$>
      Data.Map.fromList (kinds' ++ (second (\(a, _, _) -> Prelude.foldr arrow_kind star_kind (snd <$> a)) <$> algebraics')))
  kinds' :: [(String, Kind_1)]
  kinds' =
    [
      ("Char", star_kind),
      ("!EQ", comparison_kind),
      ("Function", arrow_kind star_kind (arrow_kind star_kind star_kind)),
      ("!GT", comparison_kind),
      ("Int", star_kind),
      ("!LT", comparison_kind)]
  location_err' :: String -> Location_1 -> Location_1 -> String
  location_err' a b = location_err a (Library b)
  locations :: Locations
  locations =
    Data.Map.fromList
      (flip (,) (Language) <$> (Data.List.filter not_promoted (keys hkinds ++ keys kinds) ++ keys defs_and_types))
  make_eq :: Data_2 -> Map' (Either Bool (Map' Location_0), Status) -> Map' (Either Bool (Map' Location_0), Status)
  make_eq (Data_2 (Name _ a) b') =
    case b' of
      Branching_data_2 _ _ _ _ -> ins_new a (Left False)
      Plain_data_2 b c ->
        ins_new a (case promotable b (Data.Set.fromList [a]) of
          Just d -> case (case c of
            Algebraic_data_1 e -> gather_forms d e
            Struct_data_1 e -> gather_fields d e) Data.Map.empty of
              Just e -> Right e
              Nothing -> Left False
          Nothing -> Left False)
  make_eqs :: [Data_2] -> Map' (Either Bool (Map' Location_0), Status) -> Map' (Either Bool (Map' Location_0), Status)
  make_eqs a b = case a of
    [] -> b
    c : d -> make_eqs d (make_eq c b)
{-
  maybe_type :: Type_1 -> Type_1
  maybe_type = Application_type_1 (Name_type_1 "Maybe" [])
-}
  naming_typing ::
    String ->
    Tree_2 ->
    (Locations, File, Map' Expression_2, Map' Polykind, Map' (Map' Location'), Map' ([String], Map' [(String, Nat)])) ->
    Err (Locations, File, Map' Expression_2, Map' Polykind, Map' (Map' Location'), Map' ([String], Map' [(String, Nat)]))
  naming_typing f a (b, c, g, j, m, w) =
    naming f a b >>= \(d, e) -> (\(h, i, k, n, u) -> (d, h, i, k, n, u)) <$> typing f e (c, g, j, m, w)
  not_promoted :: String -> Bool
  not_promoted a = case a of
    '!' : _ -> False
    _ -> True
  ntype :: String -> Type_1
  ntype a = Name_type_1 a []
  old :: Map' t -> Map' (t, Status)
  old = (<$>) (flip (,) Old)
  old' :: Map' (Map' t) -> Map' (Map' (t, Status))
  old' = (<$>) old
  pkind :: Kind_1 -> Polykind
  pkind = Polykind []
  prom_algs :: Map' Prom_alg
  prom_algs =
    Data.Map.fromList
      (
        bimap
          ((:) '!')
          (\(a, b) -> Prom_alg a ((<$>) (prom_type (Data.Set.fromList a)) <$> Data.Map.fromList (first ((:) '!') <$> b))) <$>
        algebraics'')
  prom_type :: Set String -> Type_1 -> Kind_1
  prom_type d a =
    let
      f = prom_type d
    in case a of
      Application_type_1 b c -> Application_kind_1 (f b) (f c)
      Name_type_1 b _ -> Name_kind_1 (if Data.Set.member b d then b else '!' : b)
      _ -> undefined
  promotable :: [(String, Kind_0)] -> Set String -> Maybe (Set String)
  promotable a b = case a of
    [] -> Just b
    (c, d) : e -> case promotable' d of
      False -> Nothing
      True -> promotable e (Data.Set.insert c b)
  promotable' :: Kind_0 -> Bool
  promotable' (Kind_0 _ a) = a == Name_kind_0 "Star"
  promotables :: Map' Bool
  promotables = Data.Map.fromList ((\a -> (a, True)) <$> ["Char", "Comparison", "Int", "Nat"])
  rem_old :: Map' (t, Status) -> Map' t
  rem_old a = fst <$> Data.Map.filter (\(_, b) -> b == New) a
  rem_old' :: Map' (Map' (t, Status)) -> Map' (Map' t)
  rem_old' a = Data.Map.filter (\b -> not (Data.Map.null b)) (rem_old <$> a)
  repkinds :: Map' Kind_1 -> Kind_1 -> Kind_1
  repkinds a b = case b of
    Application_kind_1 c d -> Application_kind_1 (repkinds a c) (repkinds a d)
    Name_kind_1 c -> case Data.Map.lookup c a of
      Just d -> d
      Nothing -> b
  repl :: Map' String -> Type_1 -> Type_1
  repl a b = case b of
    Application_type_1 c d -> Application_type_1 (repl a c) (repl a d)
    Name_type_1 c e -> Name_type_1 (case Data.Map.lookup c a of
      Just d -> d
      Nothing -> c) e
    _ -> b
  slv :: Map' (Map' [[String]]) -> [(String, Type_1)] -> (String -> String) -> Err ()
  slv a b h =
    case b of
      [] -> Right ()
      (c, d) : e ->
        let
          i = h c
        in
          get_args (Left i) d [] >>= \(f, g) -> case Data.Map.lookup f (unsafe_lookup c a) of
            Just j -> slv_constrs a e h g j
            Nothing -> Left i
  slv_constrs :: Map' (Map' [[String]]) -> [(String, Type_1)] -> (String -> String) -> [Type_1] -> [[String]] -> Err ()
  slv_constrs a b c d e = case d of
    [] -> case e of
      [] -> slv a b c
      _ -> undefined
    f : g -> case e of
      [] -> undefined
      h : i -> slv_constrs a (((\j -> (j, f)) <$> h) ++ b) c g i
  solve_diff :: Ord t => Map t u -> [t] -> Map t u
  solve_diff a b = case b of
    [] -> a
    c : d -> solve_diff (Data.Map.delete c a) d
  solve_eq ::
    (Location_0 -> Location_1) ->
    String ->
    Map' (Either Bool (Map' Location_0), Status) ->
    Err (Bool, Map' (Either Bool (Map' Location_0), Status))
  solve_eq f a b = case fst (unsafe_lookup a b) of
    Left d -> Right (d, b)
    Right d -> (\e -> (e, ins_new a (Left e) b)) <$> solve_eq_help f [a] b d
  solve_eq_help ::
    (Location_0 -> Location_1) -> [String] -> Map' (Either Bool (Map' Location_0), Status) -> Map' Location_0 -> Err Bool
  solve_eq_help h a b c = case minViewWithKey c of
    Just ((d, e), f) -> und_err d b "type" (h e) (\g -> case fst g of
      Left i -> Right i
      Right i -> solve_eq_help h (d : a) b (Data.Map.union f (solve_diff i a)))
    Nothing -> Right True
  solvesys ::
    String ->
    Map' (Either Polykind Kind_1) ->
    [(Type_1, Type_1)] ->
    ([(String, Type_1)], Typedexpr) ->
    Err ([(String, Type_1)], Typedexpr)
  solvesys m a b a' =
    let
      x = Left m
    in case b of
      [] -> Right a'
      (c, d) : g -> case c of
        Application_type_1 e f -> case d of
          Application_type_1 h i -> solvesys m a ((e, h) : (f, i) : g) a'
          Name_type_1 h _ -> solvesys' m a h c g a'
          _ -> x
        Char_type_1 e -> case d of
          Char_type_1 f -> if e == f then solvesys m a g a' else x
          Name_type_1 f _ -> solvesys' m a f c g a'
          _ -> x
        Int_type_1 e -> case d of
          Int_type_1 f -> if e == f then solvesys m a g a' else x
          Name_type_1 f _ -> solvesys' m a f c g a'
          _ -> x
        Name_type_1 e v -> case d of
          Name_type_1 f w ->
            let
              n = unsafe_lookup f a
            in case unsafe_lookup e a of
              Left t -> case n of
                Left _ -> if c == d then solvesys m a g a' else x
                Right u -> solvesys_names m a f u e v t g a'
              Right t -> case n of
                Left u -> solvesys_names m a e t f w u g a'
                Right u -> if t == u then solvesys_rep m a e d g a' else x
          _ -> solvesys' m a e d g a'
  solvesys' ::
    String ->
    Map' (Either Polykind Kind_1) ->
    String ->
    Type_1 ->
    [(Type_1, Type_1)] ->
    ([(String, Type_1)], Typedexpr) ->
    Err ([(String, Type_1)], Typedexpr)
  solvesys' h a b c d x = case unsafe_lookup b a of
    Left _ -> Left h
    Right e -> check_kind h b a c >>= \g -> if g == e then solvesys_rep h a b c d x else Left h
-- TODO: is it necessary to check here that hyperkinds of [Kind_1] arguments match the hyperkinds of variables in Polykind?
  solvesys_names ::
    String ->
    Map' (Either Polykind Kind_1) ->
    String ->
    Kind_1 ->
    String ->
    [Kind_1] ->
    Polykind ->
    [(Type_1, Type_1)] ->
    ([(String, Type_1)], Typedexpr) ->
    Err ([(String, Type_1)], Typedexpr)
  solvesys_names a b c d e f (Polykind h i) j k =
    if d == repkinds (Data.Map.fromList (zip h f)) i then solvesys_rep a b c (Name_type_1 e f) j k else Left a
  solvesys_rep ::
    String ->
    Map' (Either Polykind Kind_1) ->
    String ->
    Type_1 ->
    [(Type_1, Type_1)] ->
    ([(String, Type_1)], Typedexpr) ->
    Err ([(String, Type_1)], Typedexpr)
-- todo: the variable that was replaced can be removed from the context!
  solvesys_rep a b c d e (x, f) = solvesys a b (sysrep c d e) (second (sysrep' c d) <$> x, sysrep2 c d f)
  star_kind :: Kind_1
  star_kind = Name_kind_1 "Star"
  sysrep :: String -> Type_1 -> [(Type_1, Type_1)] -> [(Type_1, Type_1)]
  sysrep a b =
    let
      c = sysrep' a b
    in
      (<$>) (bimap c c)
  sysrep' :: String -> Type_1 -> Type_1 -> Type_1
  sysrep' a b c =
    let
      f = sysrep' a b
    in case c of
      Application_type_1 d e -> Application_type_1 (f d) (f e)
      Name_type_1 d _ -> if d == a then b else c
      _ -> c
  sysrep2 :: String -> Type_1 -> Typedexpr -> Typedexpr
  sysrep2 a b c =
    let
      f = sysrep2 a b
    in case c of
      Application_texpr d e -> Application_texpr (f d) (f e)
      Function_texpr d e -> Function_texpr d (f e)
      Match_texpr d e -> Match_texpr (f d) (case e of
        Tmatch_algebraic g h -> Tmatch_algebraic ((\(Tmatch' i j) -> Tmatch' i (f j)) <$> g) (f <$> h)
        Tmatch_char g h -> Tmatch_char (f <$> g) (f h)
        Tmatch_int g h -> Tmatch_int (f <$> g) (f h))
      Name_texpr_0 d g e -> Name_texpr_0 d g (sysrep' a b e)
      Name_texpr_1 d e -> Name_texpr_1 d (second (sysrep' a b) <$> e)
      _ -> c
  type_alg :: [t] -> String -> Expression_2
  type_alg a b =
    let
      c = show <$> [0 .. length a - 1]
    in Prelude.foldr Function_expression_2 (Algebraic_expression_2 b (Name_expression_2 <$> c)) (Name_pattern <$> c)
  type_branching ::
    (
      (Location_0 -> Location_1) ->
      String ->
      Map' (Either Location_0 [Kind_1]) ->
      Brnch_2 ->
      Err (Map' (Either Location_0 [Kind_1]), Brnch_3))
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
  type_branching_1 ::
    (Location_0 -> Location_1) -> (Map' Kind, Map' Polykind) -> Types -> String -> [(String, Kind_1)] -> Brnch_3 -> Err Types
  type_branching_1 f (a, q) g n o (Brnch_3 b c d e) =
    (
      (\h ->
        let
          p = o ++ c
          l =
            Prelude.foldl
              (\i -> \(j, _) -> Application_type_1 i (ntype j))
              (Application_type_1 (ntype n) (Prelude.foldl (\i -> \(j, _) -> Application_type_1 i (ntype j)) (ntype b) c))
              p
          m = Basic_type_1 p Nothing []
        in
          Prelude.foldl
            (\i -> \(j, k) -> ins_new j (m (function_type l k)) i)
            (ins_new d (m (Prelude.foldr (\(_, i) -> function_type i) l h)) g)
            h) <$>
      type_types' f (a, Prelude.foldl (\i -> \(j, k) -> Data.Map.insert j (pkind k) i) q c) e)
  type_branchings ::
    (
      (Location_0 -> Location_1) ->
      String ->
      Location_0 ->
      String ->
      Map' (Either Location_0 [Kind_1]) ->
      [Brnch_2] ->
      Err [Brnch_3])
  type_branchings e h i j f a =
    case a of
      [] -> if all isLeft f then Right [] else Left ("Incomplete match in branching data type " ++ h ++ location' (e i))
      b : c -> type_branching e j f b >>= \(g, d) -> (:) d <$> type_branchings e h i j g c
  type_branchings_1 ::
    (
      (Location_0 -> Location_1) ->
      (Map' Kind, Map' Polykind) ->
      Types ->
      String ->
      [(String, Kind_1)] ->
      [Brnch_3] ->
      Err Types)
  type_branchings_1 g f a i h b =
    case b of
      [] -> Right a
      c : d -> type_branching_1 g f a i h c >>= \e -> type_branchings_1 g f e i h d
  type_case ::
    (Location_0 -> Location_1) -> Name -> Map' String -> [Pattern_0] -> [Type_1] -> Map' Type_2 -> Err (Map' Type_2)
  type_case j (m @ (Name k l)) a b c d = case b of
    [] -> Right d
    e : f -> case c of
      [] -> Left ("Constructor " ++ l ++ location (j k) ++ " has been given too many fields.")
      g : h -> type_case j m a f h (case e of
        Blank_pattern -> d
        Name_pattern i -> Data.Map.insert i (Basic_type_1 [] Nothing [] (repl a g)) d)
  type_class_0 ::
    (Location_0 -> Location_1) ->
    Map' Kind ->
    Map' Polykind ->
    Class_2 ->
    (
      Map' (Map' Location'),
      Map' (Type_2, Status),
      Map' ([String], Map' [(String, Nat)]),
      Map' (Kind_1, Status)) ->
    Err
      (
        Class_4,
        (
          Map' (Map' Location'),
          Map' (Type_2, Status),
          Map' ([String], Map' [(String, Nat)]),
          Map' (Kind_1, Status)))
  type_class_0 a i j (Class_2 b (c, d) g' e) (m, y, w0, i') =
    let
      x1 = Constraint_1 b c
    in
      (
        type_kind_7 a i Star_kind d >>=
        \h ->
          (
            (\g ->
              (
                Class_4 b (c, h) g' g,
                (
                  Data.Map.insert b Data.Map.empty m,
                  Prelude.foldl (\x -> \(Method_3 t s u) -> ins_new t (Basic_type_1 ((c, h) : s) (Just x1) [x1] u) x) y g,
                  Data.Map.insert b ((\(Method_3 w1 _ _) -> w1) <$> g, Data.Map.empty) w0,
                  ins_new b h i'))) <$>
            type_methods_0 a e (Data.Map.insert c (pkind h) j) i))
  type_class_1 ::
    (Location_0 -> Location_1) ->
    Class_4 ->
    Map' Kind_1 ->
    Map' (Class_3, Status) ->
    Err (Map' (Class_3, Status))
  type_class_1 a (Class_4 b (c, k) g' e) d f =
    let
      l m = Right (ins_new b (Class_3 (c, k) m e) f)
    in
      case g' of
        Just (Name m g) -> und_err g d "class" (a m) (\h -> if h == k then l (Just g) else kind_err (a m))
        Nothing -> l Nothing
  type_class_args ::
    Kind_1 ->
    [Pattern_0] ->
    Err ([(String, Kind_1)], Integer, Type_1, Map' (Kind_1, Nat)) ->
    Kind_1 ->
    Integer ->
    Type_1 ->
    Map' (Kind_1, Nat) ->
    Nat ->
    Err ([(String, Kind_1)], Integer, Type_1, Map' (Kind_1, Nat))
  type_class_args a b e g c x c0 n = case b of
    [] -> if a == g then Right ([], c, x, c0) else e
    h : d -> case a of
      Application_kind_1 (Application_kind_1 (Name_kind_1 "Arrow") l) f ->
        let
          i j k =
            (
              (\(t, u, v, t') -> ((j, l) : t, u, v, t')) <$>
              type_class_args f d e g k (Application_type_1 x (ntype j)) (Data.Map.insert j (l, n) c0) (Nxt n))
        in case h of
          Blank_pattern -> i (show c) (c + 1)
          Name_pattern j -> i j c
      _ -> e
  type_classes ::
    (Location_0 -> Location_1) ->
    Map' Kind ->
    Map' Polykind ->
    [Class_2] ->
    (
      Map' (Class_3, Status),
      Map' (Map' Location'),
      Map' (Type_2, Status),
      Map' ([String], Map' [(String, Nat)]),
      Map' (Kind_1, Status)) ->
    Err
      (
        Map' (Class_3, Status),
        Map' (Map' Location'),
        Map' (Type_2, Status),
        Map' ([String], Map' [(String, Nat)]),
        Map' (Kind_1, Status))
  type_classes a b c d (e, f, g, i, o) =
    (
      type_classes_0 a b c d (f, g, i, o) >>=
      \(r, (j, k, m, p)) -> (\n -> (n, j, k, m, p)) <$> type_classes_1 a r (fst <$> p) e)
  type_classes_0 ::
    (Location_0 -> Location_1) ->
    Map' Kind ->
    Map' Polykind ->
    [Class_2] ->
    (Map' (Map' Location'), Map' (Type_2, Status), Map' ([String], Map' [(String, Nat)]), Map' (Kind_1, Status)) ->
    Err
      (
        [Class_4],
        (Map' (Map' Location'), Map' (Type_2, Status), Map' ([String], Map' [(String, Nat)]), Map' (Kind_1, Status)))
  type_classes_0 a f g b c = case b of
    [] -> Right ([], c)
    d : e -> type_class_0 a f g d c >>= \(h, i) -> first ((:) h) <$> type_classes_0 a f g e i
  type_classes_1 ::
    (Location_0 -> Location_1) ->
    [Class_4] ->
    Map' Kind_1 ->
    Map' (Class_3, Status) ->
    Err (Map' (Class_3, Status))
  type_classes_1 a b h c = case b of
    [] -> Right c
    d : e -> type_class_1 a d h c >>= type_classes_1 a e h
  type_cls_0 ::
    String ->
    [Method_3] ->
    Type_1 ->
    [(Name, Expression_1)] ->
    (Location_0 -> Location_1) ->
    String ->
    Location_0 ->
    Err [(Name, Expression_1, [(String, Kind_1)], Type_1)]
  type_cls_0 a b c d l m n = case d of
    [] -> case b of
      [] -> Right []
      (Method_3 e _ _) : _ -> Left ("Missing definition " ++ e ++ m ++ location' (l n))
    (p' @ (Name h i), j) : k ->
-- todo: distinguish between these two error messages
      let
        o p = Left ("Definition " ++ i ++ location (l h) ++ " is not a component of class " ++ m ++ p)
      in case b of
        [] -> o "."
        (Method_3 e s f) : g ->
          if i == e then (:) (p', j, s, f) <$> type_cls_0 a g c k l m n else o " or the definitions are in a wrong order."
  type_constraint_0 ::
    Map' (Map' (Maybe String, Location_0)) ->
    Constraint_0 ->
    (Map' Class_3, Map' (Kind_1, Nat)) ->
    String ->
    Err (Map' (Map' (Maybe String, Location_0)), Constraint_1, [String], (String, Nat))
  type_constraint_0 k a @ (Constraint_0 (Name _ c) (Name _ e)) (f, g) j =
    case Data.Map.lookup e g of
      Just (i, i') -> (\(l, m, n) -> (l, m, n, (c, i'))) <$> type_constraint_3 k a f j (Just i)
      Nothing ->
        case type_constraint_3 k a f j Nothing of
          Left x -> Left x
          Right _ -> undefined
  type_constraint_1 :: Constraint_1 -> Map' (Map' [[String]]) -> Map' Class_3 -> Map' (Map' [[String]])
  type_constraint_1 (Constraint_1 c e) a b =
    let
      d =
        Data.Map.insert c (Data.Map.insert e [] (case Data.Map.lookup c a of
          Just l -> l
          Nothing -> Data.Map.empty)) a
      Class_3 _ f _ = unsafe_lookup c b
    in
      case f of
        Just g -> type_constraint_1 (Constraint_1 g e) d b
        Nothing -> d
  type_constraint_2 ::
    Map' (Map' (Maybe String, Location_0)) ->
    Constraint_0 ->
    (Map' Class_3, Map' Kind_1) ->
    String ->
    Err (Map' (Map' (Maybe String, Location_0)), Constraint_1, [String])
  type_constraint_2 k a @ (Constraint_0 _ (Name _ e)) (f, g) j = type_constraint_3 k a f j (Data.Map.lookup e g)
  type_constraint_3 ::
    Map' (Map' (Maybe String, Location_0)) ->
    Constraint_0 ->
    Map' Class_3 ->
    String ->
    Maybe Kind_1 ->
    Err (Map' (Map' (Maybe String, Location_0)), Constraint_1, [String])
  type_constraint_3 k (Constraint_0 (Name b c) (Name d e)) f j i' =
    und_err
      c
      f
      "class"
      (Location_1 j b)
      (\(Class_3 (_, h) y h') ->
        case i' of
          Just i ->
            let
              f' t' =
                (
                  chain_constraints y f (t' k) e c b j ((\(Method_3 u _ _) -> u ++ " " ++ e) <$> h') >>=
                  \(t0, t1) -> Right (t0, Constraint_1 c e, t1))
            in
              if i == h then case Data.Map.lookup c k of
                Just l -> case Data.Map.lookup e l of
                  Just (m', m) ->
                    Left
                      (case m' of
                        Just n' ->
                          "Constraint " ++
                          c ++
                          " " ++
                          e ++
                          " in " ++
                          j ++
                          " at " ++
                          location0 b ++
                          " overlaps with constraint " ++
                          n' ++
                          " " ++
                          e ++
                          " at " ++
                          location0 m ++
                          "."
                        Nothing ->
                          "Duplicate constraint " ++
                          c ++
                          " " ++
                          e ++
                          " in " ++
                          j ++
                          " at " ++
                          location0 m ++
                          " and " ++
                          location0 b ++
                          ".")
                  Nothing -> f' (adjust (Data.Map.insert e (Nothing, b)) c)
                Nothing -> f' (Data.Map.insert c (Data.Map.singleton e (Nothing, b)))
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
  type_constraints_0 ::
    Map' (Map' (Maybe String, Location_0)) ->
    [Constraint_0] ->
    (Map' Class_3, Map' (Kind_1, Nat)) ->
    String ->
    Err ([Constraint_1], [String], [(String, Nat)])
  type_constraints_0 g a f h = case a of
    [] -> Right ([], [], [])
    b : c ->
      type_constraint_0 g b f h >>= \(d, e, i, m) -> (\(j, k, n) -> (e : j, i ++ k, m : n)) <$> type_constraints_0 d c f h
  type_constraints_1 :: [Constraint_1] -> Map' (Map' [[String]]) -> Map' Class_3 -> Map' (Map' [[String]])
  type_constraints_1 a e d = case a of
    [] -> e
    b : c -> type_constraints_1 c (type_constraint_1 b e d) d
  type_constraints_2 ::
    Map' (Map' (Maybe String, Location_0)) ->
    [Constraint_0] ->
    (Map' Class_3, Map' Kind_1) ->
    String ->
    Err ([Constraint_1], [String])
  type_constraints_2 g a f h =
    case a of
      [] -> Right ([], [])
      b : c -> type_constraint_2 g b f h >>= \(d, e, i) -> bimap ((:) e) ((++) i) <$> type_constraints_2 d c f h
  type_data_1 ::
    (Location_0 -> Location_1) ->
    (Map' Kind, Map' Prom_alg) ->
    Data_2 ->
    (Map' (Polykind, Status), Constrs, Map' Expression_2, Map' Polykind) ->
    Err ((Map' (Polykind, Status), Constrs, Map' Expression_2, Map' Polykind), Data_3)
  type_data_1 q (o, o') (Data_2 (Name a2 a) b') (i, j, k, x) =
    case b' of
      Branching_data_2 (Name b c) d e f ->
        und_err
          c
          o'
          "promoted algebraic"
          (q b)
          (\(Prom_alg g h) ->
            (
              type_kinds_9 q o g d (\t -> "Kind " ++ c ++ location (q b) ++ " has too " ++ t ++ " arguments.") >>=
              \l ->
                (
                  type_kinds_5 q o e >>=
                  \m ->
                    let
                      p =
                        pkind
                          (Prelude.foldr
                            arrow_kind
                            star_kind
                            (Prelude.foldl Application_kind_1 (Name_kind_1 c) l : (snd <$> m)))
                    in
                      (
                        (\n ->
                          (
                            (
                              ins_new a p i,
                              j,
                              Prelude.foldl
                                (\o1 -> \(Brnch_3 u _ _ s) ->
                                  let
                                    w = fst <$> s
                                  in
                                    Prelude.foldl
                                      (\t -> \v -> Data.Map.insert v (Field_expression_2 v) t)
                                      (Data.Map.insert
                                        u
                                        (Prelude.foldr
                                          (\t -> Function_expression_2 (Name_pattern ('#' : t)))
                                          (Struct_expression_2
                                            (Data.Map.fromList ((\t -> (t, Name_expression_2 ('#' : t))) <$> w)))
                                          w)
                                        o1)
                                      w)
                                k
                                n,
                              Data.Map.insert a p x),
                            Data_3 a (Branching_data_3 c l m n))) <$>
                        type_branchings q a a2 c (Right <$> h) f))))
      Plain_data_2 b c ->
        let
          (l, m) = case c of
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
                        (\f -> Function_expression_2 (Name_pattern ('#' : f)))
                        (Struct_expression_2 (Data.Map.fromList ((\f -> (f, Name_expression_2 ('#' : f))) <$> e')))
                        e')
                      k)
                    e')
        in
          (
            (\p ->
              let
                y = pkind (Prelude.foldr arrow_kind star_kind (snd <$> p))
              in
                ((ins_new a y i, l, m, Data.Map.insert a y x), Data_3 a (Plain_data_3 p c))) <$>
            type_kinds_5 q o b)
  type_data_2 ::
    (Location_0 -> Location_1) -> Data_3 -> Map' Polykind -> Map' Kind -> (Algebraics, Types) -> Err (Algebraics, Types)
  type_data_2 f (Data_3 a b') d y (p, e) =
    case b' of
      Branching_data_3 _ _ g h ->
        let
          i = type_kinds g d
        in
          (,) p <$> type_branchings_1 f (y, i) e a g h
      Plain_data_3 b c ->
        let
          g = type_kinds b d
          x = Prelude.foldl (\n -> Application_type_1 n <$> ntype) (ntype a) (fst <$> b)
          t' = Basic_type_1 b Nothing []
        in case c of
          Algebraic_data_1 h ->
            (
              (\q ->
                (
                  ins_new a (Alg b (Data.Map.fromList ((\(Form_2 r s) -> (r, s)) <$> q)) x) p,
                  Prelude.foldl (flip (\(Form_2 l m) -> ins_new l (t' (Prelude.foldr function_type x m)))) e q)) <$>
              type_forms f h g y)
          Struct_data_1 h ->
            (
              (\i ->
                (
                  p,
                  Prelude.foldl
                    (flip (\(k, l) -> ins_new k (t' (function_type x l))))
                    (ins_new a (t' (Prelude.foldr (function_type <$> snd) x i)) e)
                    i)) <$>
              type_fields f h g y)
  type_datas ::
    (Location_0 -> Location_1) ->
    [Data_2] ->
    (
      Map' (Polykind, Status),
      Algebraics,
      Constrs,
      Types,
      Map' (Kind, Status),
      Map' Expression_2,
      Map' Polykind,
      Map' (Bool, Status),
      Map' (Prom_alg, Status)) ->
    Err
      (
        Map' (Polykind, Status),
        Algebraics,
        Constrs,
        Types,
        Map' (Kind, Status),
        Map' Expression_2,
        Map' Polykind,
        Map' (Bool, Status),
        Map' (Prom_alg, Status))
  type_datas h a (b, i, j, d, o, c, m, x, a0) =
    (
      type_proms_1 h a (o, b, j, c, m) (make_eqs a (first Left <$> x)) >>=
      \((e, p, q, r, s), f, g, k) ->
        (
          type_proms_2 h f s (fst <$> o) (p, i, d, a0) >>=
          \(t, l, n, b0) ->
            (
              type_datas_1 h (fst <$> e, fst <$> b0) g (t, q, r, s) >>=
              \((u, v, w, a'), y) ->
                (
                  (\(b', c') -> (u, b', v, c', e, w, a', first unsafe_left <$> k, b0)) <$>
                  type_datas_2 h y (fst <$> u) (fst <$> e) (l, n)))))
  type_datas_1 ::
    (Location_0 -> Location_1) ->
    (Map' Kind, Map' Prom_alg) ->
    [Data_2] ->
    (Map' (Polykind, Status), Constrs, Map' Expression_2, Map' Polykind) ->
    Err ((Map' (Polykind, Status), Constrs, Map' Expression_2, Map' Polykind), [Data_3])
  type_datas_1 f a b c = case b of
    [] -> Right (c, [])
    d : e -> type_data_1 f a d c >>= \(g, h) -> second ((:) h) <$> type_datas_1 f a e g
  type_datas_2 ::
    (Location_0 -> Location_1) -> [Data_3] -> Map' Polykind -> Map' Kind -> (Algebraics, Types) -> Err (Algebraics, Types)
  type_datas_2 f a b y c = case a of
    [] -> Right c
    d : e -> type_data_2 f d b y c >>= type_datas_2 f e b y
  type_def_1 ::
    String ->
    Map' Kind ->
    Def_3 ->
    Map' Polykind ->
    Map' (Type_2, Status) ->
    Map' Class_3 ->
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
  type_def_1 l x a b c k t t' u3 = case a of
    Basic_def_3 f d e e' g i ->
      (
        type_kinds_1 (Location_1 l) x e b Data.Map.empty >>=
        \(y, j, j') ->
          (
            type_constraints_2 Data.Map.empty e' (k, j') l >>=
              \(o1, o2) ->
              (
                (\h -> (Basic_def_4 f d y o1 h i o2, ins_new d (Basic_type_1 y Nothing o1 h) c, t, t', u3)) <$>
                type_type (Location_1 l) g j x star_kind)))
    Instance_3 d (Name e m) (Name f n) k' o' g ->
      und_err
        m
        k
        "class"
        (Location_1 l e)
        (\(Class_3 (o, p) _ q) -> und_err n b "type" (Location_1 l f) (\(Polykind r s) -> case r of
          [] ->
            (
              type_class_args s k' (kind_err (Location_1 l f)) p 0 (ntype n) Data.Map.empty Zr >>=
              \(q', p', s', t0) ->
                (
                  type_constraints_0 Data.Map.empty o' (k, t0) l >>=
                  \(o1, o2, o3) ->
                    let
                      r' =
                        (
                          (\(x', _) ->
                            (\(Constraint_1 y' _) -> y') <$> (Data.List.filter (\(Constraint_1 _ y') -> y' == x') o1)) <$>
                          q')
                    in
                      type_cls_0 o q s' g (Location_1 l) m d >>= \w -> case Data.Map.lookup n (unsafe_lookup m t) of
                        Just u -> Left (location_err ("instances of " ++ m ++ " " ++ n) u (Location_1 l d))
                        Nothing ->
                          Right
                            (
                              Instance_4 o n q' p' w s' o1 o2,
                              c,
                              adjust (Data.Map.insert n (Library (Location_1 l d))) m t,
                              (case Data.Map.lookup m t' of
                                Just _ -> adjust (ins_new n r') m
                                Nothing -> Data.Map.insert m (Data.Map.singleton n (r', New))) t',
                              adjust (second (Data.Map.insert n o3)) m u3)))
          _ -> Left ("Kind-polymorphic type " ++ n ++ " an instance of a class" ++ location' (Location_1 l f))))
  type_def_2 ::
    (Location_0 -> Location_1) ->
    Def_4 ->
    (Map' Alg, Map' String, Map' Type_2) ->
    Map' Expression_2 ->
    Map' (Map' [[String]]) ->
    Map' Polykind ->
    Map' ([String], Map' [(String, Nat)]) ->
    Map' Class_3 ->
    Err (Map' Expression_2)
  type_def_2 j a (d, l, k) c m n t' u0 = case a of
    Basic_def_4 r e b x h i y' ->
      (
        (\t -> Data.Map.insert e (Prelude.foldr (\x' -> Function_expression_2 (Name_pattern x')) t y') c) <$>
        type_expr
          ("definition " ++ e ++ location' (j r))
          h
          j
          (Left <$> Prelude.foldl (\y -> \(z, w) -> Data.Map.insert z (pkind w) y) n b, d, l, k)
          i
          (type_constraints_1 x m u0)
          0
          t'
          u0)
    Instance_4 w e e0 e1 f f' g' c2 ->
      type_exprs
        (\(Name x g) -> "definition " ++ g ++ " " ++ e ++ location' (j x))
        j
        (Left <$> Prelude.foldl (\x -> \(y, g) -> Data.Map.insert y (pkind g) x) n e0, d, l, k)
        (type_constraints_1 g' m u0)
        f
        c
        e
        (sysrep' w f')
        e1
        (\g -> Prelude.foldr (\b -> Function_expression_2 (Name_pattern b)) g c2)
        t'
        u0
  type_defs ::
    String ->
    Map' Kind ->
    [Def_3] ->
    (Map' Polykind, Map' Alg, Map' String) ->
    (Map' Expression_2, Types) ->
    Map' Class_3 ->
    Map' (Map' Location') ->
    Map' (Map' ([[String]], Status)) ->
    Map' ([String], Map' [(String, Nat)]) ->
    Err
      (
        Map' Expression_2,
        Types,
        Map' (Map' Location'),
        Map' (Map' ([[String]], Status)),
        Map' ([String], Map' [(String, Nat)]))
  type_defs h x a (b, i, j) (c, d) y z t u' =
    (
      type_defs_1 h x a b d y z t u' >>=
      \(g, e, k, u, f') ->
        (\f -> (f, e, k, u, f')) <$> type_defs_2 (Location_1 h) g (i, j, fst <$> e) c ((<$>) fst <$> u) b f' y)
  type_defs_1 ::
    String ->
    Map' Kind ->
    [Def_3] ->
    Map' Polykind ->
    Types ->
    Map' Class_3 ->
    Map' (Map' Location') ->
    Map' (Map' ([[String]], Status)) ->
    Map' ([String], Map' [(String, Nat)]) ->
    Err ([Def_4], Types, Map' (Map' Location'), Map' (Map' ([[String]], Status)), Map' ([String], Map' [(String, Nat)]))
  type_defs_1 h x a b c y z u v = case a of
    [] -> Right ([], c, z, u, v)
    d : e ->
      (
        type_def_1 h x d b c y z u v >>=
        \(f, g, t, u', v') -> (\(k, l, m, t', k') -> (f : k, l, m, t', k')) <$> type_defs_1 h x e b g y t u' v')
  type_defs_2 ::
    (Location_0 -> Location_1) ->
    [Def_4] ->
    (Map' Alg, Map' String, Map' Type_2) ->
    Map' Expression_2 ->
    Map' (Map' [[String]]) ->
    Map' Polykind ->
    Map' ([String], Map' [(String, Nat)]) ->
    Map' Class_3 ->
    Err (Map' Expression_2)
  type_defs_2 f a b c g i j u = case a of
    [] -> Right c
    d : e -> type_def_2 f d b c g i j u >>= \h -> type_defs_2 f e b h g i j u
  type_expr ::
    String ->
    Type_1 ->
    (Location_0 -> Location_1) ->
    (Map' (Either Polykind Kind_1), Map' Alg, Map' String, Map' Type_2) ->
    Expression_1 ->
    Map' (Map' [[String]]) ->
    Integer ->
    Map' ([String], Map' [(String, Nat)]) ->
    Map' Class_3 ->
    Err Expression_2
  type_expr k h a (b, c, d, e) f m w w' w0 =
    let
      n = " in " ++ k
    in (
      type_expression c d a w 0 b [] e f h [] >>=
      \(g, i, j, _, _, x) ->
        (
          solvesys ("Type error" ++ n) i j (x, g) >>=
          \(y, p) -> addargs w0 w' p <$ slv m y (\t -> "Failure to resolve constraints for class " ++ t ++ n)))
  type_expr' ::
    (Location_0 -> Location_1) ->
    (Map' Polykind, Map' Alg, Map' String, Map' Type_2) ->
    Expression_1 ->
    Map' (Map' [[String]]) ->
    Map' ([String], Map' [(String, Nat)]) ->
    Map' Class_3 ->
    Err Expression_2
  type_expr' a (b, c, d, e) f g =
    type_expr "input." (ntype "!") a (Data.Map.insert "!" (Right star_kind) (Left <$> b), c, d, e) f g 0
  type_expression ::
    Map' Alg ->
    Map' String ->
    (Location_0 -> Location_1) ->
    Integer ->
    Integer ->
    Map' (Either Polykind Kind_1) ->
    [(Type_1, Type_1)] ->
    Map' Type_2 ->
    Expression_1 ->
    Type_1 ->
    [(String, Type_1)] ->
    Err (Typedexpr, Map' (Either Polykind Kind_1), [(Type_1, Type_1)], Integer, Integer, [(String, Type_1)])
  type_expression v w r o s f h d (Expression_1 a b) e c' =
    let
      x' = location' (r a)
    in case b of
      Application_expression_1 c g ->
        (
          type_expression
            v
            w
            r
            (o + 1)
            s
            (Data.Map.insert (show o) (Right star_kind) f)
            h
            d
            c
            (function_type (ntype (show o)) e)
            c' >>=
          \(i, j, k, p, t, d') ->
            (
              (\(l, m, n, q, u, e') -> (Application_texpr i l, m, n, q, u, e')) <$>
              type_expression v w r p t j k d g (ntype (show o)) d'))
      Char_expression_1 c -> Right (Char_texpr c, f, (e, char_type) : h, o, s, c')
      Function_expression_1 c g ->
        (
          (\(a', b', c3, d', e', f') -> (Function_texpr c a', b', c3, d', e', f')) <$>
          type_expression
            v
            w
            r
            (o + 2)
            s
            (Data.Map.insert (show (o + 1)) (Right star_kind) (Data.Map.insert (show o) (Right star_kind) f))
            ((e, function_type (ntype (show o)) (ntype (show (o + 1)))) : h)
            (case c of
              Blank_pattern -> d
              Name_pattern i -> Data.Map.insert i (Basic_type_1 [] Nothing [] (ntype (show o))) d)
            g
            (ntype (show (o + 1)))
            c')
      Int_expression_1 c -> Right (Int_texpr c, f, (e, int_type) : h, o, s, c')
      Match_expression_1 c g -> case g of
        Matches_Algebraic_1 i j -> case i of
          [] -> undefined
          Match_Algebraic_1 (Name l2 l) _ _ : _ -> case Data.Map.lookup l w of
            Just m ->
              let
                Alg n p q = unsafe_lookup m v
                (t, u) = typevars (\v' -> v' ++ " " ++ show s) n (Data.Map.empty, f)
                l0 = repl t q
              in (
                type_expression v w r o (s + 1) u h d c l0 c' >>=
                \(x, y, a0, b0, c0, a2) ->
                  type_matches_algebraic v w r b0 c0 y a0 d Data.Map.empty i e (Right <$> p) (l ++ location (r l2)) t a2 >>=
                    \(d0, e0, f0, g0, h0, i0, a3) ->
                      let
                        k0 k1 = Match_texpr x (Tmatch_algebraic d0 k1)
                      in if all isLeft (Data.Map.elems i0) then case j of
                        Just (l3, _) -> Left ("Unnecessary default case" ++ location' (r l3))
                        Nothing -> Right (k0 Nothing, e0, f0, g0, h0, a3) else case j of
                          Just (_, j0) ->
                            (
                              (\(a', b', c2, d', e', a4) -> (k0 (Just a'), b', c2, d', e', a4)) <$>
                              type_expression v w r g0 h0 e0 f0 d j0 e a3)
                          Nothing -> Left ("Incomplete match" ++ x'))
            Nothing -> Left ("Undefined algebraic constructor " ++ l ++ location' (r l2))
        Matches_char_1 i j ->
          (
            type_expression v w r o s f h d c char_type c' >>=
            \(k, l, m, n, p, d') ->
              (
                type_matches_char v w r n p l m d Data.Map.empty i e d' >>=
                \(q, t, u, x, y, e') ->
                  (
                    (\(a0, b0, c0, d0, e0, a2) -> (Match_texpr k (Tmatch_char q a0), b0, c0, d0, e0, a2)) <$>
                    type_expression v w r x y t u d j e e')))
        Matches_Int_1 i j ->
          (
            type_expression v w r o s f h d c int_type c' >>=
            \(k, l, m, n, p, d') ->
              (
                type_matches_int v w r n p l m d Data.Map.empty i e d' >>=
                \(q, t, u, x, y, e') ->
                  (
                    (\(a0, b0, c0, d0, e0, a2) -> (Match_texpr k (Tmatch_int q a0), b0, c0, d0, e0, a2)) <$>
                    type_expression v w r x y t u d j e e')))
{-
INEFFICIENCY.
ONE COULD CONSTRUCT AN IDENTITY MAP AND PUT IT INTO BASIC_TYPE AND THEN MAP (++ SUFFIX) OVER IT
OR SUFFIX COULD BE GIVEN AS ARGUMENT TO REPL AND ADDED INSIDE REPL
-}
      Name_expression_1 c ->
        und_err
          c
          d
          "variable"
          (r a)
          (\(Basic_type_1 i x0 a' j) ->
            Right
              (case i of
                [] -> (Name_texpr_1 c [], f, (e, j) : h, o, s, c')
                _ ->
                  let
                    (n, p) = type_kinds'' i s f
                  in
                    (
                      case x0 of
                        Just (Constraint_1 y0 y1) -> Name_texpr_0 c y0 (Name_type_1 (y1 ++ " " ++ show s) [])
                        Nothing ->
                          Name_texpr_1 c ((\(Constraint_1 a0 b0) -> (a0, Name_type_1 (b0 ++ " " ++ show s) [])) <$> a'),
                      n,
                      (e, repl p j) : h,
                      o,
                      s + 1,
                      ((\(Constraint_1 y t) -> (y, Name_type_1 (t ++ " " ++ show s) [])) <$> a') ++ c')))
  type_exprs ::
    (Name -> String) ->
    (Location_0 -> Location_1) ->
    (Map' (Either Polykind Kind_1), Map' Alg, Map' String, Map' Type_2) ->
    Map' (Map' [[String]]) ->
    [(Name, Expression_1, [(String, Kind_1)], Type_1)] ->
    (Map' Expression_2) ->
    String ->
    (Type_1 -> Type_1) ->
    Integer ->
    (Expression_2 -> Expression_2) ->
    Map' ([String], Map' [(String, Nat)]) ->
    Map' Class_3 ->
    Err (Map' Expression_2)
  type_exprs a b c @ (c0, c1, c2, c3) d h i t z w f' t' t0 = case h of
    [] -> Right i
    (j @ (Name _ y), k, s, l) : m ->
      (
        type_expr
          (a j)
          (z l)
          b
          (Prelude.foldl (\k' -> \(l', u) -> Data.Map.insert l' (Left (pkind u)) k') c0 s, c1, c2, c3)
          k
          d
          w
          t'
          t0 >>=
        \g -> type_exprs a b c d m (Data.Map.insert (y ++ " " ++ t) (f' g) i) t z w f' t' t0)
  type_field :: (Location_0 -> Location_1) -> (String, Type_0) -> Map' Polykind -> Map' Kind -> Err (String, Type_1)
  type_field d (a, b) c e  = (,) a <$> type_type d b c e star_kind
  type_fields :: (Location_0 -> Location_1) -> [(String, Type_0)] -> Map' Polykind -> Map' Kind -> Err [(String, Type_1)]
  type_fields f a b g = case a of
    [] -> Right []
    c : d -> type_field f c b g >>= \e -> (:) e <$> type_fields f d b g
  type_form :: (Location_0 -> Location_1) -> Form_1 -> Map' Polykind -> Map' Kind -> Err Form_2
  type_form d (Form_1 a b) c e = Form_2 a <$> type_types d b c e
  type_forms :: (Location_0 -> Location_1) -> [Form_1] -> Map' Polykind -> Map' Kind -> Err [Form_2]
  type_forms f a b g = case a of
    [] -> Right []
    c : d -> type_form f c b g >>= \e -> (:) e <$> type_forms f d b g
  type_kind :: (String, Kind_1) -> Map' Polykind -> Map' Polykind
  type_kind (a, b) = Data.Map.insert a (pkind b)
  type_kind_4 ::
    (Location_0 -> Location_1) ->
    Map' Kind ->
    (String, Kind_0) ->
    (Map' Kind_1, Map' Kind_1) ->
    Err (Map' Kind_1, Map' Kind_1)
  type_kind_4 d e (g, a) b = (\h ->
    let
      f = Data.Map.insert g h
    in bimap f f b) <$> type_kind_7 d e Star_kind a
  type_kind_6 :: (Location_0 -> Location_1) -> Map' Kind -> Kind_0 -> Err (Kind_1, Kind)
  type_kind_6 a b (Kind_0 c d) = case d of
    Application_kind_0 e f -> type_kind_6 a b e >>= \(g, h) -> case h of
      Arrow_kind i j -> (\k -> (Application_kind_1 g k, j)) <$> type_kind_7 a b i f
      Star_kind -> kind_err (a c)
    Name_kind_0 e -> und_err e b "kind" (a c) (\f -> Right (Name_kind_1 e, f))
  type_kind_7 :: (Location_0 -> Location_1) -> Map' Kind -> Kind -> Kind_0 -> Err Kind_1
  type_kind_7 a b c (Kind_0 d e) = case e of
    Application_kind_0 f g -> type_kind_6 a b f >>= \(h, i) -> case i of
      Arrow_kind j k -> if k == c then Application_kind_1 h <$> type_kind_7 a b j g else kind_err (a d)
      Star_kind -> kind_err (a d)
    Name_kind_0 f -> und_err f b "kind" (a d) (\g -> if g == c then Right (Name_kind_1 f) else kind_err (a d))
  type_kinds :: [(String, Kind_1)] -> Map' Polykind -> Map' Polykind
  type_kinds a b = case a of
    [] -> b
    c : d -> type_kinds d (type_kind c b)
  type_kinds'' ::
    [(String, Kind_1)] -> Integer -> Map' (Either Polykind Kind_1) -> (Map' (Either Polykind Kind_1), Map' String)
  type_kinds'' a b c = type_kinds_3 a (" " ++ show b) c Data.Map.empty
  type_kinds_0 ::
    (Location_0 -> Location_1) -> Map' Kind -> [(String, Kind_0)] -> Map' Polykind -> Err ([(String, Kind_1)], Map' Polykind)
  type_kinds_0 a b c d = case c of
    [] -> Right ([], d)
    (e, f) : g ->
      type_kind_7 a b Star_kind f >>= \h -> first ((:) (e, h)) <$> type_kinds_0 a b g (Data.Map.insert e (pkind h) d)
  type_kinds_1 ::
    (Location_0 -> Location_1) ->
    Map' Kind ->
    [(String, Kind_0)] ->
    Map' Polykind ->
    Map' Kind_1 ->
    Err ([(String, Kind_1)], Map' Polykind, Map' Kind_1)
  type_kinds_1 a b c d i =
    case c of
      [] -> Right ([], d, i)
      (e, f) : g ->
        (
          type_kind_7 a b Star_kind f >>=
          \h ->
            (
              (\(j, k, l) -> ((e, h) : j, k, l)) <$>
              type_kinds_1 a b g (Data.Map.insert e (pkind h) d) (Data.Map.insert e h i)))
  type_kinds_3 ::
    [(String, Kind_1)] ->
    String ->
    Map' (Either Polykind Kind_1) ->
    Map' String ->
    (Map' (Either Polykind Kind_1), Map' String)
  type_kinds_3 a b f c = case a of
    [] -> (f, c)
    (d, e) : g ->
      let
        h = d ++ b
      in
        type_kinds_3 g b (Data.Map.insert h (Right e) f) (Data.Map.insert d h c)
  type_kinds_4 ::
    (Location_0 -> Location_1) ->
    Map' Kind ->
    [(String, Kind_0)] ->
    (Map' Kind_1, Map' Kind_1) ->
    Err (Map' Kind_1, Map' Kind_1)
  type_kinds_4 e f a b = case a of
    [] -> Right b
    c : d -> type_kind_4 e f c b >>= type_kinds_4 e f d
  type_kinds_5 :: (Location_0 -> Location_1) -> Map' Kind -> [(String, Kind_0)] -> Err [(String, Kind_1)]
  type_kinds_5 f a b = case b of
    [] -> Right []
    (g, c) : d -> type_kind_7 f a Star_kind c >>= \e -> (:) (g, e) <$> type_kinds_5 f a d
  type_kinds_9 :: (Location_0 -> Location_1) -> Map' Kind -> [String] -> [Kind_0] -> (String -> String) -> Err [Kind_1]
  type_kinds_9 a b c d i =
    case c of
      [] ->
        case d of
          [] -> Right []
          _ : _ -> Left (i "many")
      _ : e ->
        case d of
          [] -> Left (i "few")
          f : g -> type_kind_7 a b Star_kind f >>= \h -> (:) h <$> type_kinds_9 a b e g i
  type_match_algebraic ::
    Map' Alg ->
    Map' String ->
    (Location_0 -> Location_1) ->
    Integer ->
    Integer ->
    Map' (Either Polykind Kind_1) ->
    [(Type_1, Type_1)] ->
    Map' Type_2 ->
    Map' Tmatch' ->
    Match_Algebraic_1 ->
    Type_1 ->
    Map' (Either Location_0 [Type_1]) ->
    String ->
    Map' String ->
    [(String, Type_1)] ->
    Err
      (
        Map' Tmatch',
        Map' (Either Polykind Kind_1),
        [(Type_1, Type_1)],
        Integer,
        Integer,
        Map' (Either Location_0 [Type_1]),
        [(String, Type_1)])
  type_match_algebraic a b c d e f g h i (Match_Algebraic_1 (Name j k) l m) n o q r a' = case Data.Map.lookup k o of
    Just p' -> case p' of
      Left e' -> Left ("Conflicting cases for " ++ k ++ location (c e') ++ " and" ++ location' (c j))
      Right p ->
        (
          type_case c (Name j k) r l p h >>=
          \s ->
            (
              (\(t, u, v, w, x, b') -> (Data.Map.insert k (Tmatch' l t) i, u, v, w, x, Data.Map.insert k (Left j) o, b')) <$>
              type_expression a b c d e f g s m n a'))
    Nothing -> Left ((case Data.Map.lookup k b of
      Just _ -> "Incompatible constructors " ++ q ++ " and "
      Nothing -> "Undefined algebraic constructor ") ++ k ++ location' (c j))
  type_match_char ::
    Map' Alg ->
    Map' String ->
    (Location_0 -> Location_1) ->
    Integer ->
    Integer ->
    Map' (Either Polykind Kind_1) ->
    [(Type_1, Type_1)] ->
    Map' Type_2 ->
    Map Char Typedexpr ->
    Match_char_1 ->
    Type_1 ->
    [(String, Type_1)] ->
    Err (Map Char Typedexpr, Map' (Either Polykind Kind_1), [(Type_1, Type_1)], Integer, Integer, [(String, Type_1)])
  type_match_char a b c d e f g h i (Match_char_1 j k) l a' =
-- TODO: PUT A GOOD ERROR MESSAGE HERE. LOCATIONS AND STUFF.
    (
      type_expression a b c d e f g h k l a' >>=
      \(m, n, o, p, q, b') ->
        bimap (\_ -> location_err' ("cases for " ++ show j) undefined undefined) (\r -> (r, n, o, p, q, b')) (add i j m))
  type_match_int ::
    Map' Alg ->
    Map' String ->
    (Location_0 -> Location_1) ->
    Integer ->
    Integer ->
    Map' (Either Polykind Kind_1) ->
    [(Type_1, Type_1)] ->
    Map' Type_2 ->
    Map Integer Typedexpr ->
    Match_Int_1 ->
    Type_1 ->
    [(String, Type_1)] ->
    Err (Map Integer Typedexpr, Map' (Either Polykind Kind_1), [(Type_1, Type_1)], Integer, Integer, [(String, Type_1)])
  type_match_int a b c d e f g h i (Match_Int_1 j k) l a' =
-- TODO: PUT A GOOD ERROR MESSAGE HERE. LOCATIONS AND STUFF.
    (
      type_expression a b c d e f g h k l a' >>=
      \(m, n, o, p, q, b') ->
        bimap (\_ -> location_err' ("cases for " ++ show j) undefined undefined) (\r -> (r, n, o, p, q, b')) (add i j m))
  type_matches_algebraic ::
    Map' Alg ->
    Map' String ->
    (Location_0 -> Location_1) ->
    Integer ->
    Integer ->
    Map' (Either Polykind Kind_1) ->
    [(Type_1, Type_1)] ->
    Map' Type_2 ->
    Map' Tmatch' ->
    [Match_Algebraic_1] ->
    Type_1 ->
    Map' (Either Location_0 [Type_1]) ->
    String ->
    Map' String ->
    [(String, Type_1)] ->
    Err
      (
        Map' Tmatch',
        Map' (Either Polykind Kind_1),
        [(Type_1, Type_1)],
        Integer,
        Integer,
        Map' (Either Location_0 [Type_1]),
        [(String, Type_1)])
  type_matches_algebraic a b c d e f g h i j k s u v a' = case j of
    [] -> Right (i, f, g, d, e, s, a')
    l : m ->
      (
        type_match_algebraic a b c d e f g h i l k s u v a' >>=
        \(n, o, p, q, r, t, b') -> type_matches_algebraic a b c q r o p h n m k t u v b')
  type_matches_char ::
    Map' Alg ->
    Map' String ->
    (Location_0 -> Location_1) ->
    Integer ->
    Integer ->
    Map' (Either Polykind Kind_1) ->
    [(Type_1, Type_1)] ->
    Map' Type_2 ->
    Map Char Typedexpr ->
    [Match_char_1] ->
    Type_1 ->
    [(String, Type_1)] ->
    Err (Map Char Typedexpr, Map' (Either Polykind Kind_1), [(Type_1, Type_1)], Integer, Integer, [(String, Type_1)])
  type_matches_char a b c d e f g h i j k a' = case j of
    [] -> Right (i, f, g, d, e, a')
    l : m -> type_match_char a b c d e f g h i l k a' >>= \(n, o, p, q, r, b') -> type_matches_char a b c q r o p h n m k b'
  type_matches_int ::
    Map' Alg ->
    Map' String ->
    (Location_0 -> Location_1) ->
    Integer ->
    Integer ->
    Map' (Either Polykind Kind_1) ->
    [(Type_1, Type_1)] ->
    Map' Type_2 ->
    Map Integer Typedexpr ->
    [Match_Int_1] ->
    Type_1 ->
    [(String, Type_1)] ->
    Err (Map Integer Typedexpr, Map' (Either Polykind Kind_1), [(Type_1, Type_1)], Integer, Integer, [(String, Type_1)])
  type_matches_int a b c d e f g h i j k a' = case j of
    [] -> Right (i, f, g, d, e, a')
    l : m -> type_match_int a b c d e f g h i l k a' >>= \(n, o, p, q, r, b') -> type_matches_int a b c q r o p h n m k b'
  type_method :: (Location_0 -> Location_1) -> Method_2 -> Map' Polykind -> Map' Kind -> Err Method_3
  type_method a (Method_2 b c d) e f = type_kinds_0 a f c e >>= \(g, h) -> Method_3 b g <$> type_type a d h f star_kind
  type_methods_0 :: (Location_0 -> Location_1) -> [Method_2] -> Map' Polykind -> Map' Kind -> Err [Method_3]
  type_methods_0 a b c d = case b of
    [] -> Right []
    e : g -> type_method a e c d >>= \h -> (:) h <$> type_methods_0 a g c d
  type_prom_1 ::
    (Location_0 -> Location_1) ->
    Data_2 ->
    (Map' (Kind, Status), Map' (Polykind, Status), Constrs, Map' Expression_2, Map' Polykind) ->
    Map' (Either Bool (Map' Location_0), Status) ->
    Err
      (
        Maybe ((Map' (Kind, Status), Map' (Polykind, Status), Constrs, Map' Expression_2, Map' Polykind), Plain_dat),
        Map' (Either Bool (Map' Location_0), Status))
  type_prom_1 q (Data_2 (Name _ a) b') (o, i, j, k, x) j' =
    case b' of
      Branching_data_2 _ _ _ _ -> Right (Nothing, j')
      Plain_data_2 b c ->
        first (\k' -> if k' then
          let
            (l, m) = case c of
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
                          (\f -> Function_expression_2 (Name_pattern ('#' : f)))
                          (Struct_expression_2 (Data.Map.fromList ((\f -> (f, Name_expression_2 ('#' : f))) <$> e')))
                          e')
                        k)
                      e')
          in
            let
              y = pkind (Prelude.foldr arrow_kind star_kind (return star_kind <$> b))
            in
              Just
                (
                  (
                    ins_new ('!' : a) (Prelude.foldr Arrow_kind Star_kind (return Star_kind <$> b)) o,
                    ins_new a y i,
                    l,
                    m,
                    Data.Map.insert a y x),
                  Plain_dat a (fst <$> b) c) else Nothing) <$> solve_eq q a j'
  type_prom_2 ::
    (Location_0 -> Location_1) ->
    Plain_dat ->
    Map' Polykind ->
    Map' Kind ->
    (Map' (Polykind, Status), Algebraics, Types, Map' (Prom_alg, Status)) ->
    Err (Map' (Polykind, Status), Algebraics, Types, Map' (Prom_alg, Status))
  type_prom_2 f (Plain_dat a b c) d y' (d', p, e, a0) =
    let
      g = Prelude.foldl (\n -> \y -> Data.Map.insert y (pkind star_kind) n) d b
      x = Prelude.foldl (\n -> \y -> Application_type_1 n (ntype y)) (ntype a) b
      g0 = (\t -> (t, star_kind)) <$> b
      g1 = prom_type (Data.Set.fromList b)
      promhelp p' q' =
        ins_new
          ('!' : p')
          (Polykind
            b
            (Prelude.foldr
              (\x' -> arrow_kind (g1 x'))
              (Prelude.foldl (\t' -> \u' -> Application_kind_1 t' (Name_kind_1 u')) star_kind b)
              q'))
      b' = Basic_type_1 g0 Nothing []
    in case c of
      Algebraic_data_1 h ->
        (
          (\q ->
            (
              Prelude.foldl (\t -> \(Form_2 u v) -> promhelp u v t) d' q,
              ins_new a (Alg g0 (Data.Map.fromList ((\(Form_2 r s) -> (r, s)) <$> q)) x) p,
              Prelude.foldl (flip (\(Form_2 l m) -> ins_new l (b' (Prelude.foldr function_type x m)))) e q,
              ins_new ('!' : a) (Prom_alg b (Data.Map.fromList ((\(Form_2 r s) -> ('!' : r, g1 <$> s)) <$> q))) a0)) <$>
          type_forms f h g y')
      Struct_data_1 h ->
        (
          (\i ->
            (
              promhelp a (snd <$> i) d',
              p,
              Prelude.foldl
                (flip (\(k, l) -> ins_new k (b' (function_type x l))))
                (ins_new a (b' (Prelude.foldr (function_type <$> snd) x i)) e)
                i,
              a0)) <$>
          type_fields f h g y')
  type_proms_1 ::
    (Location_0 -> Location_1) ->
    [Data_2] ->
    (Map' (Kind, Status), Map' (Polykind, Status), Constrs, Map' Expression_2, Map' Polykind) ->
    Map' (Either Bool (Map' Location_0), Status) ->
    Err
      (
        (Map' (Kind, Status), Map' (Polykind, Status), Constrs, Map' Expression_2, Map' Polykind),
        [Plain_dat],
        [Data_2],
        Map' (Either Bool (Map' Location_0), Status))
  type_proms_1 a b c f = case b of
    [] -> Right (c, [], [], f)
    d : e -> type_prom_1 a d c f >>= \(h, g) ->
      let
        o p q = p <$> type_proms_1 a e q g
      in case h of
        Just (i, j) -> o (\(k, l, m, n) -> (k, j : l, m, n)) i
        Nothing -> o (\(k, l, m, n) -> (k, l, d : m, n)) c
  type_proms_2 ::
    (Location_0 -> Location_1) ->
    [Plain_dat] ->
    Map' Polykind ->
    Map' Kind ->
    (Map' (Polykind, Status), Algebraics, Types, Map' (Prom_alg, Status)) ->
    Err (Map' (Polykind, Status), Algebraics, Types, Map' (Prom_alg, Status))
  type_proms_2 a b c y d = case b of
    [] -> Right d
    e : f -> type_prom_2 a e c y d >>= type_proms_2 a f c y
  type_type :: (Location_0 -> Location_1) -> Type_0 -> Map' Polykind -> Map' Kind -> Kind_1 -> Err Type_1
  type_type l (Type_0 a c) d y e =
    let
      x = kind_err (l a)
    in case c of
      Application_type_0 f g -> type_type' l f d y >>= \(h, i) -> case i of
        Application_kind_1 (Application_kind_1 (Name_kind_1 "Arrow") j) k ->
          if k == e then Application_type_1 h <$> type_type l g d y j else x
        _ -> x
      Char_type_0 b -> if e == char_kind then Right (Char_type_1 b) else x
      Int_type_0 b -> if e == int_kind then Right (Int_type_1 b) else x
      Name_type_0 f b ->
        und_err
          f
          d
          "type"
          (l a)
          (\(Polykind h g) ->
            ziphelp l y f a Data.Map.empty h b >>= \(z, w) -> if repkinds w g == e then Right (Name_type_1 f z) else x)
  type_type' :: (Location_0 -> Location_1) -> Type_0 -> Map' Polykind -> Map' Kind -> Err (Type_1, Kind_1)
  type_type' l (Type_0 a c) d y = case c of
    Application_type_0 e f -> type_type' l e d y >>= \(g, h) -> case h of
      Application_kind_1 (Application_kind_1 (Name_kind_1 "Arrow") i) j ->
        (\k -> (Application_type_1 g k, j)) <$> type_type l f d y i
      _ -> kind_err (l a)
    Char_type_0 e -> Right (Char_type_1 e, char_kind)
    Int_type_0 e -> Right (Int_type_1 e, int_kind)
    Name_type_0 e g ->
      und_err
        e
        d
        "type"
        (l a)
        (\(Polykind h f) -> bimap (Name_type_1 e) (\w -> repkinds w f) <$> ziphelp l y e a Data.Map.empty h g)
  type_types :: (Location_0 -> Location_1) -> [Type_0] -> Map' Polykind -> Map' Kind -> Err [Type_1]
  type_types f a b g = case a of
    [] -> Right []
    c : d -> type_type f c b g star_kind >>= \e -> (:) e <$> type_types f d b g
  type_types' :: (Location_0 -> Location_1) -> (Map' Kind, Map' Polykind) -> [(String, Type_0)] -> Err [(String, Type_1)]
  type_types' a (b, c) d =
    case d of
      [] -> Right []
      (e, f) : g -> type_type a f c b star_kind >>= \h -> (:) (e, h) <$> type_types' a (b, c) g
  typestring :: Type_1 -> [Type_1] -> (String, [Type_1])
  typestring a d = case a of
    Application_type_1 b c -> typestring b (c : d)
    Name_type_1 b _ -> (b, d)
    _ -> undefined
  typevar ::
    (String -> String) ->
    (String, Kind_1) ->
    (Map' String, Map' (Either Polykind Kind_1)) ->
    (Map' String, Map' (Either Polykind Kind_1))
  typevar e (a, b) =
    let
      d = e a
    in
      bimap (Data.Map.insert a d) (Data.Map.insert d (Right b))
  typevars ::
    (String -> String) ->
    [(String, Kind_1)] ->
    (Map' String, Map' (Either Polykind Kind_1)) ->
    (Map' String, Map' (Either Polykind Kind_1))
  typevars e a b = case a of
    [] -> b
    c : d -> typevars e d (typevar e c b)
  typing ::
    String ->
    Tree_5 ->
    (File, Map' Expression_2, Map' Polykind, Map' (Map' Location'), Map' ([String], Map' [(String, Nat)])) ->
    Err (File, Map' Expression_2, Map' Polykind, Map' (Map' Location'), Map' ([String], Map' [(String, Nat)]))
  typing k (Tree_5 a a' c) (File d t u v w w0 b' x t2 u0, l, m, m', n4) =
    (
      type_datas (Location_1 k) a (old d, old t, old u, old v, old w, l, m, old w0, old u0) >>=
      \(e, b, h, g, o, f, n, w1, u1) ->
        (
          type_classes (Location_1 k) (fst <$> o) (fst <$> e) a' (old b', m', g, n4, old t2) >>=
          \(c', m2, g0, x1, x2) ->
            (
              (\(i, j, n', y, y2) ->
                (
                  File
                    (rem_old e)
                    (rem_old b)
                    (rem_old h)
                    (rem_old j)
                    (rem_old o)
                    (rem_old w1)
                    (rem_old c')
                    (rem_old' y)
                    (rem_old x2)
                    (rem_old u1),
                  i,
                  n,
                  n',
                  y2)) <$>
              type_defs k (fst <$> o) c (fst <$> e, fst <$> b, fst <$> h) (f, g0) (fst <$> c') m2 (old' x) x1)))
  und_err :: String -> Map' t -> String -> Location_1 -> (t -> Err u) -> Err u
  und_err a b c d f = case Data.Map.lookup a b of
    Just e -> f e
    Nothing -> Left ("Undefined " ++ c ++ " " ++ a ++ location' d)
  unsafe_left :: Either t u -> t
  unsafe_left a = case a of
    Left b -> b
    Right _ -> undefined
  unsafe_lookup :: Ord t => t -> Map t u -> u
  unsafe_lookup a b = case Data.Map.lookup a b of
    Just c -> c
    Nothing -> undefined
  ziphelp ::
    (Location_0 -> Location_1) ->
    Map' Kind ->
    String ->
    Location_0 ->
    Map' Kind_1 ->
    [String] ->
    [Kind_0] ->
    Err ([Kind_1], Map' Kind_1)
  ziphelp l k f a d b c =
    let
      e = Left ("Wrong number of kind arguments for " ++ f ++ location' (l a))
    in case b of
      [] -> case c of
        [] -> Right ([], d)
        _ -> e
      g : h -> case c of
        [] -> e
        i : j -> type_kind_7 l k Star_kind i >>= \m -> first ((:) m) <$> ziphelp l k f a (Data.Map.insert g m d) h j
-----------------------------------------------------------------------------------------------------------------------------