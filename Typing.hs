{-
let expression (sequential? or more powerful, like in Haskell?)
protection against duplicate file loading - what happens now? if crashes - fix, give a nice error/warning. if nothing - warn?
implement all necessary operations for ints and bools
tests
type synonyms?
operators
type operators (minimally for functions, either, pair).
abstract methods
if-elif-else?
eta reduction warnings
unused type variable warnings
unused local variable warnings
add something for easily changing fields of structs?
internal: do something with old/new status tags. check where exactly they're necessary. get rid of them where they're useless
internal: with new matching error system, do we need to keep locations for each match? if not, modify parser/namer to remove
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
midi and music library
boolean function library
parsing library
implement map and set (AVL trees?)
make match work with finite and char
different ways of folding lists, vectors, sets, maps etc
real numbers (float, fix, fraction), basic arithmetic + real functions (sine, exp etc). Trig also for complex numbers?
gather naming and type errors and give a list instead of returning only the first one?
enrich kind system via promotion
make match work with chars
make promotion for built-in ADT-s automatic
modify parser: make promotion of ints and chars to type level explicit (with !)
simplify hyperkind system because there's no need to keep anything but the number of arguments
modify flexible type variable name generation. use just numbers for everything? ("T0" - maybe name conflict with userdefined)
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
  type Algebraics = Map' (([(String, Kind_1)], Map' [Type_1], Type_1), Status) -- TODO: REM STRINGS FROM FST MAP
  type Constrs = Map' (String, Status)
  data Data_3 = Data_3 String [(String, Kind_1)] Data_branch_1 deriving Show
  data Def_4 = Basic_def_4 Location_0 String (Map' Polykind) Type_1 Expression_1 deriving Show
  type Defs = Map' Expression_2
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
  data File = File (Map' (Polykind, Status)) Algebraics Constrs Types (Map' (Kind, Status)) (Map' (Bool, Status))
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
  data Polykind = Polykind [String] Kind_1 deriving Show
  data Type_1 = Application_type_1 Type_1 Type_1 | Char_type_1 Char | Int_type_1 Integer | Name_type_1 String [Kind_1]
    deriving (Eq, Show)
  data Type_1' = Basic_type_1 [(String, Kind_1)] Type_1 deriving Show
  type Types = Map' (Type_1', Status)
  algebraics :: Map' ([(String, Kind_1)], Map' [Type_1], Type_1)
  algebraics = Data.Map.fromList (second (\(a, b, c) -> (a, Data.Map.fromList b, c)) <$> algebraics')
  algebraics' :: [(String, ([(String, Kind_1)], [(String, [Type_1])], Type_1))]
  algebraics' =
    (
      (\(a, (b, c)) -> (a, (b, c, Prelude.foldl (\d -> \(e, _) -> Application_type_1 d (ntype e)) (ntype a) b))) <$>
      algebraics'')
  algebraics'' :: [(String, ([(String, Kind_1)], [(String, [Type_1])]))]
  algebraics'' = [("Comparison", ([], [("EQ", []), ("GT", []), ("LT", [])]))]
  arrow_kind :: Kind_1 -> Kind_1 -> Kind_1
  arrow_kind a = Application_kind_1 (Application_kind_1 (Name_kind_1 "Arrow") a)
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
-- TODO: are more checks necessary? is it possible that there are problems and unsafety in Name_type_1 case?
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
  constrs = Data.Map.fromList (join ((\(a, (_, b, _)) -> (\c -> (c, a)) <$> keys b) <$> assocs algebraics))
  context_union :: File -> File -> File
  context_union (File b i j d a x) (File f k l h c y) =
    File
      (Data.Map.union b f)
      (Data.Map.union i k)
      (Data.Map.union j l)
      (Data.Map.union d h)
      (Data.Map.union a c)
      (Data.Map.union x y)
  defs :: Map' Expression_2
  defs = fst <$> defs_and_types
  defs_and_types :: Map' (Expression_2, Type_1')
  defs_and_types =
    Data.Map.fromList
      (
        defs_and_types' ++
        join
          (
            (\(_, (b, c, d)) -> (\(e, f) -> (e, (type_alg f e, Basic_type_1 b (Prelude.foldr function_type d f)))) <$> c) <$>
            algebraics'))
  defs_and_types' :: [(String, (Expression_2, Type_1'))]
  defs_and_types' =
    [
      ("Add_Int", (Add_Int_expression_2, Basic_type_1 [] (function_type int_type (function_type int_type int_type)))),
      (
        "Compare_Int",
        (Compare_Int_expression_2, Basic_type_1 [] (function_type int_type (function_type int_type comparison_type)))),
      ("Crash", (Crash_expression_2, Basic_type_1 [("T", star_kind)] (ntype "T"))),
      ("Mod_Int", (Mod_Int_expression_2, Basic_type_1 [] (function_type int_type (function_type int_type int_type)))),
      (
        "Multiply_Int",
        (Multiply_Int_expression_2, Basic_type_1 [] (function_type int_type (function_type int_type int_type))))]
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
    File (old kinds) (old algebraics) (old constrs) (old (snd <$> defs_and_types)) (old hkinds) (old promotables)
  ins_new :: Ord t => t -> u -> Map t (u, Status) -> Map t (u, Status)
  ins_new a b = Data.Map.insert a (b, New)
  int_kind :: Kind_1
  int_kind = Name_kind_1 "!Int"
  int_type :: Type_1
  int_type = ntype "Int"
  kinds :: Map' Polykind
  kinds =
    (
      Polykind [] <$>
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
  make_eq (Data_2 a b c) = ins_new a (case promotable b (Data.Set.fromList [a]) of
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
  naming_typing :: String -> Tree_2 -> (Locations, File, Defs, Map' Polykind) -> Err (Locations, File, Defs, Map' Polykind)
  naming_typing f a (b, c, g, j) =
    naming f a b >>= \(d, e) -> (\(h, i, k) -> (d, h, i, k)) <$> typing (Location_1 f) e (c, g, j)
  not_promoted :: String -> Bool
  not_promoted a = case a of
    '!' : _ -> False
    _ -> True
  ntype :: String -> Type_1
  ntype a = Name_type_1 a []
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
  promotables = Data.Map.fromList ((\a -> (a, True)) <$> ["Char", "Comparison", "Int"])
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
    Just ((d, e), f) -> case Data.Map.lookup d b of
      Just (g, _) -> case g of
        Left i -> Right i
        Right i -> solve_eq_help h (d : a) b (Data.Map.union f (solve_diff i a))
      Nothing -> undefined_error "type" d (h e)
    Nothing -> Right True
  solvesys :: String -> Map' (Either Polykind Kind_1) -> [(Type_1, Type_1)] -> Err ()
  solvesys m a b =
    let
      x = Left m
    in case b of
      [] -> Right ()
      (c, d) : g -> case c of
        Application_type_1 e f -> case d of
          Application_type_1 h i -> solvesys m a ((e, h) : (f, i) : g)
          Name_type_1 h _ -> solvesys' m a h c g
          _ -> x
        Char_type_1 e -> case d of
          Char_type_1 f -> if e == f then solvesys m a g else x
          Name_type_1 f _ -> solvesys' m a f c g
          _ -> x
        Int_type_1 e -> case d of
          Int_type_1 f -> if e == f then solvesys m a g else x
          Name_type_1 f _ -> solvesys' m a f c g
          _ -> x
        Name_type_1 e v -> case d of
          Name_type_1 f w ->
            let
              n = unsafe_lookup f a
            in case unsafe_lookup e a of
              Left t -> case n of
                Left _ -> if c == d then solvesys m a g else x
                Right u -> solvesys_names m a f u e v t g
              Right t -> case n of
                Left u -> solvesys_names m a e t f w u g
                Right u -> if t == u then solvesys_rep m a e d g else x
          _ -> solvesys' m a e d g
  solvesys' :: String -> Map' (Either Polykind Kind_1) -> String -> Type_1 -> [(Type_1, Type_1)] -> Err ()
  solvesys' h a b c d = case unsafe_lookup b a of
    Left _ -> Left h
    Right e -> check_kind h b a c >>= \g -> if g == e then solvesys_rep h a b c d else Left h
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
    Err ()
  solvesys_names a b c d e f (Polykind h i) j =
    if d == repkinds (Data.Map.fromList (zip h f)) i then solvesys_rep a b c (Name_type_1 e f) j else Left a
  solvesys_rep :: String -> Map' (Either Polykind Kind_1) -> String -> Type_1 -> [(Type_1, Type_1)] -> Err ()
  solvesys_rep a b c d e = solvesys a b (sysrep c d e)
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
    in
      case c of
        Application_type_1 d e -> Application_type_1 (f d) (f e)
        Name_type_1 d _ -> if d == a then b else c
        _ -> c
  type_alg :: [t] -> String -> Expression_2
  type_alg a b =
    let
      c = show <$> [0 .. length a - 1]
    in Prelude.foldr Function_expression_2 (Algebraic_expression_2 b (Name_expression_2 <$> c)) (Name_pattern <$> c)
  type_case :: (Location_0 -> Location_1) -> Name -> Map' String -> [Pattern_0] -> [Type_1] -> Types -> Err Types
  type_case j (m @ (Name k l)) a b c d = case b of
    [] -> Right d
    e : f -> case c of
      [] -> Left ("Constructor " ++ l ++ location (j k) ++ " has been given too many fields.")
      g : h -> type_case j m a f h (case e of
        Blank_pattern -> d
        Name_pattern i -> ins_new i (Basic_type_1 [] (repl a g)) d)
  type_data_1 ::
    (Location_0 -> Location_1) ->
    Map' Kind ->
    Data_2 ->
    (Map' (Polykind, Status), Constrs, Defs, Map' Polykind) ->
    Err ((Map' (Polykind, Status), Constrs, Defs, Map' Polykind), Data_3)
  type_data_1 q o (Data_2 a b c) (i, j, k, x) =
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
    in (\p ->
      let
        y = Prelude.foldr arrow_kind star_kind (snd <$> p)
      in ((ins_new a (Polykind [] y) i, l, m, Data.Map.insert a (Polykind [] y) x), Data_3 a p c)) <$> type_kinds_5 q o b
  type_data_2 ::
    (Location_0 -> Location_1) -> Data_3 -> Map' Polykind -> Map' Kind -> (Algebraics, Types) -> Err (Algebraics, Types)
  type_data_2 f (Data_3 a b c) d y (p, e) =
    let
      g = type_kinds b d
      x = Prelude.foldl (\n -> Application_type_1 n <$> ntype) (ntype a) (fst <$> b)
    in case c of
      Algebraic_data_1 h ->
        (
          (\q ->
            (
              ins_new a (b, Data.Map.fromList ((\(Form_2 r s) -> (r, s)) <$> q), x) p,
              Prelude.foldl (flip (\(Form_2 l m) -> ins_new l (Basic_type_1 b (Prelude.foldr function_type x m)))) e q)) <$>
          type_forms f h g y)
      Struct_data_1 h ->
        (
          (\i ->
            (
              p,
              Prelude.foldl
                (flip (\(k, l) -> ins_new k (Basic_type_1 b (function_type x l))))
                (ins_new a (Basic_type_1 b (Prelude.foldr (function_type <$> snd) x i)) e)
                i)) <$>
          type_fields f h g y)
  type_datas ::
    (Location_0 -> Location_1) ->
    [Data_2] ->
    (Map' (Polykind, Status), Algebraics, Constrs, Types, Map' (Kind, Status), Defs, Map' Polykind, Map' (Bool, Status)) ->
    Err (Map' (Polykind, Status), Algebraics, Constrs, Types, Map' (Kind, Status), Defs, Map' Polykind, Map' (Bool, Status))
  type_datas h a (b, i, j, d, o, c, m, x) =
    (
      type_proms_1 h a (o, b, j, c, m) (make_eqs a (first Left <$> x)) >>=
      \((e, p, q, r, s), f, g, k) ->
        (
          type_proms_2 h f s (fst <$> o) (p, i, d) >>=
          \(t, l, n) ->
            (
              type_datas_1 h (fst <$> e) g (t, q, r, s) >>=
              \((u, v, w, a'), y) ->
                (
                  (\(b', c') -> (u, b', v, c', e, w, a', first unsafe_left <$> k)) <$>
                  type_datas_2 h y (fst <$> u) (fst <$> e) (l, n)))))
  type_datas_1 ::
    (Location_0 -> Location_1) ->
    Map' Kind ->
    [Data_2] ->
    (Map' (Polykind, Status), Constrs, Defs, Map' Polykind) ->
    Err ((Map' (Polykind, Status), Constrs, Defs, Map' Polykind), [Data_3])
  type_datas_1 f a b c = case b of
    [] -> Right (c, [])
    d : e -> type_data_1 f a d c >>= \(g, h) -> second ((:) h) <$> type_datas_1 f a e g
  type_datas_2 ::
    (Location_0 -> Location_1) -> [Data_3] -> Map' Polykind -> Map' Kind -> (Algebraics, Types) -> Err (Algebraics, Types)
  type_datas_2 f a b y c = case a of
    [] -> Right c
    d : e -> type_data_2 f d b y c >>= type_datas_2 f e b y
  type_def_1 :: (Location_0 -> Location_1) -> Map' Kind -> Def_2 -> Map' (Polykind, Status) -> Types -> Err (Def_4, Types)
  type_def_1 l x a b c = case a of
    Basic_def_2 f d e g i ->
      (
        type_kinds_0 l x e (fst <$> b) >>=
        \(y, j) -> (\h -> (Basic_def_4 f d j h i, ins_new d (Basic_type_1 y h) c)) <$> type_type l g j x star_kind)
  type_def_2 :: (Location_0 -> Location_1) -> Def_4 -> (Algebraics, Constrs, Types) -> Defs -> Err Defs
  type_def_2 j a (d, l, k) c = case a of
    Basic_def_4 r e b h i ->
      flip (Data.Map.insert e) c <$> type_expr ("function " ++ e ++ location (j r)) h j (Left <$> b, d, l, k) i
  type_defs ::
    (Location_0 -> Location_1) ->
    Map' Kind ->
    [Def_2] ->
    (Map' (Polykind, Status), Algebraics, Constrs) ->
    (Defs, Types) ->
    Err (Defs, Types)
  type_defs h x a (b, i, j) (c, d) = type_defs_1 h x a b d >>= \(g, e) -> (\f -> (f, e)) <$> type_defs_2 h g (i, j, e) c
  type_defs_1 ::
    (Location_0 -> Location_1) -> Map' Kind -> [Def_2] -> Map' (Polykind, Status) -> Types -> Err ([Def_4], Types)
  type_defs_1 h x a b c = case a of
    [] -> Right ([], c)
    d : e -> type_def_1 h x d b c >>= \(f, g) -> first ((:) f) <$> type_defs_1 h x e b g
  type_defs_2 :: (Location_0 -> Location_1) -> [Def_4] -> (Algebraics, Constrs, Types) -> Defs -> Err Defs
  type_defs_2 f a b c = case a of
    [] -> Right c
    d : e -> type_def_2 f d b c >>= type_defs_2 f e b
  type_expr ::
    String ->
    Type_1 ->
    (Location_0 -> Location_1) ->
    (Map' (Either Polykind Kind_1), Algebraics, Constrs, Types) ->
    Expression_1 ->
    Err Expression_2
  type_expr k h a (b, c, d, e) f =
    type_expression c d a 0 0 b [] e f h >>= \(g, i, j, _, _) -> g <$ solvesys ("Type error in " ++ k ++ ".") i j
  type_expr' :: (Location_0 -> Location_1) -> (Map' Polykind, Algebraics, Constrs, Types) -> Expression_1 -> Err Expression_2
  type_expr' a (b, c, d, e) = type_expr "input" (ntype "!") a (Data.Map.insert "!" (Right star_kind) (Left <$> b), c, d, e)
  type_expression ::
    Algebraics ->
    Constrs ->
    (Location_0 -> Location_1) ->
    Integer ->
    Integer ->
    Map' (Either Polykind Kind_1) ->
    [(Type_1, Type_1)] ->
    Types ->
    Expression_1 ->
    Type_1 ->
    Err (Expression_2, Map' (Either Polykind Kind_1), [(Type_1, Type_1)], Integer, Integer)
  type_expression v w r o s f h d (Expression_1 a b) e =
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
            (function_type (ntype (show o)) e) >>=
          \(i, j, k, p, t) ->
            (
              (\(l, m, n, q, u) -> (Application_expression_2 i l, m, n, q, u)) <$>
              type_expression v w r p t j k d g (ntype (show o))))
      Char_expression_1 c -> Right (Char_expression_2 c, f, (e, char_type) : h, o, s)
      Function_expression_1 c g ->
        (
          (\(a', b', c', d', e') -> (Function_expression_2 c a', b', c', d', e')) <$>
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
              Name_pattern i -> ins_new i (Basic_type_1 [] (ntype (show o))) d)
            g
            (ntype (show (o + 1))))
      Int_expression_1 c -> Right (Int_expression_2 c, f, (e, int_type) : h, o, s)
      Match_expression_1 c g -> case g of
        Matches_Algebraic_1 i j -> case i of
          [] -> undefined
          Match_Algebraic_1 (Name _ l) _ _ : _ ->
            let
              y0 =
                "Match error" ++ x' ++ " Undefined algebraic constructors, incompatible constructors or conflicting cases."
            in case Data.Map.lookup l w of
              Just (m, _) ->
                let
                  (n, p, q) = fst (unsafe_lookup m v)
                  (t, u) = typevars (\v' -> '#' : v' ++ show s) n (Data.Map.empty, f)
                  l0 = repl t q
                in (
                  type_expression v w r o (s + 1) u h d c l0 >>=
                  \(x, y, a0, b0, c0) ->
                    type_matches_algebraic v w r b0 c0 y a0 d Data.Map.empty i e p y0 t >>= \(d0, e0, f0, g0, h0, i0) ->
                      let
                        k0 = Match_expression_2 x <$> Matches_Algebraic_2 d0
                      in if Data.Map.null i0 then case j of
                        Just _ -> Left ("Unnecessary default case " ++ x')
                        Nothing -> Right (k0 Nothing, e0, f0, g0, h0) else case j of
                          Just j0 ->
                            (
                              (\(a', b', c', d', e') -> (k0 (Just a'), b', c', d', e')) <$>
                              type_expression v w r g0 h0 e0 f0 d j0 e)
                          Nothing -> Left ("Incomplete match" ++ x'))
              Nothing -> Left y0
        Matches_char_1 i j ->
          (
            type_expression v w r o s f h d c char_type >>=
            \(k, l, m, n, p) ->
              (
                type_matches_char v w r n p l m d Data.Map.empty i e >>=
                \(q, t, u, x, y) ->
                  (
                    (\(a0, b0, c0, d0, e0) -> (Match_expression_2 k (Matches_char_2 q a0), b0, c0, d0, e0)) <$>
                    type_expression v w r x y t u d j e)))
        Matches_Int_1 i j ->
          (
            type_expression v w r o s f h d c int_type >>=
            \(k, l, m, n, p) ->
              (
                type_matches_int v w r n p l m d Data.Map.empty i e >>=
                \(q, t, u, x, y) ->
                  (
                    (\(a0, b0, c0, d0, e0) -> (Match_expression_2 k (Matches_Int_2 q a0), b0, c0, d0, e0)) <$>
                    type_expression v w r x y t u d j e)))
{-
INEFFICIENCY.
ONE COULD CONSTRUCT AN IDENTITY MAP AND PUT IT INTO BASIC_TYPE AND THEN MAP (++ SUFFIX) OVER IT
OR SUFFIX COULD BE GIVEN AS ARGUMENT TO REPL AND ADDED INSIDE REPL
-}
      Name_expression_1 c -> case Data.Map.lookup c d of
        Just (Basic_type_1 i j, _) ->
          let
            (k, l, m) = case i of
              [] -> (f, j, s)
              _ ->
                let
                  (n, p) = type_kinds'' i s f
                in (n, repl p j, s + 1)
          in Right (Name_expression_2 c, k, (e, l) : h, o, m)
        Nothing -> undefined_error "variable" c (r a)
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
  type_kind (a, b) = Data.Map.insert a (Polykind [] b)
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
      Star_kind -> Left ("Kind mismatch " ++ location' (a c))
    Name_kind_0 e -> case Data.Map.lookup e b of
      Just f -> Right (Name_kind_1 e, f)
      Nothing -> undefined_error "kind" e (a c)
  type_kind_7 :: (Location_0 -> Location_1) -> Map' Kind -> Kind -> Kind_0 -> Err Kind_1
  type_kind_7 a b c (Kind_0 d e) = case e of
    Application_kind_0 f g -> type_kind_6 a b f >>= \(h, i) -> case i of
      Arrow_kind j k ->
        if k == c then Application_kind_1 h <$> type_kind_7 a b j g else Left ("Kind mismatch " ++ location' (a d))
      Star_kind -> Left ("Kind mismatch " ++ location' (a d))
    Name_kind_0 f -> case Data.Map.lookup f b of
      Just g -> if g == c then Right (Name_kind_1 f) else Left ("Kind mismatch " ++ location' (a d))
      Nothing -> undefined_error "kind" f (a d)
  type_kinds :: [(String, Kind_1)] -> Map' Polykind -> Map' Polykind
  type_kinds a b = case a of
    [] -> b
    c : d -> type_kinds d (type_kind c b)
  type_kinds'' ::
    [(String, Kind_1)] -> Integer -> Map' (Either Polykind Kind_1) -> (Map' (Either Polykind Kind_1), Map' String)
  type_kinds'' a b c = type_kinds_3 a (show b) c Data.Map.empty
  type_kinds_0 ::
    (Location_0 -> Location_1) ->
    Map' Kind ->
    [(String, Kind_0)] ->
    Map' Polykind ->
    Err ([(String, Kind_1)], Map' Polykind)
  type_kinds_0 a b c d = case c of
    [] -> Right ([], d)
    (e, f) : g ->
      type_kind_7 a b Star_kind f >>= \h -> first ((:) (e, h)) <$> type_kinds_0 a b g (Data.Map.insert e (Polykind [] h) d)
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
  type_match_algebraic ::
    Algebraics ->
    Constrs ->
    (Location_0 -> Location_1) ->
    Integer ->
    Integer ->
    Map' (Either Polykind Kind_1) ->
    [(Type_1, Type_1)] ->
    Types ->
    Map' Match_Algebraic_2 ->
    Match_Algebraic_1 ->
    Type_1 ->
    Map' [Type_1] ->
    String ->
    Map' String ->
    Err (Map' Match_Algebraic_2, Map' (Either Polykind Kind_1), [(Type_1, Type_1)], Integer, Integer, Map' [Type_1])
  type_match_algebraic a b c d e f g h i (Match_Algebraic_1 (Name j k) l m) n o q r = case find_and_delete o k of
    Just (p, y) ->
      (
        type_case c (Name j k) r l p h >>=
        \s ->
          (
            (\(t, u, v, w, x) -> (Data.Map.insert k (Match_Algebraic_2 l t) i, u, v, w, x, y)) <$>
            type_expression a b c d e f g s m n))
    Nothing -> Left q
  type_match_char ::
    Algebraics ->
    Constrs ->
    (Location_0 -> Location_1) ->
    Integer ->
    Integer ->
    Map' (Either Polykind Kind_1) ->
    [(Type_1, Type_1)] ->
    Types ->
    Map Char Expression_2 ->
    Match_char_1 ->
    Type_1 ->
    Err (Map Char Expression_2, Map' (Either Polykind Kind_1), [(Type_1, Type_1)], Integer, Integer)
  type_match_char a b c d e f g h i (Match_char_1 j k) l =
-- TODO: PUT A GOOD ERROR MESSAGE HERE. LOCATIONS AND STUFF.
    (
      type_expression a b c d e f g h k l >>=
      \(m, n, o, p, q) ->
        bimap (\_ -> location_err' ("cases for " ++ show j) undefined undefined) (\r -> (r, n, o, p, q)) (add i j m))
  type_match_int ::
    Algebraics ->
    Constrs ->
    (Location_0 -> Location_1) ->
    Integer ->
    Integer ->
    Map' (Either Polykind Kind_1) ->
    [(Type_1, Type_1)] ->
    Types ->
    Map Integer Expression_2 ->
    Match_Int_1 ->
    Type_1 ->
    Err (Map Integer Expression_2, Map' (Either Polykind Kind_1), [(Type_1, Type_1)], Integer, Integer)
  type_match_int a b c d e f g h i (Match_Int_1 j k) l =
-- TODO: PUT A GOOD ERROR MESSAGE HERE. LOCATIONS AND STUFF.
    (
      type_expression a b c d e f g h k l >>=
      \(m, n, o, p, q) ->
        bimap (\_ -> location_err' ("cases for " ++ show j) undefined undefined) (\r -> (r, n, o, p, q)) (add i j m))
  type_matches_algebraic ::
    Algebraics ->
    Constrs ->
    (Location_0 -> Location_1) ->
    Integer ->
    Integer ->
    Map' (Either Polykind Kind_1) ->
    [(Type_1, Type_1)] ->
    Types ->
    Map' Match_Algebraic_2 ->
    [Match_Algebraic_1] ->
    Type_1 ->
    Map' [Type_1] ->
    String ->
    Map' String ->
    Err (Map' Match_Algebraic_2, Map' (Either Polykind Kind_1), [(Type_1, Type_1)], Integer, Integer, Map' [Type_1])
  type_matches_algebraic a b c d e f g h i j k s u v = case j of
    [] -> Right (i, f, g, d, e, s)
    l : m ->
      (
        type_match_algebraic a b c d e f g h i l k s u v >>=
        \(n, o, p, q, r, t) -> type_matches_algebraic a b c q r o p h n m k t u v)
  type_matches_char ::
    Algebraics ->
    Constrs ->
    (Location_0 -> Location_1) ->
    Integer ->
    Integer ->
    Map' (Either Polykind Kind_1) ->
    [(Type_1, Type_1)] ->
    Types ->
    Map Char Expression_2 ->
    [Match_char_1] ->
    Type_1 ->
    Err (Map Char Expression_2, Map' (Either Polykind Kind_1), [(Type_1, Type_1)], Integer, Integer)
  type_matches_char a b c d e f g h i j k = case j of
    [] -> Right (i, f, g, d, e)
    l : m -> type_match_char a b c d e f g h i l k >>= \(n, o, p, q, r) -> type_matches_char a b c q r o p h n m k
  type_matches_int ::
    Algebraics ->
    Constrs ->
    (Location_0 -> Location_1) ->
    Integer ->
    Integer ->
    Map' (Either Polykind Kind_1) ->
    [(Type_1, Type_1)] ->
    Types ->
    Map Integer Expression_2 ->
    [Match_Int_1] ->
    Type_1 ->
    Err (Map Integer Expression_2, Map' (Either Polykind Kind_1), [(Type_1, Type_1)], Integer, Integer)
  type_matches_int a b c d e f g h i j k = case j of
    [] -> Right (i, f, g, d, e)
    l : m -> type_match_int a b c d e f g h i l k >>= \(n, o, p, q, r) -> type_matches_int a b c q r o p h n m k
  type_prom_1 ::
    (Location_0 -> Location_1) ->
    Data_2 ->
    (Map' (Kind, Status), Map' (Polykind, Status), Constrs, Defs, Map' Polykind) ->
    Map' (Either Bool (Map' Location_0), Status) ->
    Err
      (
        Maybe ((Map' (Kind, Status), Map' (Polykind, Status), Constrs, Defs, Map' Polykind), Data_3),
        Map' (Either Bool (Map' Location_0), Status))
  type_prom_1 q (Data_2 a b c) (o, i, j, k, x) j' = first (\k' -> if k' then
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
        p = second (return star_kind) <$> b
        y = Prelude.foldr arrow_kind star_kind (snd <$> p)
      in
        Just
          (
            (
              ins_new ('!' : a) (Prelude.foldr Arrow_kind Star_kind (return Star_kind <$> b)) o,
              ins_new a (Polykind [] y) i,
              l,
              m,
              Data.Map.insert a (Polykind [] y) x),
            Data_3 a p c) else Nothing) <$> solve_eq q a j'
  type_prom_2 ::
    (Location_0 -> Location_1) ->
    Data_3 ->
    Map' Polykind ->
    Map' Kind ->
    (Map' (Polykind, Status), Algebraics, Types) ->
    Err (Map' (Polykind, Status), Algebraics, Types)
  type_prom_2 f (Data_3 a b c) d y' (d', p, e) =
    let
      g = type_kinds b d
      x = Prelude.foldl (\n -> \y -> Application_type_1 n (ntype y)) (ntype a) (fst <$> b)
      promhelp p' q' =
        ins_new
          ('!' : p')
          (Polykind
            (fst <$> b)
            (Prelude.foldr
              (\x' -> arrow_kind (prom_type (Data.Set.fromList (fst <$> b)) x'))
              (Prelude.foldl (\t' -> \u' -> Application_kind_1 t' (Name_kind_1 u')) star_kind (fst <$> b))
              q'))
    in case c of
      Algebraic_data_1 h ->
        (
          (\q ->
            (
              Prelude.foldl (\t -> \(Form_2 u v) -> promhelp u v t) d' q,
              ins_new a (b, Data.Map.fromList ((\(Form_2 r s) -> (r, s)) <$> q), x) p,
              Prelude.foldl (flip (\(Form_2 l m) -> ins_new l (Basic_type_1 b (Prelude.foldr function_type x m)))) e q)) <$>
          type_forms f h g y')
      Struct_data_1 h ->
        (
          (\i ->
            (
              promhelp a (snd <$> i) d',
              p,
              Prelude.foldl
                (flip (\(k, l) -> ins_new k (Basic_type_1 b (function_type x l))))
                (ins_new a (Basic_type_1 b (Prelude.foldr (function_type <$> snd) x i)) e)
                i)) <$>
          type_fields f h g y')
  type_proms_1 ::
    (Location_0 -> Location_1) ->
    [Data_2] ->
    (Map' (Kind, Status), Map' (Polykind, Status), Constrs, Defs, Map' Polykind) ->
    Map' (Either Bool (Map' Location_0), Status) ->
    Err
      (
        (Map' (Kind, Status), Map' (Polykind, Status), Constrs, Defs, Map' Polykind),
        [Data_3],
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
    [Data_3] ->
    Map' Polykind ->
    Map' Kind ->
    (Map' (Polykind, Status), Algebraics, Types) ->
    Err (Map' (Polykind, Status), Algebraics, Types)
  type_proms_2 a b c y d = case b of
    [] -> Right d
    e : f -> type_prom_2 a e c y d >>= type_proms_2 a f c y
  type_type :: (Location_0 -> Location_1) -> Type_0 -> Map' Polykind -> Map' Kind -> Kind_1 -> Err Type_1
  type_type l (Type_0 a c) d y e =
    let
      x = Left ("Kind mismatch" ++ location' (l a))
    in case c of
      Application_type_0 f g -> type_type' l f d y >>= \(h, i) -> case i of
        Application_kind_1 (Application_kind_1 (Name_kind_1 "Arrow") j) k ->
          if k == e then Application_type_1 h <$> type_type l g d y j else x
        _ -> x
      Char_type_0 b -> if e == char_kind then Right (Char_type_1 b) else x
      Int_type_0 b -> if e == int_kind then Right (Int_type_1 b) else x
      Name_type_0 f b -> case Data.Map.lookup f d of
        Just (Polykind h g) ->
          ziphelp l y f a Data.Map.empty h b >>= \(z, w) -> if repkinds w g == e then Right (Name_type_1 f z) else x
        Nothing -> undefined_error "type" f (l a)
  type_type' :: (Location_0 -> Location_1) -> Type_0 -> Map' Polykind -> Map' Kind -> Err (Type_1, Kind_1)
  type_type' l (Type_0 a c) d y =
    let
      b e = Left (e ++ location' (l a))
    in case c of
      Application_type_0 e f -> type_type' l e d y >>= \(g, h) -> case h of
        Application_kind_1 (Application_kind_1 (Name_kind_1 "Arrow") i) j ->
          (\k -> (Application_type_1 g k, j)) <$> type_type l f d y i
        _ -> b "Kind mismatch"
      Char_type_0 e -> Right (Char_type_1 e, char_kind)
      Int_type_0 e -> Right (Int_type_1 e, int_kind)
      Name_type_0 e g -> case Data.Map.lookup e d of
        Just (Polykind h f) -> bimap (Name_type_1 e) (\w -> repkinds w f) <$> ziphelp l y e a Data.Map.empty h g
        Nothing -> undefined_error "type" e (l a)
  type_types :: (Location_0 -> Location_1) -> [Type_0] -> Map' Polykind -> Map' Kind -> Err [Type_1]
  type_types f a b g = case a of
    [] -> Right []
    c : d -> type_type f c b g star_kind >>= \e -> (:) e <$> type_types f d b g
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
  typing :: (Location_0 -> Location_1) -> Tree_5 -> (File, Defs, Map' Polykind) -> Err (File, Defs, Map' Polykind)
  typing k (Tree_5 a c) (File d t u v w w0, l, m) =
    (
      type_datas k a (d, t, u, v, w, l, m, w0) >>=
      \(e, b, h, g, o, f, n, w1) ->
        (
          (\(i, j) -> (File (rem_old e) (rem_old b) (rem_old h) (rem_old j) (rem_old o) (rem_old w1), i, n)) <$>
          type_defs k (fst <$> o) c (e, b, h) (f, g)))
  undefined_error :: String -> String -> Location_1 -> Err t
  undefined_error a b c = Left ("Undefined " ++ a ++ " " ++ b ++ location' c)
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