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
  import Naming
  import Standard
  import Tokenise
  import Tree
  type Algebraics = Map' (([(String, Kind_1)], Map' [Type_1], Type_1), Status) -- TODO: REM STRINGS FROM FST MAP
  type Constrs = Map' (String, Status)
  data Data_3 = Data_3 String [(String, Kind_1)] Data_branch_1 deriving Show
  data Def_4 = Basic_def_4 Location_0 String (Map' Kind_1) Type_1 Expression_1 deriving Show
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
  data File = File (Map' (Kind_1, Status)) Algebraics Constrs Types (Map' (Kind, Status)) deriving Show
  data Form_2 = Form_2 String [Type_1] deriving Show
  data Kind = Arrow_kind Kind Kind | Star_kind deriving (Eq, Show)
  data Kind_1 = Application_kind_1 Kind_1 Kind_1 | Name_kind_1 String deriving (Eq, Show)
  data Match_Algebraic_2 = Match_Algebraic_2 [Pattern_0] Expression_2 deriving Show
  data Matches_2 =
    Matches_Algebraic_2 (Map' Match_Algebraic_2) (Maybe Expression_2) |
    Matches_char_2 (Map Char Expression_2) Expression_2 |
    Matches_Int_2 (Map Integer Expression_2) Expression_2
      deriving Show
  data Status' = Fixed | Flexible deriving Show
  data Type_1 = Application_type_1 Type_1 Type_1 | Char_type_1 Char | Int_type_1 Integer | Name_type_1 String deriving Show
  data Type_1' = Basic_type_1 [(String, Kind_1)] Type_1 | Local_type_1 Type_1 deriving Show
  type Types = Map' (Type_1', Status)
  algebraics :: Map' ([(String, Kind_1)], Map' [Type_1], Type_1)
  algebraics = fromList (second (\(a, b, c) -> (a, fromList b, c)) <$> algebraics')
  algebraics' :: [(String, ([(String, Kind_1)], [(String, [Type_1])], Type_1))]
  algebraics' =
    (
      (\(a, (b, c)) ->
        (a, (b, c, Prelude.foldl (\d -> \(e, _) -> Application_type_1 d (Name_type_1 e)) (Name_type_1 a) b))) <$>
      algebraics'')
  algebraics'' :: [(String, ([(String, Kind_1)], [(String, [Type_1])]))]
  algebraics'' = [("Comparison", ([], [("EQ", []), ("GT", []), ("LT", [])]))]
  arrow_kind :: Kind_1 -> Kind_1 -> Kind_1
  arrow_kind a = Application_kind_1 (Application_kind_1 (Name_kind_1 "Arrow") a)
  char_kind :: Kind_1
  char_kind = Name_kind_1 "!Char"
  char_type :: Type_1
  char_type = Name_type_1 "Char"
  check_kind :: String -> String -> Map' (Kind_1, Status') -> Type_1 -> Err Kind_1
  check_kind j c a b = case b of
    Application_type_1 d e -> check_kind j c a d >>= \f -> case f of
      Application_kind_1 (Application_kind_1 (Name_kind_1 "Arrow") g) h ->
        check_kind j c a e >>= \i -> if i == g then Right h else Left j
      _ -> Left j
    Char_type_1 _ -> Right char_kind
    Int_type_1 _ -> Right int_kind
    Name_type_1 d -> if d == c then Left j else Right (fst (unsafe_lookup d a))
  comparison_kind :: Kind_1
  comparison_kind = Name_kind_1 "!Comparison"
  comparison_type :: Type_1
  comparison_type = Name_type_1 "Comparison"
  constrs :: Map' String
  constrs = fromList (join ((\(a, (_, b, _)) -> (\c -> (c, a)) <$> keys b) <$> assocs algebraics))
  context_union :: File -> File -> File
  context_union (File b i j d a) (File f k l h c) =
    File (Data.Map.union b f) (Data.Map.union i k) (Data.Map.union j l) (Data.Map.union d h) (Data.Map.union a c)
  defs :: Map' Expression_2
  defs = fst <$> defs_and_types
  defs_and_types :: Map' (Expression_2, Type_1')
  defs_and_types =
    fromList
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
      ("Crash", (Crash_expression_2, Basic_type_1 [("T", star_kind)] (Name_type_1 "T"))),
      ("Mod_Int", (Mod_Int_expression_2, Basic_type_1 [] (function_type int_type (function_type int_type int_type)))),
      (
        "Multiply_Int",
        (Multiply_Int_expression_2, Basic_type_1 [] (function_type int_type (function_type int_type int_type))))]
  find_and_delete :: Ord t => Map t u -> t -> Maybe (u, Map t u)
  find_and_delete a b = (\c -> (c, Data.Map.delete b a)) <$> Data.Map.lookup b a
  function_type :: Type_1 -> Type_1 -> Type_1
  function_type a = Application_type_1 (Application_type_1 (Name_type_1 "Function") a)
  hkinds :: Map' Kind
  hkinds =
    fromList
      [
        ("Arrow", Arrow_kind Star_kind (Arrow_kind Star_kind Star_kind)),
        ("!Char", Star_kind),
        ("!Comparison", Star_kind),
        ("!Int", Star_kind),
        ("Star", Star_kind)]
  init_type_context :: File
  init_type_context = File (old kinds) (old algebraics) (old constrs) (old (snd <$> defs_and_types)) (old hkinds)
  ins_new :: Ord t => t -> u -> Map t (u, Status) -> Map t (u, Status)
  ins_new a b = Data.Map.insert a (b, New)
  int_kind :: Kind_1
  int_kind = Name_kind_1 "!Int"
  int_type :: Type_1
  int_type = Name_type_1 "Int"
  kinds :: Map' Kind_1
  kinds = fromList (kinds' ++ (second (\(a, _, _) -> Prelude.foldr arrow_kind star_kind (snd <$> a)) <$> algebraics'))
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
    fromList (flip (,) (Language) <$> (Data.List.filter not_promoted (keys hkinds ++ keys kinds) ++ keys defs_and_types))
  naming_typing :: String -> Tree_2 -> (Locations, File, Defs, Map' Kind_1) -> Err (Locations, File, Defs, Map' Kind_1)
  naming_typing f a (b, c, g, j) =
    naming f a b >>= \(d, e) -> (\(h, i, k) -> (d, h, i, k)) <$> typing (Location_1 f) e (c, g, j)
  not_promoted :: String -> Bool
  not_promoted a = case a of
    '!' : _ -> False
    _ -> True
  promotable :: Data_2 -> Bool
  promotable (Data_2 _ a _) = Prelude.foldr (&&) True (promotable' <$> snd <$> a)
  promotable' :: Kind_0 -> Bool
  promotable' (Kind_0 _ a) = a == Name_kind_0 "Star"
  repl :: Map' String -> Type_1 -> Type_1
  repl a b = case b of
    Application_type_1 c d -> Application_type_1 (repl a c) (repl a d)
    Name_type_1 c -> Name_type_1 (case Data.Map.lookup c a of
      Just d -> d
      Nothing -> c)
    _ -> b
  solvesys :: String -> Map' (Kind_1, Status') -> [(Type_1, Type_1)] -> Err ()
  solvesys m a b = case b of
    [] -> Right ()
    (c, d) : g -> case c of
      Application_type_1 e f -> case d of
        Application_type_1 h i -> solvesys m a ((e, h) : (f, i) : g)
        Name_type_1 h -> solvesys' m a h c g
        _ -> Left m
      Char_type_1 e -> case d of
        Char_type_1 f -> if e == f then solvesys m a g else Left m
        Name_type_1 f -> solvesys' m a f c g
        _ -> Left m
      Int_type_1 e -> case d of
        Int_type_1 f -> if e == f then solvesys m a g else Left m
        Name_type_1 f -> solvesys' m a f c g
        _ -> Left m
      Name_type_1 e -> case d of
        Name_type_1 f ->
          let
            (h, j) = unsafe_lookup e a
            (k, l) = unsafe_lookup f a
          in if h == k then case j of
            Fixed -> case l of
              Fixed -> if e == f then solvesys m a g else Left m
              Flexible -> solvesys m a (sysrep f c g)
            Flexible -> solvesys m a (sysrep e d g) else Left m
        _ -> solvesys' m a e d g
  solvesys' :: String -> Map' (Kind_1, Status') -> String -> Type_1 -> [(Type_1, Type_1)] -> Err ()
  solvesys' h a b c d =
    let
      (e, f) = unsafe_lookup b a
    in case f of
      Fixed -> Left h
      Flexible -> check_kind h b a c >>= \g -> if g == e then solvesys h a (sysrep b c d) else Left h
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
        Name_type_1 d -> if d == a then b else c
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
        Name_pattern i -> ins_new i (Local_type_1 (repl a g)) d)
{-
Kuidas otsustada, kas andmetüüp edutatakse?
Tal on ainult Star liiki tüübiparameetrid
Structi jaoks:
Kõigi väljade tüübid koosnevad ainult tüüpidest mis on ise edutatavad
Algebralise andmetüübi jaoks:
Iga andmekonstruktori väljade tüübid koosnevad ainult tüüpidest mis on ise edutatavad
Mis ei ole edutatav?
1) Andmetüüp millel on tüübiparameetreid mis ei ole liiki Star
2) GADT
3) Andmetüüp mille mõnel konstruktoril on tüüp mis ei ole edutatav
Kuidas edutamine käib?
1) Tüübikonstruktorist saab liigikonstruktor
   Comparison : Star                 =>   !Comparison : *
   Either : Star -> Star -> Star     =>   !Either : * -> * -> *
   List : Star -> Star               =>   !List : * -> *
   Logical : Star                    =>   !Logical : * -> *
   Maybe : Star -> Star              =>   !Maybe : * -> *
   Nat : Star                        =>   !Nat : *
   Pair : Star -> Star -> Star       =>   !Pair : * -> * -> *
   Trivial : Star                    =>   !Trivial : *
Nimele tuleb ette hüüumärk.
Liik Star muutub hüperliigiks *.
Liik Arrow muutub hüperliigiks ->.
2) Andmekonstruktoritest saavad tüübikonstruktorid
   Cons[T : Star] : T -> List T -> List T          =>   !Cons[K : *] : K -> !List K -> !List K
   EQ : Comparison                                 =>   !EQ : !Comparison
   Empty[T : Star] : List T                        =>   !Empty[K : *] : !List K
   False : Logical                                 =>   !False : !Logical
   GT : Comparison                                 =>   !GT : !Comparison
   LT : Comparison                                 =>   !LT : !Comparison
   Left[T : Star, U : Star] : T -> Either T U      =>   !Left[K : *, L : *] : K -> !Either K L
   Next : Nat -> Nat                               =>   !Next : !Nat -> !Nat
   Nothing[T : Star] : Maybe T                     =>   !Nothing[K : *] : !Maybe K
   Pair[T : Star, U : Star] : T -> U -> Pair T U   =>   !Pair[K : *, L : *] : K -> L -> !Pair K L
   Right[T : Star, U : Star] : U -> Either T U     =>   !Right[K : *, L : *] : L -> !Either K L
   Trivial : Trivial                               =>   !Trivial : !Trivial
   True : Logical                                  =>   !True : !Logical
   Wrap[T : Star] : T -> Maybe T                   =>   !Wrap[K : *] : K -> !Maybe K
   Zero : Nat                                      =>   !Zero : !Nat
Nimele tuleb ette hüüumärk.
Tüübimuutujad (mis on alati liiki Star) muutuvad liigimuutujateks (mis on alati hüperliiki * mida eksplitsiitselt ei hoita)
Tüübimuutuja T muutub liigimuutujaks T
Tüüp Function muutub liigiks Arrow
Tüübikonstruktor muutub vastavaks edutatud liigikonstruktoriks

Either_Char_Int : Star => !Either_Char_Int : *
Fun_wrapper : Star -> Star -> Star => !Fun_wrapper : * -> * -> *
Fun_wrapper[T : Star, U : Star] : (T -> U) -> Fun_wrapper T U => !Fun_wrapper[K : *, L : *] : (K -> L) -> !Fun_wrapper K L
Left_Char_Int : Char -> Either_Char_Int => !Left_Char_Int : !Char -> !Either_Char_Int
Pair_Char_Int : Star => !Pair_Char_Int : *
Pair_Char_Int : Char -> Int -> Pair_Char_Int => !Pair_Char_Int : !Char -> !Int -> !Pair_Char_Int
Right_Char_Int : Int -> Either_Char_Int => !Right_Char_Int : !Int -> !Either_Char_Int
-}
  type_data_1 ::
    (Location_0 -> Location_1) ->
    Map' (Kind, Status) ->
    Data_2 ->
    (Map' (Kind_1, Status), Constrs, Defs, Map' Kind_1) ->
    Err ((Map' (Kind_1, Status), Constrs, Defs, Map' Kind_1), Data_3)
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
                    (Struct_expression_2 (fromList ((\f -> (f, Name_expression_2 ('#' : f))) <$> e')))
                    e')
                  k)
                e')
    in (\p ->
      let
        y = Prelude.foldr arrow_kind star_kind (snd <$> p)
      in ((ins_new a y i, l, m, Data.Map.insert a y x), Data_3 a p c)) <$> type_kinds_5 q o b
  type_data_2 :: (Location_0 -> Location_1) -> Data_3 -> Map' Kind_1 -> (Algebraics, Types) -> Err (Algebraics, Types)
  type_data_2 f (Data_3 a b c) d (p, e) =
    let
      g = type_kinds b d
      x = Prelude.foldl (\n -> Application_type_1 n <$> Name_type_1) (Name_type_1 a) (fst <$> b)
    in case c of
      Algebraic_data_1 h ->
        (
          (\q ->
            (
              ins_new a (b, fromList ((\(Form_2 r s) -> (r, s)) <$> q), x) p,
              Prelude.foldl (flip (\(Form_2 l m) -> ins_new l (Basic_type_1 b (Prelude.foldr function_type x m)))) e q)) <$>
          type_forms f h g)
      Struct_data_1 h ->
        (
          (\i ->
            (
              p,
              Prelude.foldl
                (flip (\(k, l) -> ins_new k (Basic_type_1 b (function_type x l))))
                (ins_new a (Basic_type_1 b (Prelude.foldr (function_type <$> snd) x i)) e)
                i)) <$>
          type_fields f h g)
  type_datas ::
    (Location_0 -> Location_1) ->
    [Data_2] ->
    (Map' (Kind_1, Status), Algebraics, Constrs, Types, Map' (Kind, Status), Defs, Map' Kind_1) ->
    Err (Map' (Kind_1, Status), Algebraics, Constrs, Types, Map' (Kind, Status), Defs, Map' Kind_1)
  type_datas h a (b, i, j, d, o, c, m) = (
    type_datas_1 h o a (b, j, c, m) >>=
    \((e, k, f, n), p) -> (\(l, g) -> (e, l, k, g, o, f, n)) <$> type_datas_2 h p (fst <$> e) (i, d))
  type_datas_1 ::
    (Location_0 -> Location_1) ->
    Map' (Kind, Status) ->
    [Data_2] ->
    (Map' (Kind_1, Status), Constrs, Defs, Map' Kind_1) ->
    Err ((Map' (Kind_1, Status), Constrs, Defs, Map' Kind_1), [Data_3])
  type_datas_1 f a b c = case b of
    [] -> Right (c, [])
    d : e -> type_data_1 f a d c >>= \(g, h) -> second ((:) h) <$> type_datas_1 f a e g
  type_datas_2 :: (Location_0 -> Location_1) -> [Data_3] -> Map' Kind_1 -> (Algebraics, Types) -> Err (Algebraics, Types)
  type_datas_2 f a b c = case a of
    [] -> Right c
    d : e -> type_data_2 f d b c >>= type_datas_2 f e b
  type_def_1 ::
    (Location_0 -> Location_1) -> Map' (Kind, Status) -> Def_2 -> Map' (Kind_1, Status) -> Types -> Err (Def_4, Types)
  type_def_1 l x a b c = case a of
    Basic_def_2 f d e g i ->
      (
        type_kinds_0 l x e (fst <$> b) >>=
        \(y, j) -> (\h -> (Basic_def_4 f d j h i, ins_new d (Basic_type_1 y h) c)) <$> type_type l g j star_kind)
  type_def_2 :: (Location_0 -> Location_1) -> Def_4 -> (Algebraics, Constrs, Types) -> Defs -> Err Defs
  type_def_2 j a (d, l, k) c = case a of
    Basic_def_4 r e b h i ->
      flip (Data.Map.insert e) c <$> type_expr ("function " ++ e ++ location (j r)) h j ((\x -> (x, Fixed)) <$> b, d, l, k) i
  type_defs ::
    (Location_0 -> Location_1) ->
    Map' (Kind, Status) ->
    [Def_2] ->
    (Map' (Kind_1, Status), Algebraics, Constrs) ->
    (Defs, Types) ->
    Err (Defs, Types)
  type_defs h x a (b, i, j) (c, d) = type_defs_1 h x a b d >>= \(g, e) -> (\f -> (f, e)) <$> type_defs_2 h g (i, j, e) c
  type_defs_1 ::
    (Location_0 -> Location_1) -> Map' (Kind, Status) -> [Def_2] -> Map' (Kind_1, Status) -> Types -> Err ([Def_4], Types)
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
    (Map' (Kind_1, Status'), Algebraics, Constrs, Types) ->
    Expression_1 ->
    Err Expression_2
  type_expr k h a (b, c, d, e) f =
    type_expression c d a 0 0 b [] e f h >>= \(g, i, j, _, _) -> g <$ solvesys ("Type error in " ++ k ++ ".") i j
  type_expr' :: (Location_0 -> Location_1) -> (Map' Kind_1, Algebraics, Constrs, Types) -> Expression_1 -> Err Expression_2
  type_expr' a (b, c, d, e) =
    type_expr "input" (Name_type_1 "!") a (Data.Map.insert "!" (star_kind, Flexible) (flip (,) Fixed <$> b), c, d, e)
  type_expression ::
    Algebraics ->
    Constrs ->
    (Location_0 -> Location_1) ->
    Integer ->
    Integer ->
    Map' (Kind_1, Status') ->
    [(Type_1, Type_1)] ->
    Types ->
    Expression_1 ->
    Type_1 ->
    Err (Expression_2, Map' (Kind_1, Status'), [(Type_1, Type_1)], Integer, Integer)
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
            (Data.Map.insert (show o) (star_kind, Flexible) f)
            h
            d
            c
            (function_type (Name_type_1 (show o)) e) >>=
          \(i, j, k, p, t) ->
            (
              (\(l, m, n, q, u) -> (Application_expression_2 i l, m, n, q, u)) <$>
              type_expression v w r p t j k d g (Name_type_1 (show o))))
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
            (Data.Map.insert (show (o + 1)) (star_kind, Flexible) (Data.Map.insert (show o) (star_kind, Flexible) f))
            ((e, function_type (Name_type_1 (show o)) (Name_type_1 (show (o + 1)))) : h)
            (case c of
              Blank_pattern -> d
              Name_pattern i -> ins_new i (Local_type_1 (Name_type_1 (show o))) d)
            g
            (Name_type_1 (show (o + 1))))
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
                  (t, u) = typevars (\v' -> '#' : v' ++ show s) n (empty, f)
                  l0 = repl t q
                in (
                  type_expression v w r o (s + 1) u h d c l0 >>=
                  \(x, y, a0, b0, c0) ->
                    type_matches_algebraic v w r b0 c0 y a0 d empty i e p y0 t >>= \(d0, e0, f0, g0, h0, i0) ->
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
                type_matches_char v w r n p l m d empty i e >>=
                \(q, t, u, x, y) ->
                  (
                    (\(a0, b0, c0, d0, e0) -> (Match_expression_2 k (Matches_char_2 q a0), b0, c0, d0, e0)) <$>
                    type_expression v w r x y t u d j e)))
        Matches_Int_1 i j ->
          (
            type_expression v w r o s f h d c int_type >>=
            \(k, l, m, n, p) ->
              (
                type_matches_int v w r n p l m d empty i e >>=
                \(q, t, u, x, y) ->
                  (
                    (\(a0, b0, c0, d0, e0) -> (Match_expression_2 k (Matches_Int_2 q a0), b0, c0, d0, e0)) <$>
                    type_expression v w r x y t u d j e)))
      Name_expression_1 c -> case Data.Map.lookup c d of
        Just (g, _) ->
          let
            (k, l, m) = case g of
              Basic_type_1 i j ->
{-
INEFFICIENCY.
ONE COULD CONSTRUCT AN IDENTITY MAP AND PUT IT INTO BASIC_TYPE AND THEN MAP (++ SUFFIX) OVER IT
OR SUFFIX COULD BE GIVEN AS ARGUMENT TO REPL AND ADDED INSIDE REPL
-}
                let
                  (n, p) = type_kinds'' i s f
                in (n, repl p j, s + 1)
              Local_type_1 i -> (f, i, s)
          in Right (Name_expression_2 c, k, (e, l) : h, o, m)
        Nothing -> undefined_error "variable" c (r a)
  type_field :: (Location_0 -> Location_1) -> (String, Type_0) -> Map' Kind_1 -> Err (String, Type_1)
  type_field d (a, b) c = (,) a <$> type_type d b c star_kind
  type_fields :: (Location_0 -> Location_1) -> [(String, Type_0)] -> Map' Kind_1 -> Err [(String, Type_1)]
  type_fields f a b = case a of
    [] -> Right []
    c : d -> type_field f c b >>= \e -> (:) e <$> type_fields f d b
  type_form :: (Location_0 -> Location_1) -> Form_1 -> Map' Kind_1 -> Err Form_2
  type_form d (Form_1 a b) c = Form_2 a <$> type_types d b c
  type_forms :: (Location_0 -> Location_1) -> [Form_1] -> Map' Kind_1 -> Err [Form_2]
  type_forms f a b = case a of
    [] -> Right []
    c : d -> type_form f c b >>= \e -> (:) e <$> type_forms f d b
  type_kind :: (String, Kind_1) -> Map' Kind_1 -> Map' Kind_1
  type_kind = uncurry Data.Map.insert
  type_kind_4 ::
    (Location_0 -> Location_1) ->
    Map' (Kind, Status) ->
    (String, Kind_0) ->
    (Map' Kind_1, Map' Kind_1) ->
    Err (Map' Kind_1, Map' Kind_1)
  type_kind_4 d e (g, a) b = (\h ->
    let
      f = Data.Map.insert g h
    in bimap f f b) <$> type_kind_7 d e Star_kind a
  type_kind_6 :: (Location_0 -> Location_1) -> Map' (Kind, Status) -> Kind_0 -> Err (Kind_1, Kind)
  type_kind_6 a b (Kind_0 c d) = case d of
    Application_kind_0 e f -> type_kind_6 a b e >>= \(g, h) -> case h of
      Arrow_kind i j -> (\k -> (Application_kind_1 g k, j)) <$> type_kind_7 a b i f
      Star_kind -> Left ("Kind mismatch " ++ location' (a c))
    Name_kind_0 e -> case Data.Map.lookup e b of
      Just (f, _) -> Right (Name_kind_1 e, f)
      Nothing -> undefined_error "kind" e (a c)
  type_kind_7 :: (Location_0 -> Location_1) -> Map' (Kind, Status) -> Kind -> Kind_0 -> Err Kind_1
  type_kind_7 a b c (Kind_0 d e) = case e of
    Application_kind_0 f g -> type_kind_6 a b f >>= \(h, i) -> case i of
      Arrow_kind j k ->
        if k == c then Application_kind_1 h <$> type_kind_7 a b j g else Left ("Kind mismatch " ++ location' (a d))
      Star_kind -> Left ("Kind mismatch " ++ location' (a d))
    Name_kind_0 f -> case Data.Map.lookup f b of
      Just (g, _) -> if g == c then Right (Name_kind_1 f) else Left ("Kind mismatch " ++ location' (a d))
      Nothing -> undefined_error "kind" f (a d)
  type_kinds :: [(String, Kind_1)] -> Map' Kind_1 -> Map' Kind_1
  type_kinds a b = case a of
    [] -> b
    c : d -> type_kinds d (type_kind c b)
  type_kinds'' :: [(String, Kind_1)] -> Integer -> Map' (Kind_1, Status') -> (Map' (Kind_1, Status'), Map' String)
  type_kinds'' a b c = type_kinds_3 a (show b) c empty
  type_kinds_0 ::
    (Location_0 -> Location_1) ->
    Map' (Kind, Status) ->
    [(String, Kind_0)] ->
    Map' Kind_1 ->
    Err ([(String, Kind_1)], Map' Kind_1)
  type_kinds_0 a b c d = case c of
    [] -> Right ([], d)
    (e, f) : g -> type_kind_7 a b Star_kind f >>= \h -> first ((:) (e, h)) <$> type_kinds_0 a b g (Data.Map.insert e h d)
  type_kinds_3 ::
    [(String, Kind_1)] -> String -> Map' (Kind_1, Status') -> Map' String -> (Map' (Kind_1, Status'), Map' String)
  type_kinds_3 a b f c = case a of
    [] -> (f, c)
    (d, e) : g ->
      let
        h = d ++ b
      in
        type_kinds_3 g b (Data.Map.insert h (e, Flexible) f) (Data.Map.insert d h c)
  type_kinds_4 ::
    (Location_0 -> Location_1) ->
    Map' (Kind, Status) ->
    [(String, Kind_0)] ->
    (Map' Kind_1, Map' Kind_1) ->
    Err (Map' Kind_1, Map' Kind_1)
  type_kinds_4 e f a b = case a of
    [] -> Right b
    c : d -> type_kind_4 e f c b >>= type_kinds_4 e f d
  type_kinds_5 :: (Location_0 -> Location_1) -> Map' (Kind, Status) -> [(String, Kind_0)] -> Err [(String, Kind_1)]
  type_kinds_5 f a b = case b of
    [] -> Right []
    (g, c) : d -> type_kind_7 f a Star_kind c >>= \e -> (:) (g, e) <$> type_kinds_5 f a d
  type_match_algebraic ::
    Algebraics ->
    Constrs ->
    (Location_0 -> Location_1) ->
    Integer ->
    Integer ->
    Map' (Kind_1, Status') ->
    [(Type_1, Type_1)] ->
    Types ->
    Map' Match_Algebraic_2 ->
    Match_Algebraic_1 ->
    Type_1 ->
    Map' [Type_1] ->
    String ->
    Map' String ->
    Err (Map' Match_Algebraic_2, Map' (Kind_1, Status'), [(Type_1, Type_1)], Integer, Integer, Map' [Type_1])
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
    Map' (Kind_1, Status') ->
    [(Type_1, Type_1)] ->
    Types ->
    Map Char Expression_2 ->
    Match_char_1 ->
    Type_1 ->
    Err (Map Char Expression_2, Map' (Kind_1, Status'), [(Type_1, Type_1)], Integer, Integer)
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
    Map' (Kind_1, Status') ->
    [(Type_1, Type_1)] ->
    Types ->
    Map Integer Expression_2 ->
    Match_Int_1 ->
    Type_1 ->
    Err (Map Integer Expression_2, Map' (Kind_1, Status'), [(Type_1, Type_1)], Integer, Integer)
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
    Map' (Kind_1, Status') ->
    [(Type_1, Type_1)] ->
    Types ->
    Map' Match_Algebraic_2 ->
    [Match_Algebraic_1] ->
    Type_1 ->
    Map' [Type_1] ->
    String ->
    Map' String ->
    Err (Map' Match_Algebraic_2, Map' (Kind_1, Status'), [(Type_1, Type_1)], Integer, Integer, Map' [Type_1])
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
    Map' (Kind_1, Status') ->
    [(Type_1, Type_1)] ->
    Types ->
    Map Char Expression_2 ->
    [Match_char_1] ->
    Type_1 ->
    Err (Map Char Expression_2, Map' (Kind_1, Status'), [(Type_1, Type_1)], Integer, Integer)
  type_matches_char a b c d e f g h i j k = case j of
    [] -> Right (i, f, g, d, e)
    l : m -> type_match_char a b c d e f g h i l k >>= \(n, o, p, q, r) -> type_matches_char a b c q r o p h n m k
  type_matches_int ::
    Algebraics ->
    Constrs ->
    (Location_0 -> Location_1) ->
    Integer ->
    Integer ->
    Map' (Kind_1, Status') ->
    [(Type_1, Type_1)] ->
    Types ->
    Map Integer Expression_2 ->
    [Match_Int_1] ->
    Type_1 ->
    Err (Map Integer Expression_2, Map' (Kind_1, Status'), [(Type_1, Type_1)], Integer, Integer)
  type_matches_int a b c d e f g h i j k = case j of
    [] -> Right (i, f, g, d, e)
    l : m -> type_match_int a b c d e f g h i l k >>= \(n, o, p, q, r) -> type_matches_int a b c q r o p h n m k
  type_type :: (Location_0 -> Location_1) -> Type_0 -> Map' Kind_1 -> Kind_1 -> Err Type_1
  type_type l (Type_0 a c) d e =
    let
      x = Left ("Kind mismatch" ++ location' (l a))
    in case c of
      Application_type_0 f g -> type_type' l f d >>= \(h, i) -> case i of
        Application_kind_1 (Application_kind_1 (Name_kind_1 "Arrow") j) k ->
          if k == e then Application_type_1 h <$> type_type l g d j else x
        _ -> x
      Char_type_0 b -> if e == Name_kind_1 "!Char" then Right (Char_type_1 b) else x
      Int_type_0 b -> if e == Name_kind_1 "!Int" then Right (Int_type_1 b) else x
      Name_type_0 f -> case Data.Map.lookup f d of
        Just g -> if g == e then Right (Name_type_1 f) else x
        Nothing -> undefined_error "type" f (l a)
  type_type' :: (Location_0 -> Location_1) -> Type_0 -> Map' Kind_1 -> Err (Type_1, Kind_1)
  type_type' l (Type_0 a c) d =
    let
      b e = Left (e ++ location' (l a))
    in case c of
      Application_type_0 e f -> type_type' l e d >>= \(g, h) -> case h of
        Application_kind_1 (Application_kind_1 (Name_kind_1 "Arrow") i) j ->
          (\k -> (Application_type_1 g k, j)) <$> type_type l f d i
        _ -> b "Kind mismatch"
      Char_type_0 e -> Right (Char_type_1 e, char_kind)
      Int_type_0 e -> Right (Int_type_1 e, int_kind)
      Name_type_0 e -> case Data.Map.lookup e d of
        Just f -> Right (Name_type_1 e, f)
        Nothing -> undefined_error "type" e (l a)
  type_types :: (Location_0 -> Location_1) -> [Type_0] -> Map' Kind_1 -> Err [Type_1]
  type_types f a b = case a of
    [] -> Right []
    c : d -> type_type f c b star_kind >>= \e -> (:) e <$> type_types f d b
  typevar ::
    (String -> String) -> (String, Kind_1) -> (Map' String, Map' (Kind_1, Status')) -> (Map' String, Map' (Kind_1, Status'))
  typevar e (a, b) =
    let
      d = e a
    in
      bimap (Data.Map.insert a d) (Data.Map.insert d (b, Flexible))
  typevars ::
    (String -> String) ->
    [(String, Kind_1)] ->
    (Map' String, Map' (Kind_1, Status')) ->
    (Map' String, Map' (Kind_1, Status'))
  typevars e a b = case a of
    [] -> b
    c : d -> typevars e d (typevar e c b)
  typing :: (Location_0 -> Location_1) -> Tree_5 -> (File, Defs, Map' Kind_1) -> Err (File, Defs, Map' Kind_1)
  typing k (Tree_5 a c) (File d t u v w, l, m) =
    (
      type_datas k a (d, t, u, v, w, l, m) >>=
      \(e, b, h, g, o, f, n) ->
        (
          (\(i, j) -> (File (rem_old e) (rem_old b) (rem_old h) (rem_old j) (rem_old o), i, n)) <$>
          type_defs k o c (e, b, h) (f, g)))
  undefined_error :: String -> String -> Location_1 -> Err t
  undefined_error a b c = Left ("Undefined " ++ a ++ " " ++ b ++ location' c)
  unsafe_lookup :: Ord t => t -> Map t u -> u
  unsafe_lookup a b = case Data.Map.lookup a b of
    Just c -> c
    Nothing -> undefined
-----------------------------------------------------------------------------------------------------------------------------