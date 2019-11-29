--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Eval (standard_naming_typing, tokenise_parse_naming_typing_eval) where
  import Data.Set
  import Data.Map
  import Dictionary
  import Errors
  import Naming
  import Standard
  import Tree
  import Typing
  div_finite :: Integer -> Integer -> Integer -> Maybe Integer
  div_finite a b c =
    case a of
      1 -> Just 0
      _ ->
        case c of
          0 -> Nothing
          _ -> (\d -> div (a * d + b) c) <$> div_finite c (mod (- b) c) (mod a c)
  eval :: Dictionary Polymorphic_term -> Term_4 -> String
  eval a b =
    case eval' a b of
      Just c -> tostr c
      Nothing -> "Crash"
  eval' :: Dictionary Polymorphic_term -> Term_4 -> Maybe Term_4
  eval' a c =
    case c of
      Add_Int_term_4 (Int_term_4 x) (Int_term_4 y) -> Just (Int_term_4 (x + y))
      Add_Modular_term_4 n (Modular_term_4 x) (Modular_term_4 y) -> Just (Modular_term_4 (mod (x + y) n))
      Application_term_4 d e ->
        (
          (,) <$> eval' a d <*> eval' a e >>=
          \m ->
            case m of
              (Algebraic_term_4 t x, y) -> Just (Algebraic_term_4 t (x ++ [y]))
              (Arrow_term_4 (Arrow_4 k l), j) -> eval_pat j k l >>= eval' a
              (Field_term_4 n, Algebraic_term_4 _ fields) -> Just (fields !! n)
              _ -> undefined)
      Branch_term_4 b d e f ->
        eval'
          a
          (case b of
            Application_type_4 (Name_type_4 "Next") g ->
              repl_expr
                (case e of
                  Blank_type_pattern_3 -> Data.Map.empty
                  Name_type_pattern_3 x -> Data.Map.singleton x g)
                f
            Name_type_4 "Zero" -> d
            _ -> undefined)
      Compare_Int_term_4 (Int_term_4 x) (Int_term_4 y) -> Just (Algebraic_term_4 (show (compare x y)) [])
      Compare_Modular_term_4 (Modular_term_4 x) (Modular_term_4 y) -> Just (Algebraic_term_4 (show (compare x y)) [])
      Convert_Modular_term_4 n (Int_term_4 x) -> Just (Modular_term_4 (mod x n))
      Div_term_4 (Int_term_4 x) (Int_term_4 y) ->
        case y > 0 of
          False -> Nothing
          True -> Just (Int_term_4 (div x y))
      Inverse_Modular_term_4 n (Modular_term_4 x) ->
        Just
          (case div_finite n 1 x of
            Nothing -> Algebraic_term_4 "Nothing" []
            Just y -> Algebraic_term_4 "Wrap" [Modular_term_4 y])
      Match_term_4 b d -> eval' a b >>= \e -> eval' a (eval_match e d)
      Mod_term_4 (Int_term_4 x) (Int_term_4 y) ->
        case y > 0 of
          False -> Nothing
          True -> Just (Int_term_4 (mod x y))
      Glob_term_4 d b e ->
        let
          j k l m = eval' a (repl_expr (Data.Map.fromList (zip k l)) m)
        in
          case b of
            Nothing -> Data.Map.lookup d a >>= \(Parametric_term g h) -> j g e h
            Just i ->
              let
                (k, l) = typestring i []
              in
                let
                  Ad_hoc_term u v = a ! d
                in
                  case v ! k of
                    Add_Modular_term ->
                      Just
                        (Arrow_term_4
                          (Arrow_4
                            (Name_term_pattern_6 "x")
                            (Arrow_term_4
                              (Arrow_4
                                (Name_term_pattern_6 "y")
                                (Add_Modular_term_4 (nat_to_int (head l)) (Loc_term_4 "x") (Loc_term_4 "y"))))))
                    Convert_Modular_term ->
                      Just
                        (Arrow_term_4
                          (Arrow_4 (Name_term_pattern_6 "x") (Convert_Modular_term_4 (nat_to_int (head l)) (Loc_term_4 "x"))))
                    Implementation w x -> j (w ++ u) (l ++ e) x
                    Times_Modular_term ->
                      Just
                        (Arrow_term_4
                          (Arrow_4
                            (Name_term_pattern_6 "x")
                            (Arrow_term_4
                              (Arrow_4
                                (Name_term_pattern_6 "y")
                                (Times_Modular_term_4 (nat_to_int (head l)) (Loc_term_4 "x") (Loc_term_4 "y"))))))
      Times_Int_term_4 (Int_term_4 x) (Int_term_4 y) -> Just (Int_term_4 (x * y))
      Times_Modular_term_4 n (Modular_term_4 x) (Modular_term_4 y) -> Just (Modular_term_4 (mod (x * y) n))
      _ -> Just c
  eval_match :: Term_4 -> [Arrow_4] -> Term_4
  eval_match a b =
    case b of
      [] -> undefined
      Arrow_4 c d : e ->
        case eval_pat a c d of
          Nothing -> eval_match a e
          Just g -> g
  eval_pat :: Term_4 -> Term_pattern_6 -> Term_4 -> Maybe Term_4
  eval_pat a b c =
    case (a, b) of
      (Algebraic_term_4 d e, Struct_term_pattern_6 f g) ->
        case d == f of
          False -> Nothing
          True -> eval_pats (zip e g) c
      (Int_term_4 d, Int_term_pattern_6 e) ->
        case d == e of
          False -> Nothing
          True -> Just c
      (Modular_term_4 d, Modular_term_pattern_6 e) ->
        case d == e of
          False -> Nothing
          True -> Just c
      (_, Blank_term_pattern_6) -> Just c
      (_, Name_term_pattern_6 d) -> Just (subst_expr d c a)
      _ -> Nothing
  eval_pats :: [(Term_4, Term_pattern_6)] -> Term_4 -> Maybe Term_4
  eval_pats a b =
    case a of
      [] -> Just b
      (c, d) : e -> eval_pat c d b >>= eval_pats e
  nat_to_int :: Type_4 -> Integer
  nat_to_int a =
    case a of
      Application_type_4 (Name_type_4 "Next") b -> 1 + nat_to_int b
      Name_type_4 "Zero" -> 0
      _ -> undefined
  repl_expr :: Dictionary Type_4 -> Term_4 -> Term_4
  repl_expr a b =
    let
      e = repl_expr a
      f = repl3 a
    in
      case b of
        Algebraic_term_4 c d -> Algebraic_term_4 c (e <$> d)
        Application_term_4 c d -> Application_term_4 (e c) (e d)
        Branch_term_4 c d g h -> Branch_term_4 (f c) (e d) g (e h)
        Arrow_term_4 (Arrow_4 c d) -> Arrow_term_4 (Arrow_4 c (e d))
        Glob_term_4 c d g -> Glob_term_4 c (f <$> d) (f <$> g)
        Match_term_4 c d -> Match_term_4 (e c) ((\(Arrow_4 g h) -> Arrow_4 g (e h)) <$> d)
        _ -> b
  repl3 :: Dictionary Type_4 -> Type_4 -> Type_4
  repl3 a b =
    case b of
      Application_type_4 c d -> Application_type_4 (repl3 a c) (repl3 a d)
      Name_type_4 c ->
        case Data.Map.lookup c a of
          Nothing -> b
          Just d -> d
  standard_naming_typing ::
    (
      String ->
      File_0 ->
      (
        (
          Set String,
          (
            Dictionary Language_or_location,
            Dictionary Language_or_location,
            Dictionary Language_or_location,
            Dictionary Language_or_location,
            Dictionary Language_or_location,
            Dictionary' Language_or_location)),
        (
          (
            Dictionary Kind_0,
            Dictionary Constructor_3,
            Dictionary Polymorphic_type,
            Dictionary Class_6,
            Dictionary Class_5,
            Dictionary (Dictionary [[String]]),
            Dictionary Kind_0),
          (Dictionary Operator_0, Dictionary Operator_0)),
        Dictionary Polymorphic_term) ->
      Err
        (
          (
            Set String,
            (
              Dictionary Language_or_location,
              Dictionary Language_or_location,
              Dictionary Language_or_location,
              Dictionary Language_or_location,
              Dictionary Language_or_location,
              Dictionary' Language_or_location)),
          (
            (
              Dictionary Kind_0,
              Dictionary Constructor_3,
              Dictionary Polymorphic_type,
              Dictionary Class_6,
              Dictionary Class_5,
              Dictionary (Dictionary [[String]]),
              Dictionary Kind_0),
            (Dictionary Operator_0, Dictionary Operator_0)),
          Dictionary Polymorphic_term))
  standard_naming_typing f a ((b, b0), (c, (u0, u1)), g) =
    (
      standard_file f u0 u1 a b >>=
      \(t0, (t1, t2, n')) ->
        naming_file n' f b0 >>= \(d0, e) -> (\(h, i) -> ((t0, d0), (h, (t1, t2)), i)) <$> typing f (c, g) e)
  subst_expr :: String -> Term_4 -> Term_4 -> Term_4
  subst_expr a b c =
    let
      f x = subst_expr a x c
    in
      case b of
        Algebraic_term_4 d e -> Algebraic_term_4 d (f <$> e)
        Application_term_4 d e -> Application_term_4 (f d) (f e)
        Branch_term_4 d e g h -> Branch_term_4 d (f e) g (f h)
        Arrow_term_4 (Arrow_4 d e) -> Arrow_term_4 (Arrow_4 d (if subst_help a d then e else f e))
        Loc_term_4 d -> if d == a then c else b
        Match_term_4 d e ->
          Match_term_4 (f d) ((\(Arrow_4 g h) -> Arrow_4 g (if subst_help a g then h else f h)) <$> e)
        _ -> b
  subst_help :: String -> Term_pattern_6 -> Bool
  subst_help a b =
    case b of
      Name_term_pattern_6 c -> c == a
      Struct_term_pattern_6 _ c -> any (subst_help a) c
      _ -> False
  tokenise_parse_naming_typing_eval ::
    (
      (Set String, Dictionary Language_or_location, Dictionary Language_or_location) ->
      Dictionary Kind_0 ->
      (Dictionary Constructor_3, Dictionary Polymorphic_type) ->
      Dictionary Polymorphic_term ->
      Term_0 ->
      Dictionary (Dictionary [[String]]) ->
      Dictionary Operator_0 ->
      Err String)
  tokenise_parse_naming_typing_eval (c0, f3, c) f (h, i) l e u q2 =
    standard_term "input" q2 c0 e >>= \e' -> naming_term "input" f3 c e' >>= \j -> eval l <$> type_expr' (f, h, i) j u
  tostr :: Term_4 -> String
  tostr x = show x
--------------------------------------------------------------------------------------------------------------------------------