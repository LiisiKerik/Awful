--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Eval where
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
  eval :: Dictionary Expr_2 -> Term_4 -> String
  eval a b =
    case eval' a b of
      Just c -> tostr c
      Nothing -> "Crash"
  eval' :: Dictionary Expr_2 -> Term_4 -> Maybe Term_4
  eval' a c =
    case c of
      Application_expression_2 d e ->
        (
          (,) <$> eval' a d <*> eval' a e >>=
          \h ->
            case h of
              (Add_Int_0_expression_2, Int_expression_2 l) -> Just (Add_Int_1_expression_2 l)
              (Add_Int_1_expression_2 k, Int_expression_2 n) -> Just (Int_expression_2 (k + n))
              (Add_Modular_0_expression_2 k, Modular_expression_2 l) -> Just (Add_Modular_1_expression_2 k l)
              (Add_Modular_1_expression_2 l k, Modular_expression_2 n) -> Just (Modular_expression_2 (mod (k + n) l))
              (Compare_Int_0_expression_2, Int_expression_2 k) -> Just (Compare_Int_1_expression_2 k)
              (Compare_Int_1_expression_2 k, Int_expression_2 l) -> Just (Algebraic_expression_2 (show (compare k l)) [])
              (Compare_Modular_0_expression_2, Modular_expression_2 k) -> Just (Compare_Modular_1_expression_2 k)
              (Compare_Modular_1_expression_2 k, Modular_expression_2 l) ->
                Just (Algebraic_expression_2 (show (compare k l)) [])
              (Convert_Int_expression_2, j) -> Just j
              (Convert_Modular_expression_2 k, Int_expression_2 l) -> Just (Modular_expression_2 (mod l k))
              (Div_0_expression_2, Int_expression_2 k) -> Just (Div_1_expression_2 k)
              (Div_1_expression_2 k, Int_expression_2 l) ->
                Just
                  (case l of
                    0 -> nothing_algebraic
                    _ -> wrap_algebraic (Int_expression_2 (div k l)))
              (Field_expression_2 k, Algebraic_expression_2 _ l) -> Just (l !! fromIntegral k)
              (Function_expression_2 k l, j) -> eval_pat j k l >>= eval' a
              (Inverse_Modular_expression_2 k, Modular_expression_2 l) ->
                Just
                  (case div_finite k 1 l of
                    Nothing -> nothing_algebraic
                    Just w -> wrap_algebraic (Modular_expression_2 w))
              (Mod_0_expression_2, Int_expression_2 k) -> Just (Mod_1_expression_2 k)
              (Mod_1_expression_2 k, Int_expression_2 l) ->
                Just
                  (case l of
                    0 -> nothing_algebraic
                    _ -> wrap_algebraic (Int_expression_2 (mod k l)))
              (Multiply_Int_0_expression_2, Int_expression_2 k) -> Just (Multiply_Int_1_expression_2 k)
              (Multiply_Int_1_expression_2 k, Int_expression_2 l) -> Just (Int_expression_2 (k * l))
              (Multiply_Modular_0_expression_2 k, Modular_expression_2 l) -> Just (Multiply_Modular_1_expression_2 k l)
              (Multiply_Modular_1_expression_2 l k, Modular_expression_2 n) -> Just (Modular_expression_2 (mod (k * n) l))
              (Negate_Int_expression_2, Int_expression_2 k) -> Just (Int_expression_2 (- k))
              (Negate_Modular_expression_2 k, Modular_expression_2 l) -> Just (Modular_expression_2 (mod (- l) k))
              _ -> undefined)
      Branch_expression_2 b d e f ->
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
      Match_expression_2 b d -> eval' a b >>= \e -> eval' a (eval_match e d)
      Glob_expression_2 d b e ->
        let
          j k l m = eval' a (repl_expr (Data.Map.fromList (zip k l)) m)
        in
          case b of
            Nothing -> Data.Map.lookup d a >>= \(Expr_2 _ g h) -> j g e h
            Just i ->
              let
                (k, l) = typestring i []
              in
                Data.Map.lookup (d ++ " " ++ k) a >>= \(Expr_2 f g h) ->
                  case f of
                    Nothing -> undefined
                    Just m -> j (m ++ g) (l ++ e) h
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
      (Algebraic_expression_2 d e, Struct_term_pattern_6 f g) ->
        case d == f of
          False -> Nothing
          True -> eval_pats (zip e g) c
      (Int_expression_2 d, Int_term_pattern_6 e) ->
        case d == e of
          False -> Nothing
          True -> Just c
      (Modular_expression_2 d, Modular_term_pattern_6 e) ->
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
  nothing_algebraic :: Term_4
  nothing_algebraic = Algebraic_expression_2 "Nothing" []
  repl_expr :: Dictionary Type_4 -> Term_4 -> Term_4
  repl_expr a b =
    let
      e = repl_expr a
      f = repl3 a
    in
      case b of
        Algebraic_expression_2 c d -> Algebraic_expression_2 c (e <$> d)
        Application_expression_2 c d -> Application_expression_2 (e c) (e d)
        Branch_expression_2 c d g h -> Branch_expression_2 (f c) (e d) g (e h)
        Function_expression_2 c d -> Function_expression_2 c (e d)
        Glob_expression_2 c d g -> Glob_expression_2 c (f <$> d) (f <$> g)
        Match_expression_2 c d -> Match_expression_2 (e c) ((\(Arrow_4 g h) -> Arrow_4 g (e h)) <$> d)
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
            Dictionary [Type_variable_1],
            Dictionary Constructor_3,
            Dictionary Type_5,
            Dictionary Class_6,
            Dictionary Class_5,
            Dictionary (Dictionary [[String]]),
            Dictionary Kind_0),
          (Dictionary Operator_0, Dictionary Operator_0)),
        Dictionary Expr_2) ->
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
              Dictionary [Type_variable_1],
              Dictionary Constructor_3,
              Dictionary Type_5,
              Dictionary Class_6,
              Dictionary Class_5,
              Dictionary (Dictionary [[String]]),
              Dictionary Kind_0),
            (Dictionary Operator_0, Dictionary Operator_0)),
          Dictionary Expr_2))
  standard_naming_typing f a ((b, b0), (c, (u0, u1)), g) =
    (
      standard_file f u0 u1 a b >>=
      \(t0, (t1, t2, n')) ->
        naming_file n' f b0 >>= \(d0, e) -> (\(h, i) -> ((t0, d0), (h, (t1, t2)), i)) <$> typing f e (c, g))
  subst_expr :: String -> Term_4 -> Term_4 -> Term_4
  subst_expr a b c =
    let
      f x = subst_expr a x c
    in
      case b of
        Algebraic_expression_2 d e -> Algebraic_expression_2 d (f <$> e)
        Application_expression_2 d e -> Application_expression_2 (f d) (f e)
        Branch_expression_2 d e g h -> Branch_expression_2 d (f e) g (f h)
        Function_expression_2 d e -> Function_expression_2 d (if subst_help a d then e else f e)
        Loc_expression_2 d -> if d == a then c else b
        Match_expression_2 d e ->
          Match_expression_2 (f d) ((\(Arrow_4 g h) -> Arrow_4 g (if subst_help a g then h else f h)) <$> e)
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
      (Dictionary [Type_variable_1], Dictionary Constructor_3, Dictionary Type_5) ->
      Dictionary Expr_2 ->
      Term_0 ->
      Dictionary (Dictionary [[String]]) ->
      Dictionary Operator_0 ->
      Err String)
  tokenise_parse_naming_typing_eval (c0, f3, c) f (g, h, i) l e u q2 =
    standard_term "input" q2 c0 e >>= \e' -> naming_term "input" f3 c e' >>= \j -> eval l <$> type_expr' (f, g, h, i) j u
  tostr :: Term_4 -> String
  tostr x = show x
  wrap_algebraic :: Term_4 -> Term_4
  wrap_algebraic x = Algebraic_expression_2 "Wrap" [x]
--------------------------------------------------------------------------------------------------------------------------------