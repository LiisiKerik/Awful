--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Eval where
  import Data.Foldable
  import Data.Set
  import Data.Map
  import Naming
  import Standard
  import Tokenise
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
  do_check :: (String, Map' Expr_2) -> Chck' -> Err ()
  do_check (f, y) (Chck' (Name l a) (b, c) d) =
    (
      traverse
        (simple'
          (
            (
              "Constructor " ++
              a ++
              location f l ++
              " is restricted and should be applied only to primitives and fully applied constructors."),
            y))
        d >>=
      \e ->
        case length e == length b of
          False -> Left ("Constructor " ++ a ++ location f l ++ " is restricted and should be fully applied.")
          True ->
            case eval' y (Prelude.foldl (\m -> \(n, o) -> subst_expr n m o) c (zip b e)) of
              Nothing -> Left ("Static argument check for constructor " ++ a ++ location f l ++ " resulted in Crash.")
              Just k ->
                case k of
                  Algebraic_expression_2 "False" [] ->
                    Left ("Failed static argument check for constructor " ++ a ++ location' f l)
                  Algebraic_expression_2 "True" [] -> Right ()
                  _ -> undefined)
  do_checks :: (String, Map' Expr_2) -> [Chck'] -> Err ()
  do_checks f a = traverse_ (do_check f) a
  eval :: Map' Expr_2 -> Expression_2 -> String
  eval a b =
    case eval' a b of
      Just c -> tostr c
      Nothing -> "Crash"
  eval' :: Map' Expr_2 -> Expression_2 -> Maybe Expression_2
  eval' a c =
    case c of
      Application_expression_2 d e ->
        (
          eval' a d >>=
          \h ->
            (
              eval' a e >>=
              \j ->
                case h of
                  Add_Int_0_expression_2 ->
                    case j of
                      Int_expression_2 l -> Just (Add_Int_1_expression_2 l)
                      _ -> undefined
                  Add_Int_1_expression_2 k ->
                    case j of
                      Int_expression_2 n -> Just (Int_expression_2 (k + n))
                      _ -> undefined
                  Add_Modular_0_expression_2 k ->
                    case j of
                      Modular_expression_2 l -> Just (Add_Modular_1_expression_2 k l)
                      _ -> undefined
                  Add_Modular_1_expression_2 l k ->
                    case j of
                      Modular_expression_2 n -> Just (Modular_expression_2 (mod (k + n) l))
                      _ -> undefined
                  Compare_Char_0_expression_2 ->
                    case j of
                      Char_expression_2 k -> Just (Compare_Char_1_expression_2 k)
                      _ -> undefined
                  Compare_Char_1_expression_2 k ->
                    case j of
                      Char_expression_2 l -> Just (Algebraic_expression_2 (show (compare k l)) [])
                      _ -> undefined
                  Compare_Int_0_expression_2 ->
                    case j of
                      Int_expression_2 k -> Just (Compare_Int_1_expression_2 k)
                      _ -> undefined
                  Compare_Int_1_expression_2 k ->
                    case j of
                      Int_expression_2 l -> Just (Algebraic_expression_2 (show (compare k l)) [])
                      _ -> undefined
                  Compare_Modular_0_expression_2 ->
                    case j of
                      Modular_expression_2 k -> Just (Compare_Modular_1_expression_2 k)
                      _ -> undefined
                  Compare_Modular_1_expression_2 k ->
                    case j of
                      Modular_expression_2 l -> Just (Algebraic_expression_2 (show (compare k l)) [])
                      _ -> undefined
                  Convert_Int_expression_2 -> Just j
                  Convert_Modular_expression_2 k ->
                    case j of
                      Int_expression_2 l -> Just (Modular_expression_2 (mod l k))
                      _ -> undefined
                  Div_0_expression_2 ->
                    case j of
                      Int_expression_2 k -> Just (Div_1_expression_2 k)
                      _ -> undefined
                  Div_1_expression_2 k ->
                    case j of
                      Int_expression_2 l ->
                        Just
                          (case l of
                            0 -> nothing_algebraic
                            _ -> wrap_algebraic (Int_expression_2 (div k l)))
                      _ -> undefined
                  Field_expression_2 k ->
                    case j of
                      Algebraic_expression_2 _ l -> Just (l !! fromIntegral k)
                      _ -> undefined
                  Function_expression_2 k l -> eval' a (subst_pat' k j l)
                  Inverse_Modular_expression_2 k ->
                    case j of
                      Modular_expression_2 l ->
                        Just
                          (case div_finite k 1 l of
                            Nothing -> nothing_algebraic
                            Just w -> wrap_algebraic (Modular_expression_2 w))
                      _ -> undefined
                  Mod_0_expression_2 ->
                    case j of
                      Int_expression_2 k -> Just (Mod_1_expression_2 k)
                      _ -> undefined
                  Mod_1_expression_2 k ->
                    case j of
                      Int_expression_2 l ->
                        Just
                          (case l of
                            0 -> nothing_algebraic
                            _ -> wrap_algebraic (Int_expression_2 (mod k l)))
                      _ -> undefined
                  Multiply_Int_0_expression_2 ->
                    case j of
                      Int_expression_2 k -> Just (Multiply_Int_1_expression_2 k)
                      _ -> undefined
                  Multiply_Int_1_expression_2 k ->
                    case j of
                      Int_expression_2 l -> Just (Int_expression_2 (k * l))
                      _ -> undefined
                  Multiply_Modular_0_expression_2 k ->
                    case j of
                      Modular_expression_2 l -> Just (Multiply_Modular_1_expression_2 k l)
                      _ -> undefined
                  Multiply_Modular_1_expression_2 l k ->
                    case j of
                      Modular_expression_2 n -> Just (Modular_expression_2 (mod (k * n) l))
                      _ -> undefined
                  Negate_Int_expression_2 ->
                    case j of
                      Int_expression_2 k -> Just (Int_expression_2 (- k))
                      _ -> undefined
                  Negate_Modular_expression_2 k ->
                    case j of
                      Modular_expression_2 l -> Just (Modular_expression_2 (mod (- l) k))
                      _ -> undefined
                  Write_Brackets_Int_expression_2 ->
                    case j of
                      Int_expression_2 k ->
                        Just (pair_expression (list_expression (show k)) (Int_expression_2 0))
                      _ -> undefined
                  Write_Brackets_Modular_expression_2 k ->
                    case j of
                      Modular_expression_2 l ->
                        Just (pair_expression (list_expression (show k ++ " # " ++ show l)) (Int_expression_2 1))
                      _ -> undefined
                  _ -> undefined))
      Branch_expression_2 b d e f ->
        eval'
          a
          (case b of
            Application_type_1 (Name_type_1 "Next") g ->
              repl_expr
                (case e of
                  Blank_pattern -> Data.Map.empty
                  Name_pattern x -> Data.Map.singleton x g)
                f
            Name_type_1 "Zero" -> d
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
  eval_match :: Expression_2 -> [Case_3] -> Expression_2
  eval_match a b =
    case b of
      [] -> undefined
      Case_3 c d : e ->
        case eval_pat a c d of
          Nothing -> eval_match a e
          Just g -> g
  eval_pat :: Expression_2 -> Alg_pat_2 -> Expression_2 -> Maybe Expression_2
  eval_pat a b c =
    case (a, b) of
      (Algebraic_expression_2 d e, Application_alg_pat_2 f g) ->
        case d == f of
          False -> Nothing
          True -> eval_pats (zip e g) c
      (Char_expression_2 d, Char_alg_pat_2 e) ->
        case d == e of
          False -> Nothing
          True -> Just c
      (Int_expression_2 d, Int_alg_pat_2 e) ->
        case d == e of
          False -> Nothing
          True -> Just c
      (Modular_expression_2 d, Modular_alg_pat_2 e) ->
        case d == e of
          False -> Nothing
          True -> Just c
      (_, Blank_alg_pat_2) -> Just c
      (_, Name_alg_pat_2 d) -> Just (subst_expr d c a)
      _ -> Nothing
  eval_pats :: [(Expression_2, Alg_pat_2)] -> Expression_2 -> Maybe Expression_2
  eval_pats a b =
    case a of
      [] -> Just b
      (c, d) : e -> eval_pat c d b >>= eval_pats e
  list_expression :: String -> Expression_2
  list_expression =
    Prelude.foldr
      (\x -> \y -> Algebraic_expression_2 "Construct_List" [Char_expression_2 x, y])
      (Algebraic_expression_2 "Empty_List" [])
  nothing_algebraic :: Expression_2
  nothing_algebraic = Algebraic_expression_2 "Nothing" []
  pair_expression :: Expression_2 -> Expression_2 -> Expression_2
  pair_expression x y = Algebraic_expression_2 "Pair" [x, y]
  repl_expr :: Map' Type_1 -> Expression_2 -> Expression_2
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
        Match_expression_2 c d -> Match_expression_2 (e c) ((\(Case_3 g h) -> Case_3 g (e h)) <$> d)
        _ -> b
  repl3 :: Map' Type_1 -> Type_1 -> Type_1
  repl3 a b =
    case b of
      Application_type_1 c d -> Application_type_1 (repl3 a c) (repl3 a d)
      Name_type_1 c ->
        case Data.Map.lookup c a of
          Nothing -> b
          Just d -> d
  simple :: (String, Map' Expr_2) -> Expression_2 -> Err Expression_2
  simple (x, z) a =
    case a of
      Application_expression_2 b c ->
        (
          simple (x, z) b >>=
          \d ->
            (
              (\f ->
                case d of
                  Function_expression_2 (Name_pat_1 h) i -> subst_expr h i f
                  _ -> undefined) <$>
              simple' (x, z) c))
      Branch_expression_2 _ _ _ _ -> Left x
      Function_expression_2 _ _ -> Left x
      Glob_expression_2 b _ _ ->
        let
          Expr_2 _ _ c = z ! b
        in
          case simple2 c of
            False -> Left x
            True -> Right c
      Loc_expression_2 _ -> Left x
      Match_expression_2 _ _ -> Left x
      _ -> Right a
  simple' :: (String, Map' Expr_2) -> Expression_2 -> Err Expression_2
  simple' (e, x) a =
    (
      simple (e, x) a >>=
      \b ->
        case b of
          Function_expression_2 _ _ -> Left e
          _ -> Right b)
  simple2 :: Expression_2 -> Bool
  simple2 a =
    case a of
      Function_expression_2 _ b -> simple2 b
      Algebraic_expression_2 _ _ -> True
      _ -> False
  standard_naming_typing ::
    (
      String ->
      Tree_0 ->
      (
        (Set String, (Locations, Locations, Locations), (Locations, Locations), Map' (Map' Location')),
        (File, Map' ([String], Syntax_type'), (Map' Op, Map' Op)),
        Map' Expr_2,
        (Locations, Map' Syntax_3)) ->
      Err
        (
          (Set String, (Locations, Locations, Locations), (Locations, Locations), Map' (Map' Location')),
          (File, Map' ([String], Syntax_type'), (Map' Op, Map' Op)),
          Map' Expr_2,
          (Locations, Map' Syntax_3)))
  standard_naming_typing f a ((b, b2, b3, b4), (c, t, u), g, (g0, g')) =
    (
      standard_1 f (g0, t, g', u, b) a >>=
      \((v0, v1, v2, v3, t2), n') ->
        (
          naming f n' (b2, b3, b4) >>=
          \((d0, d1, d2), e) ->
            typing f e (c, g) >>= \(h, i, j) -> ((t2, d0, d1, d2), (h, v1, v3), i, (v0, v2)) <$ do_checks (f, i) j))
  subst_expr :: String -> Expression_2 -> Expression_2 -> Expression_2
  subst_expr a b c =
    let
      f x = subst_expr a x c
    in
      case b of
        Algebraic_expression_2 d e -> Algebraic_expression_2 d (f <$> e)
        Application_expression_2 d e -> Application_expression_2 (f d) (f e)
        Branch_expression_2 d e g h -> Branch_expression_2 d (f e) g (f h)
        Function_expression_2 d e -> Function_expression_2 d (if subst_pat a d then e else f e)
        Loc_expression_2 d -> if d == a then c else b
        Match_expression_2 d e ->
          Match_expression_2 (f d) ((\(Case_3 g h) -> Case_3 g (if subst_help a g then h else f h)) <$> e)
        _ -> b
  subst_help :: String -> Alg_pat_2 -> Bool
  subst_help a b =
    case b of
      Application_alg_pat_2 _ c -> or (subst_help a <$> c)
      Name_alg_pat_2 c -> c == a
      _ -> False
  subst_pat :: String -> Pat_1 -> Bool
  subst_pat a b =
    case b of
      Application_pat_1 c -> or (subst_pat a <$> c)
      Blank_pat_1 -> False
      Name_pat_1 c -> c == a
  subst_pat' :: Pat_1 -> Expression_2 -> Expression_2 -> Expression_2
  subst_pat' a b c =
    case a of
      Application_pat_1 d ->
        case b of
          Algebraic_expression_2 _ e -> subst_pats (zip d e) c
          _ -> undefined
      Blank_pat_1 -> c
      Name_pat_1 d -> subst_expr d c b
  subst_pats :: [(Pat_1, Expression_2)] -> Expression_2 -> Expression_2
  subst_pats a b =
    case a of
      [] -> b
      (c, d) : e -> subst_pats e (subst_pat' c d b)
  tokenise_parse_naming_typing_eval ::
    (
      (Set String, Locations) ->
      Map' Kind_0 ->
      (Map' Alg, Map' Constructor, Map' Type_2) ->
      Map' Expr_2 ->
      Expression_0 ->
      Map' (Map' [[String]]) ->
      (Map' ([String], Syntax_type'), Map' Syntax_3, Map' Op) ->
      Err String)
  tokenise_parse_naming_typing_eval (c0, c) f (g, h, i) l e u (q0, q1, q2) =
    (
      std_expr Nothing (c0, q0, q1, q2) e >>=
      \e' ->
        (
          naming_expression "input" e' c >>=
          \j -> type_expr' (f, g, h, i) j u >>= \(x, y) -> eval l x <$ do_checks ("input", l) y))
  tostr :: Expression_2 -> String
  tostr x =
    case x of
      Algebraic_expression_2 "Empty_List" [] -> ""
      Algebraic_expression_2 "Construct_List" [Char_expression_2 y, z] -> y : tostr z
      _ -> undefined
  wrap_algebraic :: Expression_2 -> Expression_2
  wrap_algebraic x = Algebraic_expression_2 "Wrap" [x]
--------------------------------------------------------------------------------------------------------------------------------