--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Eval where
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
                  Div'_expression_2 k ->
                    case j of
                      Int_expression_2 l -> Just (Int_expression_2 (div l k))
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
                        Just (pair_expression (list_expression (show k)) (Algebraic_expression_2 "False" []))
                      _ -> undefined
                  Write_Brackets_Modular_expression_2 k ->
                    case j of
                      Modular_expression_2 l ->
                        Just (pair_expression (list_expression (show k ++ " # " ++ show l)) (Algebraic_expression_2 "True" []))
                      _ -> undefined
                  _ -> undefined))
      Branch_expression_2 b d e f ->
        eval'
          a
          (case b of
            Application_type_1 (Name_type_1 "Next") g -> repl_expr (Data.Map.singleton e g) f
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
      f = repl' a
    in
      case b of
        Algebraic_expression_2 c d -> Algebraic_expression_2 c (e <$> d)
        Application_expression_2 c d -> Application_expression_2 (e c) (e d)
        Branch_expression_2 c d g h -> Branch_expression_2 (f c) (e d) g (e h)
        Function_expression_2 c d -> Function_expression_2 c (e d)
        Glob_expression_2 c d g -> Glob_expression_2 c (f <$> d) (f <$> g)
        Match_expression_2 c d -> Match_expression_2 (e c) ((\(Case_3 g h) -> Case_3 g (e h)) <$> d)
        _ -> b
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
    ((Set String, Set String), Locations) ->
    Map' Kind_0 ->
    (Map' Alg, Map' Constructor, Map' Type_2) ->
    Map' Expr_2 ->
    Expression_0 ->
    Map' (Map' [[String]]) ->
    Map' Op ->
    Err String
  tokenise_parse_naming_typing_eval c f (g, h, i) l e u q =
    (
      std_expr (Location_1 "input") q e >>=
      \e' -> naming_expression "input" e' c >>= \j -> eval l <$> type_expr' (f, g, h, i) j u)
  tostr :: Expression_2 -> String
  tostr x =
    case x of
      Algebraic_expression_2 "Empty_List" [] -> ""
      Algebraic_expression_2 "Construct_List" [Char_expression_2 y, z] -> y : tostr z
      _ -> undefined
  wrap_algebraic :: Expression_2 -> Expression_2
  wrap_algebraic x = Algebraic_expression_2 "Wrap" [x]
--------------------------------------------------------------------------------------------------------------------------------