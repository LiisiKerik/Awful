-----------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Eval where
  import Data.Map
  import Naming
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
  eval :: Map' Expression_2 -> Expression_2 -> String
  eval a b =
    case eval' a b of
      Just c -> tostr c
      Nothing -> "Crash"
  eval' :: Map' Expression_2 -> Expression_2 -> Maybe Expression_2
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
                  Brackets_Modular_expression_2 k ->
                    case j of
                      Modular_expression_2 l -> Just (list_expression ("(" ++ show_mod (Modular k l) ++ ")"))
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
                  Convert_Int_expression_2 -> Just j
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
                      Struct_expression_2 l -> Just (unsafe_lookup k l)
                      _ -> undefined
                  Function_expression_2 k l ->
                    eval'
                      a
                      (case k of
                        Blank_pattern -> l
                        Name_pattern n -> subst_expr n l j)
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
                  Negate_Int_expression_2 ->
                    case j of
                      Int_expression_2 k -> Just (Int_expression_2 (- k))
                      _ -> undefined
                  Write_Int_expression_2 ->
                    case j of
                      Int_expression_2 k -> Just (list_expression (show k))
                      _ -> undefined
                  Write_Modular_expression_2 k ->
                    case j of
                      Modular_expression_2 l -> Just (list_expression (show_mod (Modular k l)))
                      _ -> undefined
                  _ -> undefined))
      Match_expression_2 d e ->
        (
          eval' a d >>=
          \h ->
            eval'
              a
              (case e of
                Matches_Algebraic_2 i j ->
                  case h of
                    Algebraic_expression_2 k l ->
                      case Data.Map.lookup k i of
                        Just (Match_Algebraic_2 o p) -> eval_match o l p
                        Nothing ->
                          case j of
                            Just o -> o
                            Nothing -> undefined
                    _ -> undefined
                Matches_char_2 i j ->
                  case h of
                    Char_expression_2 k ->
                      case Data.Map.lookup k i of
                        Just o -> o
                        Nothing -> j
                    _ -> undefined
                Matches_Int_2 i j ->
                  case h of
                    Int_expression_2 k ->
                      case Data.Map.lookup k i of
                        Just o -> o
                        Nothing -> j
                    _ -> undefined
                Matches_Modular_2 i j ->
                  case h of
                    Modular_expression_2 k ->
                      case Data.Map.lookup k i of
                        Just o -> o
                        Nothing ->
                          case j of
                            Just o -> o
                            Nothing -> undefined
                    _ -> undefined))
      Name_expression_2 d -> Data.Map.lookup d a >>= eval' a
      _ -> Just c
  eval_match :: [Pattern_0] -> [Expression_2] -> Expression_2 -> Expression_2
  eval_match a b c =
    case a of
      [] ->
        case b of
          [] -> c
          _ -> undefined
      d : e ->
        case b of
          [] -> undefined
          f : g ->
            eval_match
              e
              g
              (case d of
                Blank_pattern -> c
                Name_pattern h -> subst_expr h c f)
  list_expression :: String -> Expression_2
  list_expression =
    Prelude.foldr
      (\x -> \y -> Algebraic_expression_2 "Construct_List" [Char_expression_2 x, y])
      (Algebraic_expression_2 "Empty_List" [])
  nothing_algebraic :: Expression_2
  nothing_algebraic = Algebraic_expression_2 "Nothing" []
  subst_algebraic :: String -> Expression_2 -> Match_Algebraic_2 -> Match_Algebraic_2
  subst_algebraic a b (Match_Algebraic_2 c d) = Match_Algebraic_2 c (if subst_help c a then d else subst_expr a d b)
  subst_expr :: String -> Expression_2 -> Expression_2 -> Expression_2
  subst_expr a b c =
    let
      f x = subst_expr a x c
    in
      case b of
        Algebraic_expression_2 d e -> Algebraic_expression_2 d (f <$> e)
        Application_expression_2 d e -> Application_expression_2 (f d) (f e)
        Function_expression_2 d e ->
          case d of
            Blank_pattern -> b
            Name_pattern g -> if g == a then b else Function_expression_2 d (f e)
        Match_expression_2 d e ->
          Match_expression_2
            (f d)
            (case e of
              Matches_Algebraic_2 g h -> Matches_Algebraic_2 (subst_algebraic a c <$> g) (f <$> h)
              Matches_char_2 g h -> Matches_char_2 (f <$> g) (f h)
              Matches_Int_2 g h -> Matches_Int_2 (f <$> g) (f h)
              Matches_Modular_2 g h -> Matches_Modular_2 (f <$> g) (f <$> h))
        Name_expression_2 d -> if d == a then c else b
        Struct_expression_2 d -> Struct_expression_2 (f <$> d)
        _ -> b
  subst_help :: [Pattern_0] -> String -> Bool
  subst_help a b =
    case a of 
      [] -> False
      c : d ->
        let
          f = subst_help d b
        in
          case c of
            Blank_pattern -> f
            Name_pattern e -> e == b || f
  tokenise_parse_naming_typing_eval ::
    Locations ->
    Map' Polykind ->
    (Map' Alg, Map' String, Map' Type_2) ->
    Map' Expression_2 ->
    String ->
    Map' (Map' [[String]]) ->
    Map' ([String], Map' [(String, Nat)]) ->
    Err String
  tokenise_parse_naming_typing_eval c f (g, h, i) l b u v =
    parse_expression b >>= \e -> naming_expression "input" e c >>= \j -> eval l <$> type_expr' (f, g, h, i) j u v
  tostr :: Expression_2 -> String
  tostr x =
    case x of
      Algebraic_expression_2 "Empty_List" [] -> []
      Algebraic_expression_2 "Construct_List" [Char_expression_2 y, z] -> y : tostr z
      _ -> undefined
  wrap_algebraic :: Expression_2 -> Expression_2
  wrap_algebraic x = Algebraic_expression_2 "Wrap" [x]
-----------------------------------------------------------------------------------------------------------------------------