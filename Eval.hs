-----------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Eval where
  import Data.Map
  import Naming
  import Tokenise
  import Tree
  import Typing
{-
  div_finite :: Integer -> Integer -> Integer -> Maybe Integer
  div_finite a b c = case a of
    1 -> Just 0
    _ -> case c of
      0 -> Nothing
      _ -> (\d -> div (a * d + b) c) <$> div_finite c (mod (- b) c) (mod a c)
-}
  eval :: Defs -> Expression_2 -> Expression_2
  eval a b = case eval' a b of
    Just c -> c
    Nothing -> Name_expression_2 "Crash."
  eval' :: Defs -> Expression_2 -> Maybe Expression_2
  eval' a c = case c of
    Application_expression_2 d e -> eval' a d >>= \h -> eval' a e >>= \j -> case h of
      Add_Int_expression_2 -> case j of
        Int_expression_2 l -> Just (Add_Int'_expression_2 l)
        _ -> ice
      Add_Int'_expression_2 k -> case j of
        Int_expression_2 n -> Just (Int_expression_2 (k + n))
        _ -> ice
      Compare_Int_expression_2 -> case j of
        Int_expression_2 k -> Just (Compare_Int'_expression_2 k)
        _ -> ice
      Compare_Int'_expression_2 k -> case j of
        Int_expression_2 l -> Just (Algebraic_expression_2 (show (compare k l)) [])
        _ -> ice
      Field_expression_2 k -> case j of
        Struct_expression_2 l -> Just (unsafe_lookup k l)
        _ -> ice
      Function_expression_2 k l -> eval' a (case k of
        Blank_pattern -> l
        Name_pattern n -> subst_expr n l j)
      Mod_Int_expression_2 -> case j of
        Int_expression_2 k -> Just (Mod_Int'_expression_2 k)
        _ -> ice
      Mod_Int'_expression_2 k -> case j of
        Int_expression_2 l -> Just (Int_expression_2 (mod k (abs l)))
        _ -> ice
      Multiply_Int_expression_2 -> case j of
        Int_expression_2 k -> Just (Multiply_Int'_expression_2 k)
        _ -> ice
      Multiply_Int'_expression_2 k -> case j of
        Int_expression_2 l -> Just (Int_expression_2 (k * l))
        _ -> ice
      Negate_Int_expression_2 -> case j of
        Int_expression_2 k -> Just (Int_expression_2 (- k))
        _ -> ice
      _ -> ice
    Match_expression_2 d e -> eval' a d >>= \h -> eval' a (case e of
      Matches_Algebraic_2 i j -> case h of
        Algebraic_expression_2 k l -> case Data.Map.lookup k i of
          Just (Match_Algebraic_2 o p) -> eval_match o l p
          Nothing -> case j of
            Just o -> o
            Nothing -> ice
        _ -> ice
      Matches_Int_2 i j -> case h of
        Int_expression_2 k -> case Data.Map.lookup k i of
          Just o -> o
          Nothing -> j
        _ -> ice)
    Name_expression_2 d -> eval' a (unsafe_lookup d a)
    _ -> Just c
  eval_match :: [Pattern_0] -> [Expression_2] -> Expression_2 -> Expression_2
  eval_match a b c = case a of
    [] -> case b of
      [] -> c
      _ -> ice
    d : e -> case b of
      [] -> ice
      f : g -> eval_match e g (case d of
        Blank_pattern -> c
        Name_pattern h -> subst_expr h c f)
  nothing_algebraic :: Expression_2
  nothing_algebraic = Algebraic_expression_2 "Nothing" []
  subst_algebraic :: String -> Expression_2 -> Match_Algebraic_2 -> Match_Algebraic_2
  subst_algebraic a b (Match_Algebraic_2 c d) = Match_Algebraic_2 c (if subst_help c a then d else subst_expr a d b)
  subst_expr :: String -> Expression_2 -> Expression_2 -> Expression_2
  subst_expr a b c =
    let
      f x = subst_expr a x c
    in case b of
      Algebraic_expression_2 d e -> Algebraic_expression_2 d (f <$> e)
      Application_expression_2 d e -> Application_expression_2 (f d) (f e)
      Function_expression_2 d e -> case d of
        Blank_pattern -> b
        Name_pattern g -> if g == a then b else Function_expression_2 d (f e)
      Match_expression_2 d e -> Match_expression_2 (f d) (case e of
        Matches_Algebraic_2 g h -> Matches_Algebraic_2 (subst_algebraic a c <$> g) (f <$> h)
        Matches_Int_2 g h -> Matches_Int_2 (f <$> g) (f h))
      Name_expression_2 d -> if d == a then c else b
      Struct_expression_2 d -> Struct_expression_2 (f <$> d)
      _ -> b
  subst_help :: [Pattern_0] -> String -> Bool
  subst_help a b = case a of 
    [] -> False
    c : d ->
      let
        f = subst_help d b
      in case c of
        Blank_pattern -> f
        Name_pattern e -> e == b || f
  tokenise_parse_naming_typing_eval :: Locations -> Map' Kind -> (Algebraics, Constrs, Types) -> Defs -> String -> Err String
  tokenise_parse_naming_typing_eval c f (g, h, i) l b =
    (
      parse_expression b >>=
      \e -> naming_expression "input" e c >>= \j -> show <$> eval l <$> type_expr' (Location_1 "input") (f, g, h, i) j)
-----------------------------------------------------------------------------------------------------------------------------