module Awful.Eval (tokenise_parse_naming_typing_eval) where
-- todo: built-in prettyprinting to replace user-specified prettyprinting
  import Awful.Namechecker
  import Awful.Operators
  import Awful.Parser
  import Awful.Tokeniser
  import Awful.Typechecker
  import Control.Applicative
  import Data.List
  import Data.Set
  import Data.Map as Map
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
      Nothing -> "Evaluation error."
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
                  Add_Modular_0_expression_2 k ->
                    case j of
                      Modular_expression_2 l -> Just (Add_Modular_1_expression_2 k l)
                      _ -> undefined
                  Add_Modular_1_expression_2 l k ->
                    case j of
                      Modular_expression_2 n -> Just (Modular_expression_2 (mod (k + n) l))
                      _ -> undefined
                  Compare_Int_0_expression_2 ->
                    case j of
                      Int_expression_2 k -> Just (Compare_Int_1_expression_2 k)
                      _ -> undefined
                  Compare_Int_1_expression_2 k ->
                    case j of
                      Int_expression_2 l -> Just (Struct_expression_2 (show (compare k l)) [])
                      _ -> undefined
                  Compare_Modular_0_expression_2 ->
                    case j of
                      Modular_expression_2 k -> Just (Compare_Modular_1_expression_2 k)
                      _ -> undefined
                  Compare_Modular_1_expression_2 k ->
                    case j of
                      Modular_expression_2 l -> Just (Struct_expression_2 (show (compare k l)) [])
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
                  Field_expression_2 cname k ->
                    case j of
                      Struct_expression_2 cname' l | cname' == cname -> Just (l !! k)
                      _ -> Nothing
                  Function_expression_2 k l -> subst_new_pat' k j l >>= eval' a
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
                        case l of
                          0 -> Nothing
                          _ -> Just (Int_expression_2 (mod k l))
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
                  _ -> undefined))
      Match_expression_2 d e ->
        do
          h <- eval' a d
          i <- find_match h e
          eval' a i
      Name_expression_2 d -> Map.lookup d a >>= eval' a
      _ -> Just c
  find_match :: Expression_2 -> [Match_2] -> Maybe Expression_2
  find_match a matches =
    case matches of
      [] -> Nothing
      Match_2 b c : matches' -> subst_new_pat' b a c <|> find_match a matches'
  nothing_algebraic :: Expression_2
  nothing_algebraic = Struct_expression_2 "Nothing" []
  subst_expr :: String -> Expression_2 -> Expression_2 -> Expression_2
  subst_expr a b c =
    let
      f x = subst_expr a x c
    in
      case b of
        Application_expression_2 d e -> Application_expression_2 (f d) (f e)
        Function_expression_2 d e -> Function_expression_2 d (if subst_new_pat a d then e else f e)
        Match_expression_2 d e -> Match_expression_2 (f d) (subst_match a c <$> e)
        Name_expression_2 d -> if d == a then c else b
        Struct_expression_2 s5 d -> Struct_expression_2 s5 (f <$> d)
        _ -> b
  subst_match :: String -> Expression_2 -> Match_2 -> Match_2
  subst_match a b (Match_2 c d) = Match_2 c (if subst_new_pat a c then d else subst_expr a d b)
  subst_new_pat :: String -> New_pat_1 -> Bool
  subst_new_pat s pat =
    case pat of
      New_application_pat_1 _ pats -> or (subst_new_pat s <$> pats)
      New_blank_pat_1 -> False
      New_int_pat_1 _ -> False
      New_modular_pat_1 _ -> False
      New_name_pat_1 n -> n == s
  subst_new_pat' :: New_pat_1 -> Expression_2 -> Expression_2 -> Maybe Expression_2
  subst_new_pat' a b c =
    case (a, b) of
      (New_application_pat_1 d e, Struct_expression_2 f g) | d == f -> subst_pats_new e g c
      (New_blank_pat_1, _) -> Just c
      (New_int_pat_1 d, Int_expression_2 e) | d == e -> Just c
      (New_modular_pat_1 (Modular' _ d), Modular_expression_2 e) | d == e -> Just c
      (New_name_pat_1 d, _) -> Just (subst_expr d c b)
      _ -> Nothing
  subst_pats_new :: [New_pat_1] -> [Expression_2] -> Expression_2 -> Maybe Expression_2
  subst_pats_new a b c =
    case (a, b) of
      ([], []) -> Just c
      (d : e, f : g) -> subst_new_pat' d f c >>= subst_pats_new e g
      _ -> undefined
  tokenise_parse_naming_typing_eval ::
    (Set String, Locations) ->
    Map' Kind ->
    (Map' Unnamed_alg, Map' String, Map' Type_2) ->
    Map' Expression_2 ->
    String ->
    Map' (Map' [[String]]) ->
    Map' ([String], Map' [(String, Nat)]) ->
    Map' Strct ->
    Map' Op ->
    Map' Anyconstr ->
    Err String
  tokenise_parse_naming_typing_eval c f (g, h, i) l b u v a q anyconstrs =
    (
      parse_expression b >>=
      \e ->
        (
          std_expr (Location_1 "input") q e >>=
          \e' -> naming_expression "input" e' c >>= \j -> eval l <$> type_expr' (f, g, h, i) j u v a anyconstrs))
  tostr :: Expression_2 -> String
  tostr expr =
    case expr of
      Int_expression_2 i -> show i
      Modular_expression_2 i -> show i
      Struct_expression_2 name exprs -> intercalate " " (name : (tostr_brackets <$> exprs))
      _ -> "The result is an expression that can't be displayed."
  tostr_brackets :: Expression_2 -> String
  tostr_brackets expr = "(" <> tostr expr <> ")"
  wrap_algebraic :: Expression_2 -> Expression_2
  wrap_algebraic x = Struct_expression_2 "Wrap" [x]