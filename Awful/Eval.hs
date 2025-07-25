module Awful.Eval (tokenise_parse_naming_typing_eval) where
-- todo: built-in prettyprinting to replace user-specified prettyprinting
  import Awful.Namechecker
  import Awful.Operators
  import Awful.Parser
  import Awful.Tokeniser
  import Awful.Typechecker
  import Data.List
  import Data.Set
  import Data.Map
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
                      Struct_expression_2 _ l -> Just (unsafe_lookup k l)
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
  eval_match :: [Pat_1] -> [Expression_2] -> Expression_2 -> Expression_2
  eval_match a b c =
    case a of
      [] ->
        case b of
          [] -> c
          _ -> undefined
      d : e ->
        case b of
          [] -> undefined
          f : g -> eval_match e g (subst_pat' d f c)
  nothing_algebraic :: Expression_2
  nothing_algebraic = Algebraic_expression_2 "Nothing" []
  subst_algebraic :: String -> Expression_2 -> Match_Algebraic_2 -> Match_Algebraic_2
  subst_algebraic a b (Match_Algebraic_2 c d) = Match_Algebraic_2 c (if subst_help a c then d else subst_expr a d b)
  subst_expr :: String -> Expression_2 -> Expression_2 -> Expression_2
  subst_expr a b c =
    let
      f x = subst_expr a x c
    in
      case b of
        Algebraic_expression_2 d e -> Algebraic_expression_2 d (f <$> e)
        Application_expression_2 d e -> Application_expression_2 (f d) (f e)
        Function_expression_2 d e -> Function_expression_2 d (if subst_pat a d then e else f e)
        Match_expression_2 d e ->
          Match_expression_2
            (f d)
            (case e of
              Matches_Algebraic_2 g h -> Matches_Algebraic_2 (subst_algebraic a c <$> g) (f <$> h)
              Matches_Int_2 g h -> Matches_Int_2 (f <$> g) (f h)
              Matches_Modular_2 g h -> Matches_Modular_2 (f <$> g) (f <$> h))
        Name_expression_2 d -> if d == a then c else b
        Struct_expression_2 s5 d -> Struct_expression_2 s5 (f <$> d)
        _ -> b
  subst_help :: String -> [Pat_1] -> Bool
  subst_help a b = or (subst_pat a <$> b)
  subst_pat :: String -> Pat_1 -> Bool
  subst_pat a b =
    case b of
      Application_pat_1 c -> subst_help a (snd <$> c)
      Blank_pat_1 -> False
      Name_pat_1 c -> c == a
  subst_pat' :: Pat_1 -> Expression_2 -> Expression_2 -> Expression_2
  subst_pat' a b c =
    case a of
      Application_pat_1 d ->
        case b of
          Struct_expression_2 _ e -> subst_pats d e c
          _ -> undefined
      Blank_pat_1 -> c
      Name_pat_1 d -> subst_expr d c b
  subst_pats :: [(String, Pat_1)] -> Map' Expression_2 -> Expression_2 -> Expression_2
  subst_pats a b c =
    case a of
      [] -> c
      (d, e) : f -> subst_pats f b (subst_pat' e (unsafe_lookup d b) c)
  tokenise_parse_naming_typing_eval ::
    (Set String, Locations) ->
    Map' Polykind ->
    (Map' Alg, Map' String, Map' Type_2) ->
    Map' Expression_2 ->
    String ->
    Map' (Map' [[String]]) ->
    Map' ([String], Map' [(String, Nat)]) ->
    Map' Kind ->
    Map' Strct ->
    Map' Op ->
    Err String
  tokenise_parse_naming_typing_eval c f (g, h, i) l b u v w a q =
    (
      parse_expression b >>=
      \e ->
        (
          std_expr (Location_1 "input") q e >>=
          \e' -> naming_expression "input" e' c >>= \j -> eval l <$> type_expr' (f, g, h, i, w) j u v a))
  tostr :: Expression_2 -> String
  tostr expr =
    case expr of
      Algebraic_expression_2 name exprs -> intercalate " " (name : (tostr_brackets <$> exprs))
      Int_expression_2 i -> show i
      Modular_expression_2 i -> show i
      Struct_expression_2 name fields -> name <> " {" <> intercalate ", " (tostr_brackets' <$> assocs fields) <> "}"
      _ -> "The result is an expression that can't be displayed."
  tostr_brackets :: Expression_2 -> String
  tostr_brackets expr = "(" <> tostr expr <> ")"
  tostr_brackets' :: (String, Expression_2) -> String
  tostr_brackets' (name, expr) = name <> " = " <> tostr expr
  wrap_algebraic :: Expression_2 -> Expression_2
  wrap_algebraic x = Algebraic_expression_2 "Wrap" [x]