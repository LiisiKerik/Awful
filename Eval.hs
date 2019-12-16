--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Eval (eval') where
  import Data.Map
  import Dictionary
  import Errors
  import Typing
  eval' :: Dictionary Term_3 -> Term_3 -> Err String
  eval' terms term =
    case eval_term terms term of
      Nothing -> Left Evaluation_crashed
      Just term' -> write_term term'
  eval_arrow :: Arrow_3 -> Term_3 -> Maybe Term_3
  eval_arrow (Arrow_3 term_pattern term_0) term_1 =
    (\terms -> replace_term terms term_0) <$> eval_term_pattern term_pattern term_1
  eval_arrows :: Term_3 -> [Arrow_3] -> Maybe Term_3
  eval_arrows term arrows =
    case arrows of
      [] -> Nothing
      arrow : arrows' ->
        case eval_arrow arrow term of
          Nothing -> eval_arrows term arrows'
          Just term' -> Just term'
  eval_term :: Dictionary Term_3 -> Term_3 -> Maybe Term_3
  eval_term terms term =
    case term of
      Add_Int_term_3 (Int_term_3 int_0) (Int_term_3 int_1) -> Just (Int_term_3 (int_0 + int_1))
      Application_term_3 term_0 term_1 ->
        do
          Arrow_term_3 arrow <- eval_term terms term_0
          term' <- eval_term terms term_1
          term'' <- eval_arrow arrow term'
          eval_term terms term''
      Compare_Int_term_3 (Int_term_3 int_0) (Int_term_3 int_1) ->
        Just
          (case compare int_0 int_1 of
            LT -> LT_term_3
            EQ -> EQ_term_3
            GT -> GT_term_3)
      Div_term_3 (Int_term_3 int_0) (Int_term_3 int_1) -> Just (Int_term_3 (div int_0 int_1))
      Match_term_3 term' arrows ->
        do
          term'' <- eval_term terms term'
          term''' <- eval_arrows term'' arrows
          eval_term terms term'''
      Mod_term_3 (Int_term_3 int_0) (Int_term_3 int_1) -> Just (Int_term_3 (mod int_0 int_1))
      Name_term_3 name -> Data.Map.lookup name terms
      Times_Int_term_3 (Int_term_3 int_0) (Int_term_3 int_1) -> Just (Int_term_3 (int_0 * int_1))
      _ -> Just term
  eval_term_pattern :: Term_pattern_3 -> Term_3 -> Maybe (Dictionary Term_3)
  eval_term_pattern term_pattern term =
    case (term_pattern, term) of
      (Int_term_pattern_3 int_0, Int_term_3 int_1) ->
        case int_0 == int_1 of
          False -> Nothing
          True -> Just empty
      (Name_term_pattern_3 name, _) -> Just (singleton name term)
      (Struct_term_pattern_3 "EQ", EQ_term_3) -> Just empty
      (Struct_term_pattern_3 "GT", GT_term_3) -> Just empty
      (Struct_term_pattern_3 "LT", LT_term_3) -> Just empty
      _ -> Nothing
  gather_variables :: Term_pattern_3 -> [String]
  gather_variables term_pattern =
    case term_pattern of
      Int_term_pattern_3 _ -> []
      Name_term_pattern_3 name -> [name]
      Struct_term_pattern_3 _ -> []
  replace_arrow :: Dictionary Term_3 -> Arrow_3 -> Arrow_3
  replace_arrow terms (Arrow_3 term_pattern term) =
    Arrow_3 term_pattern (replace_term (delete_keys terms (gather_variables term_pattern)) term)
  replace_term :: Dictionary Term_3 -> Term_3 -> Term_3
  replace_term terms term =
    case term of
      Add_Int_term_3 term_0 term_1 -> Add_Int_term_3 (replace_term terms term_0) (replace_term terms term_1)
      Application_term_3 term_0 term_1 -> Application_term_3 (replace_term terms term_0) (replace_term terms term_1)
      Arrow_term_3 arrow -> Arrow_term_3 (replace_arrow terms arrow)
      Compare_Int_term_3 term_0 term_1 -> Compare_Int_term_3 (replace_term terms term_0) (replace_term terms term_1)
      Div_term_3 term_0 term_1 -> Div_term_3 (replace_term terms term_0) (replace_term terms term_1)
      EQ_term_3 -> EQ_term_3
      GT_term_3 -> GT_term_3
      Int_term_3 int -> Int_term_3 int
      LT_term_3 -> LT_term_3
      Match_term_3 term' arrows -> Match_term_3 (replace_term terms term') (replace_arrow terms <$> arrows)
      Mod_term_3 term_0 term_1 -> Mod_term_3 (replace_term terms term_0) (replace_term terms term_1)
      Name_term_3 name ->
        case Data.Map.lookup name terms of
          Nothing -> term
          Just term' -> term'
      Times_Int_term_3 term_0 term_1 -> Times_Int_term_3 (replace_term terms term_0) (replace_term terms term_1)
  write_term :: Term_3 -> Err String
  write_term term =
    case term of
      EQ_term_3 -> Right "EQ"
      GT_term_3 -> Right "GT"
      Int_term_3 int -> Right (show int)
      LT_term_3 -> Right "LT"
      _ -> Left The_result_is_not_writeable
--------------------------------------------------------------------------------------------------------------------------------